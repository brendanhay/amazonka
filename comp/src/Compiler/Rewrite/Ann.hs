{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.Rewrite.Ann
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.Ann where
    -- ( annotateTypes
    -- ) where

import           Compiler.Formatting          hiding (base)
import           Compiler.Rewrite.Ann.Syntax
import           Compiler.Rewrite.Ann.TypeOf
import           Compiler.Rewrite.Prefix
import           Compiler.Types
import           Control.Arrow                ((&&&))
import           Control.Error
import           Control.Lens                 hiding (enum, mapping, (??))
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor
import qualified Data.Foldable                as Fold
import qualified Data.HashMap.Strict          as Map
import qualified Data.HashSet                 as Set
import           Data.Monoid                  hiding (Product, Sum)
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Builder       as Build
import           Data.Text.Manipulate
import           HIndent
import           Language.Haskell.Exts.Build  (app, lamE, paren, sfun)
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SrcLoc (noLoc)
import           Language.Haskell.Exts.Syntax hiding (Int, List, Lit)

-- FIXME: Should empty responses be a shared type, and
-- always succeed based on HTTP response code?

annotateTypes :: Config
              -> Service Identity Shape Shape
              -> Either Error (Service Identity Data Data)
annotateTypes cfg svc@Service{..} = do
    ps <- prefixes universe'

    let !ts = solve cfg universe'
        !is = instances (svc ^. protocol)

    cs <- constraints cfg universe'
    ss <- kvTraverseMaybe (datatype ps ts cs is) _shapes

    return $! svc
        { _operations = mempty
        , _shapes     = ss
        }
  where
    universe' = _shapes <> foldMap f _operations

    f x = Map.fromList
        [ (x ^. requestName,  x ^. opInput  . _Identity)
        , (x ^. responseName, x ^. opOutput . _Identity)
        ]

instances :: Protocol -> [Instance]
instances = \case
    JSON     -> [FromJSON, ToJSON]
    RestJSON -> [FromJSON, ToJSON]
    XML      -> [FromXML,  ToXML]
    RestXML  -> [FromXML,  ToXML]
    Query    -> [FromXML,  ToQuery]
    EC2      -> [FromXML,  ToQuery]

data Field = Field
    { fieldParam    :: Name
    , fieldType     :: Type
    , fieldAccessor :: ([Name], Type)
    , fieldLens     :: Fun
    , fieldUpdate   :: FieldUpdate
    , fieldReq      :: !Bool
    }

solve :: Config -> Map Id (Shape Identity) -> Map Id TType
solve cfg ss = execState (Map.traverseWithKey go ss) initial
  where
    initial :: Map Id TType
    initial = replaced (TType . _replaceName) cfg

    go :: Id -> Shape Identity -> State (Map Id TType) TType
    go n = save n <=< \case
        Struct {}  -> pure (TType n)
        Enum   {}  -> pure (TType n)

        List i e -> fmap (sensitive i . flatten i) $
            c (e ^. refLocationName . _Identity) <$> memo (e ^. refShape)
          where
            c | i ^. infoMin > Just 0 = TList1
              | otherwise             = TList

        Map  i k v -> fmap (sensitive i . flatten i) $
            TMap ( "value"
                 , k ^. refLocationName . _Identity
                 , v ^. refLocationName . _Identity
                 ) <$> memo (k ^. refShape)
                   <*> memo (v ^. refShape)

        Lit i l -> pure . sensitive i $
            case l of
                Int    -> natural i (TLit l)
                Long   -> natural i (TLit l)
                _      -> TLit l

    memo :: Id -> State (Map Id TType) TType
    memo k = do
        m <- gets (Map.lookup k)
        case m of
            Just x  -> return x
            Nothing ->
                case Map.lookup k ss of
                    Just x  -> go k x
                    Nothing -> return (TType k)

    save :: Monad m => Id -> a -> StateT (Map Id a) m a
    save k v = modify (Map.insert k v) >> return v

type CS = StateT (Map Id (Set Constraint)) (Either Error)

constraints :: Config
            -> Map Id (Shape Identity)
            -> Either Error (Map Id (Set Constraint))
constraints cfg ss = evalStateT (Map.traverseWithKey go ss) initial
  where
    initial :: Map Id (Set Constraint)
    initial = replaced _replaceConstraints cfg

    go :: Id -> Shape Identity -> CS (Set Constraint)
    go n s = do
        m <- gets (Map.lookup n)
        case m of
            Just cs -> return cs
            Nothing -> derive n s

    -- FIXME: Filter constraints based on things like min/max of lists etc.
    derive n = save n <=< \case
        Struct _ s -> cplx n s
        List   {}  -> pure (base <> mono)
        Map    {}  -> pure (base <> mono)
        Enum   {}  -> pure (base <> enum)
        Lit    _ l -> pure $
            case l of
                Int    -> base <> num
                Long   -> base <> num
                Double -> base <> frac
                Text   -> base <> str
                Blob   -> [CShow]
                Time _ -> base <> enum
                Bool   -> base <> enum

    cplx :: Id -> Struct Identity -> CS (Set Constraint)
    cplx n s = combine <$> traverse ref (s ^.. references . refShape)
      where
        combine :: [Set Constraint] -> Set Constraint
        combine [x]    = x
        combine (x:xs) = Set.intersection (Fold.foldl' Set.intersection x xs) base
        combine _      = base

        ref :: Id -> CS (Set Constraint)
        ref k = cache >>= maybe miss return
          where
            cache = gets (Map.lookup k)
            miss  = lift (note e (Map.lookup k ss)) >>= go k

            e = format ("Missing shape "                     % iprimary %
                        " when determining constraints for " % iprimary %
                        " :: "                               % shown %
                        ", in possible matches "             % partial)
                       k n (s ^.. references) (k, ss)

    str, num, frac, mono, enum, base :: Set Constraint
    str  = [CIsString]
    num  = [CEnum, CNum, CIntegral, CReal]
    frac = [CRealFrac, CRealFloat]
    mono = [CMonoid, CSemigroup]
    enum = [CEnum]
    base = [CEq, COrd, CRead, CShow]

    save :: (Monad m, Monoid a) => Id -> a -> StateT (Map Id a) m a
    save k x = modify (Map.insertWith (<>) k x) >> return x

datatype :: Map Id Text
         -> Map Id TType
         -> Map Id (Set Constraint)
         -> [Instance]
         -> Id
         -> Shape Identity
         -> Either Error (Maybe (Data Identity))
datatype ps ts cs is n = \case
    Enum   i vs -> satisfy (sum' i vs)
    Struct i s  -> satisfy (prod i s)
    _           -> return Nothing
  where
    satisfy f = Just <$> ((,) <$> prefix <*> fulfill n >>= uncurry f)

    prefix :: Either Error Text
    prefix = flip note (Map.lookup n ps) $
        format ("Missing prefix for shape " % iprimary %
                ", possible matches "       % partial)
               n (n, ps)

    fulfill :: Id -> Either Error (Set Constraint)
    fulfill k = flip note (Map.lookup k cs) $
        format ("Missing constraints for shape " % iprimary %
                ", possible matches for "        % iprimary %
                ": "                             % partial)
               n k (k, cs)

    typeOf :: Id -> Either Error TType
    typeOf k = flip note (Map.lookup k ts) $
        format ("Missing type for shape " % iprimary %
                ", possible matches for " % iprimary %
                ": "                      % partial)
               n k (k, ts)

    sum' :: Info
         -> Map Text Text
         -> Text
         -> Set Constraint
         -> Either Error (Data Identity)
    sum' i vs p ds = Sum i bs <$> decl <*> pure is
      where
        bs :: Map Text Text
        bs = vs & kvTraversal %~ first (f . upperHead)
          where
            f | Text.null p = id
              | otherwise   = mappend (upperHead p)

        decl :: Either Error LazyText
        decl = pretty True$
            dataDecl (n ^. typeId . to ident) (map branch $ Map.keys bs) (derivings ds)

        branch :: Text -> QualConDecl
        branch k = QualConDecl noLoc [] [] (ConDecl (ident k) [])

    prod :: Info
         -> Struct Identity
         -> Text
         -> Set Constraint
         -> Either Error (Data Identity)
    prod i s@Struct'{..} p ds = do
        fs <- traverse (uncurry field) (zip [1..] $ Map.toList _members)
        Product i s <$> decl fs <*> pure is <*> ctor fs <*> lenses fs
      where
        decl fs = pretty True $ dataDecl (n ^. typeId . to ident)
            [ QualConDecl noLoc [] [] (RecDecl (n ^. typeId . to ident) (map fieldAccessor fs))
            ] (derivings ds)

        ctor fs = Fun (n ^. ctorId) h <$> pretty False sig <*> pretty True body
          where
            -- FIXME: this should output haddock single quotes to ensure
            -- the type is linked properly.
            h = fromString $ Text.unpack ("'" <> n ^. typeId <> "' smart constructor.")

            sig  = typeSig c (n ^. typeId . to tycon) ty
            body = sfun noLoc c [] (UnGuardedRhs (RecConstr (n ^. typeId . to unqual) us)) (BDecls [])

            c = n ^. ctorId . to ident

            ty = map fieldType rs
            ps = map fieldParam rs  -- Constructor parameters
            rs = filter fieldReq fs -- Required fields
            us = map fieldUpdate fs

        lenses = pure . map fieldLens

        -- FIXME: Facets of Info for the field need to be layered on top
        -- of the type, such as nonempty, maybe, etc.
        field :: Int -> (Id, Ref Identity) -> Either Error Field
        field i (k, v) = do
            c <- fulfill r
            t <- optional req <$> typeOf r

            let o = Ident ("p" ++ show i)
                f = fromMaybe (Var (UnQual o)) (def c)

            ls <- pretty False $ lenss t
            lf <- pretty False $ lensf t

            return $! Field
                { fieldParam    = o
                , fieldType     = external t
                , fieldAccessor = ([ident a], internal t)
                , fieldLens     = Fun (k ^. lensId p) (v ^. refDocumentation . _Identity) ls lf
                , fieldUpdate   = FieldUpdate (unqual a) f
                , fieldReq      = req
                }
          where
            r = v ^. refShape
            a = k ^. fieldId p

            req :: Bool
            req = Set.member k _required

            def :: Set Constraint -> Maybe Exp
            def c
                | not req              = Just (var "Nothing")
                | Set.member CMonoid c = Just (var "mempty")
                | otherwise            = Nothing

            lensf t = sfun noLoc (k ^. lensId p . to ident) [] (UnGuardedRhs sett) (BDecls [])
              where
                sett = mapping t $
                    app (app (var "lens") (var a))
                        (paren (lamE noLoc [pvar "s", pvar "a"]
                            (RecUpdate (var "s") [FieldUpdate (unqual a) (var "a")])))

           -- lensf
            lenss t = typeSig (k ^. lensId p . to ident) s []
              where
                s = TyApp (TyApp (tycon "Lens'") (n ^. typeId . to tycon)) (external t)

pretty :: Bool -> Decl -> Either Error LazyText
pretty fmt d
    | fmt       = bimap e Build.toLazyText $ reformat johanTibell Nothing p
    | otherwise = return p
  where
    e = flip mappend (", when formatting datatype: " <> p) . LText.pack

    p = LText.pack (prettyPrintStyleMode s m d)

    s = style
        { mode           = PageMode
        , lineLength     = 80
        , ribbonsPerLine = 1.5
        }

    m = defaultMode
        { spacing = False
        , layout  = PPNoLayout
        }

replaced :: (Replace -> a) -> Config -> Map Id a
replaced f =
      Map.fromList
    . map (_replaceName &&& f)
    . Map.elems
    . vMapMaybe _replacedBy
    . _typeOverrides
