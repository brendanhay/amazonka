{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.Rewrite.TypeOf
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.TypeOf
    ( annotateTypes
    ) where

import           Compiler.Formatting          hiding (base)
import           Compiler.Rewrite.Prefix
import           Compiler.Types
import           Control.Arrow                ((&&&))
import           Control.Error
import           Control.Lens                 hiding (enum, (??))
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Foldable                (foldl')
import qualified Data.HashMap.Strict          as Map
import qualified Data.HashSet                 as Set
import           Data.List                    (sort)
import           Data.Monoid                  hiding (Product, Sum)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Builder       as Build
import           Data.Text.Manipulate
import           HIndent
import           Language.Haskell.Exts        hiding (Int, List, Lit)
import           Language.Haskell.Exts.SrcLoc (noLoc)

-- FIXME: Should empty responses be a shared type, and
-- always succeed based on HTTP response code?

annotateTypes :: Monad m
              => Config
              -> Service Identity Shape Shape
              -> Compiler m (Service Identity Data Data)
annotateTypes cfg svc@Service{..} = do
    ps <- prefixes universe'

    let !ts = solve cfg (svc ^. timestampFormat . _Identity) universe'

    cs <- constraints cfg universe'
    ss <- kvTraverseMaybe (datatype ps ts cs) _shapes

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

solve :: Config -> Timestamp -> Map Id (Shape Identity) -> Map Id Type
solve cfg t ss = execState (Map.traverseWithKey go ss) initial
  where
    initial :: Map Id Type
    initial = replaced (itycon . _replaceName) cfg

    go :: Id -> Shape Identity -> State (Map Id Type) Type
    go n = save n <=< \case
        Struct {}  -> pure (itycon n)
        Enum   {}  -> pure (itycon n)

        List _ e ->
            TyApp (tycon "List") <$> memo (e ^. refShape)

        Map _ k v ->
            TyApp (tycon "Map") <$>
                (TyApp <$> memo (k ^. refShape)
                       <*> memo (v ^. refShape))

        Lit i l -> pure . sensitive i $
            case l of
                Int    -> natural i (tycon "Int")
                Long   -> natural i (tycon "Long")
                Double -> tycon "Double"
                Text   -> tycon "Text"
                Blob   -> tycon "Blob"
                Time   -> time
                Bool   -> tycon "Bool"

    time :: Type
    time = TyCon . UnQual . Ident $ show t

    natural :: HasInfo a f => a -> (Type -> Type)
    natural x
        | Just i <- x ^. infoMin
        , i >= 0    = const (tycon "Natural")
        | otherwise = id

    sensitive :: HasInfo a f => a -> (Type -> Type)
    sensitive x
        | x ^. infoSensitive = TyApp (tycon "Sensitive")
        | otherwise          = id

    memo :: Id -> State (Map Id Type) Type
    memo k = do
        m <- gets (Map.lookup k)
        case m of
            Just x  -> return x
            Nothing ->
                case Map.lookup k ss of
                    Just x  -> go k x
                    Nothing -> return (itycon k)

    save :: Monad m => Id -> a -> StateT (Map Id a) m a
    save k v = modify (Map.insert k v) >> return v

constraints :: Monad m
            => Config
            -> Map Id (Shape Identity)
            -> Compiler m (Map Id (Set Constraint))
constraints cfg ss = evalStateT (Map.traverseWithKey go ss) initial
  where
    initial :: Map Id (Set Constraint)
    initial = replaced _replaceConstraints cfg

    go :: Monad m
       => Id
       -> Shape Identity
       -> StateT (Map Id (Set Constraint)) (Compiler m) (Set Constraint)
    go n s = do
        m <- gets (Map.lookup n)
        case m of
            Just cs -> return cs
            Nothing -> derive n s

    -- FIXME: Filter constraints based on things like min/max of lists etc.
    derive n = save n <=< \case
        Struct _ s -> cplx n s
        List   {}  -> pure (base <> list)
        Map    {}  -> pure (base <> list)
        Enum   {}  -> pure (base <> enum)
        Lit    _ l -> pure $
            case l of
                Int    -> base <> num
                Long   -> base <> num
                Double -> base <> frac
                Text   -> base <> str
                Blob   -> [CShow]
                Time   -> base <> enum
                Bool   -> base <> enum

    cplx :: Monad m
         => Id
         -> Struct Identity
         -> StateT (Map Id (Set Constraint)) (Compiler m) (Set Constraint)
    cplx n s = combine <$> traverse ref (s ^.. references . refShape)
      where
        combine :: [Set Constraint] -> Set Constraint
        combine [x]    = x
        combine (x:xs) = Set.intersection (foldl' Set.intersection x xs) base
        combine _      = base

        ref :: Monad m
            => Id
            -> StateT (Map Id (Set Constraint)) (Compiler m) (Set Constraint)
        ref k = cache >>= maybe miss return
          where
            cache = gets (Map.lookup k)
            miss  = lift (Map.lookup k ss ?? e) >>= go k

            e = format ("Missing shape "                     % fid %
                        " when determining constraints for " % fid %
                        " :: "                               % shown %
                        ", in possible matches "             % partial)
                       k n (s ^.. references) (k, ss)

    str, num, frac, list, enum, base :: Set Constraint
    str  = [CIsString]
    num  = [CEnum, CNum, CIntegral, CReal]
    frac = [CRealFrac, CRealFloat]
    list = [CMonoid, CSemigroup]
    enum = [CEnum]
    base = [CEq, COrd, CRead, CShow]

    save :: (Monad m, Monoid a) => Id -> a -> StateT (Map Id a) m a
    save k x = modify (Map.insertWith (<>) k x) >> return x

datatype :: Monad m
         => Map Id Text
         -> Map Id Type
         -> Map Id (Set Constraint)
         -> Id
         -> Shape Identity
         -> Compiler m (Maybe (Data Identity))
datatype ps ts cs n = \case
    Enum   i vs -> satisfy (sum' i vs)
    Struct i s  -> satisfy (prod i s)
    _           -> return Nothing
  where
    satisfy f = Just <$> ((,) <$> prefix <*> fulfill >>= uncurry f)

    prefix :: Monad m => Compiler m Text
    prefix = Map.lookup n ps ??
        format ("Missing prefix for shape " % fid %
                ", possible matches "       % partial)
               n (n, ps)

    fulfill :: Monad m => Compiler m [Deriving]
    fulfill = fmap derivings $ Map.lookup n cs ??
        format ("Missing constraints for shape " % fid %
                ", possible matches "            % partial)
               n (n, cs)

    sum' :: Monad m
         => Info Identity
         -> Map Text Text
         -> Text
         -> [Deriving]
         -> Compiler m (Data Identity)
    sum' i vs p ds = Sum i bs <$> decl <*> pure []
      where
        bs :: Map Text Text
        bs = vs & kvTraversal %~ first (f . upperHead)
          where
            f | Text.null p = id
              | otherwise   = mappend (upperHead p)

        decl :: Monad m => Compiler m LazyText
        decl = hoistEither . pretty $
            dataDecl (iident n) (map branch $ Map.keys bs) ds

        branch :: Text -> QualConDecl
        branch k = QualConDecl noLoc [] [] (ConDecl (ident k) [])

    prod :: Monad m
         => Info Identity
         -> Struct Identity
         -> Text
         -> [Deriving]
         -> Compiler m (Data Identity)
    prod i s@Struct'{..} p ds =
        Product i s <$> decl <*> pure [] <*> ctor <*> pure lenses
     where
        decl :: Monad m => Compiler m LazyText
        decl = do
           fs <- traverse field (Map.toList _members)
           hoistEither . pretty $ dataDecl (iident n)
               [ QualConDecl noLoc [] [] (RecDecl (iident n) fs)
               ] ds

        -- FIXME: Facets of Info for the field need to be layered on top
        -- of the type, such as nonempty, maybe, etc.
        field :: Monad m => (Id, Ref Identity) -> Compiler m ([Name], Type)
        field (k, v) = ([accessor (Text.toLower p) k],)
            <$> (Map.lookup t ts ?? e)
          where
            t = v ^. refShape
            e = format ("Missing type "       % fid %
                        " of field "          % fid %
                        " in shape "          % fid %
                        ", possible matches " % partial)
                       t k n (t, ts)

        ctor = pure undefined -- Fun

        lenses = mempty

pretty :: Decl -> Either LazyText LazyText
pretty d = bimap e Build.toLazyText $ reformat johanTibell Nothing p
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

accessor :: Text -> Id -> Name
accessor p k = ident ("_" <> p <> upperHead (k ^. keyActual))

itycon :: Id -> Type
itycon = TyCon . UnQual . iident

tycon :: Text -> Type
tycon = TyCon . unqual

unqual :: Text -> QName
unqual = UnQual . ident

iident :: Id -> Name
iident = ident . view keyActual

ident :: Text -> Name
ident = Ident . Text.unpack

dataDecl :: Name -> [QualConDecl] -> [Deriving] -> Decl
dataDecl n cs = DataDecl noLoc arity [] n [] cs
  where
    arity = case cs of
        [QualConDecl _ _ _ (RecDecl _ [_])] -> NewType
        _                                   -> DataType

derivings :: Set Constraint -> [Deriving]
derivings = map ((,[]) . UnQual . Ident . drop 1 . show) . sort . Set.toList
