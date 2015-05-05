{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
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

import           Compiler.Formatting
import           Compiler.Rewrite.Prefix
import           Compiler.Types
import           Control.Arrow                ((&&&))
import           Control.Error
import           Control.Lens                 hiding ((??))
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Foldable                (foldl')
import qualified Data.HashMap.Strict          as Map
import qualified Data.HashSet                 as Set
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

    let !ts = solve cfg universe'

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

solve :: Config -> Map Id (Shape Identity) -> Map Id Type
solve cfg ss = execState (Map.traverseWithKey go ss) initial
  where
    initial :: Map Id Type
    initial = replaced (itycon . _replaceName) cfg

    go :: Id -> Shape Identity -> State (Map Id Type) Type
    go n = \case
        Struct {}  -> save n (itycon n)
        Enum   {}  -> save n (itycon n)

        List _ e -> do
            t <- TyApp (tycon "List") <$> memo (e ^. refShape)
            save n t

        Map _ k v -> do
            t <- TyApp <$> memo (k ^. refShape) <*> memo (v ^. refShape)
            save n (TyApp (tycon "Map") t)

        Lit _ l -> case l of
            Int    -> save n (tycon "Int")
            Long   -> save n (tycon "Long")
            Double -> save n (tycon "Double")
            Text   -> save n (tycon "Text")
            Blob   -> save n (tycon "Blob")
            Time   -> save n (tycon "Time")
            Bool   -> save n (tycon "Bool")

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
    save n t = modify (Map.insert n t) >> return t

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

    derive n = \case
        List   {}  -> save n (def <> list)
        Map    {}  -> save n (def <> list)
        Struct _ s -> save n . Set.insert CGeneric =<< cplx n s
        s          -> save n (smpl s)

    cplx :: Monad m
         => Id
         -> Struct Identity
         -> StateT (Map Id (Set Constraint)) (Compiler m) (Set Constraint)
    cplx n s = sect <$> traverse ref (s ^.. references)
      where
        sect (x:xs) = Set.intersection str (foldl' Set.intersection x xs)
        sect _      = mempty

        ref r = do
            m <- gets (Map.lookup k)
            maybe (lift (Map.lookup k ss ?? e) >>= go k) return m
          where
            k = r ^. refShape
            e = format ("Missing shape "                     % fid %
                        " when determining constraints for " % fid %
                        " :: "                               % shown %
                        ", in possible matches "             % partial)
                       k n (s ^.. references) (k, ss)

    smpl = \case
        -- SString _ -> [CEq, COrd, CRead, CShow, CGeneric, CIsString]
        -- SEnum   _ -> [CEq, COrd, CEnum, CRead, CShow, CGeneric]
        -- SBlob   _ -> [CGeneric]
        -- SBool   _ -> [CEq, COrd, CEnum, CRead, CShow, CGeneric]
        -- STime   _ -> [CEq, COrd, CRead, CShow, CGeneric]
        -- SInt    _ -> [CEq, COrd, CEnum, CRead, CShow, CNum, CIntegral, CReal]
        -- SDouble _ -> [CEq, COrd, CEnum, CRead, CShow, CNum, CReal, CRealFrac, CRealFloat]
        -- SLong   _ -> [CEq, COrd, CEnum, CRead, CShow, CNum, CIntegral, CReal]
        _         -> mempty

    str  = def <> Set.fromList [COrd, CIsString]
    list = Set.fromList [CMonoid, CSemigroup]
    def  = Set.fromList [CEq, CRead, CShow, CGeneric]

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
    Struct i s -> Just <$> (prod i s =<< prefix)
    Enum   {}  -> return Nothing
    _          -> return Nothing
  where
    prefix = Map.lookup n ps ??
        format ("Missing prefix for shape " % fid %
                ", possible matches "       % partial)
               n (n, ps)

    prod i s@Struct'{..} p = Product i s
        <$> decl
        <*> pure []
        <*> ctor
        <*> pure lenses
      where
        decl = do
           fs <- traverse field (Map.toList _members)
           hoistEither . pretty $ DataDecl noLoc arity [] (iident n) []
               [ QualConDecl noLoc [] [] (RecDecl (iident n) fs)
               ] []

        -- FIXME: Facets of Info for the field need to be layered on top
        -- of the type, such as nonempty, maybe, etc.
        field (k, v) = ([accessor (Text.toLower p) k],)
            <$> (Map.lookup t ts ?? e)
          where
            t = v ^. refShape
            e = format ("Missing type "       % fid %
                        " of field "          % fid %
                        " in shape "          % fid %
                        ", possible matches " % partial)
                       t k n (t, ts)

        arity | Map.size _members == 1 = NewType
              | otherwise              = DataType

        ctor = pure undefined -- Fun

        lenses = mempty

pretty :: Decl -> Either LazyText LazyText
pretty d = bimap e Build.toLazyText $ reformat johanTibell Nothing p
  where
    e = flip mappend (" - when formatting datatype: " <> p) . LText.pack

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
