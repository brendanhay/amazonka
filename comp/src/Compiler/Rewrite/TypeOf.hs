{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

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
import           Control.Applicative
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
import           Debug.Trace
import           HIndent
import           Language.Haskell.Exts        hiding (Int, List, Lit)
import           Language.Haskell.Exts.SrcLoc (noLoc)

annotateTypes :: Monad m
              => Config
              -> Service Identity Shape Shape
              -> Compiler m (Service Identity Data Data)
annotateTypes cfg svc@Service{..} = do
    let !ts = solve cfg universe'

    ps <- prefixes universe'
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

-- FIXME:
-- It'll be an error if the operation input/output is untranslatable
--
-- Should empty responses be a shared type, and always succeed based
-- on HTTP response code?

solve :: Config -> Map Text (Shape Identity) -> Map Text Type
solve cfg ss = execState (Map.traverseWithKey go ss) initial
  where
    initial :: Map Text Type
    initial = replaced (tycon . _replaceName) cfg

    go :: Text -> Shape Identity -> State (Map Text Type) Type
    go n = \case
        Struct {}  -> save n (tycon n)
        Enum   {}  -> save n (tycon n)

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

    memo :: Text -> State (Map Text Type) Type
    memo k = do
        m <- gets (Map.lookup k)
        case m of
            Just x  -> return x
            Nothing ->
                case Map.lookup k ss of
                    Just x  -> go k x
                    -- FIXME: Is this an error? Renaming via the overrides
                    -- means types won't be solvable via the current environment.
                    Nothing -> return (tycon k)

-- So maybe it should be an error .. but the 'replacedBy' override rather than
-- removing something from the universe should just not have it rendered?

-- How else could constraints be safely calcuated, ie. for Region?
-- Have a bit/Bool on the shape which is set to true by default
-- for whether then shape is rendered or not?

-- Unrelated, is it worth investigating idempotency of requests
-- and how this info can be used to generate tests or examples?

-- Should the 'override' replacedBy specify the constraints?


    save :: Monad m => Text -> a -> StateT (Map Text a) m a
    save n t = modify (Map.insert n t) >> return t

constraints :: Monad m
            => Config
            -> Map Text (Shape Identity)
            -> Compiler m (Map Text (Set Constraint))
constraints cfg ss = evalStateT (Map.traverseWithKey go ss) initial
  where
    initial :: Map Text (Set Constraint)
    initial = replaced _replaceConstraints cfg

    go :: Monad m
       => Text
       -> Shape Identity
       -> StateT (Map Text (Set Constraint)) (Compiler m) (Set Constraint)
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
         => Text
         -> Struct Identity
         -> StateT (Map Text (Set Constraint)) (Compiler m) (Set Constraint)
    cplx n s = sect <$> traverse ref (s ^.. references)
      where
        sect (x:xs) = Set.intersection str (foldl' Set.intersection x xs)
        sect _      = mempty

        ref r = do
            m <- gets (Map.lookup k)
            maybe (lift (Map.lookup k ss ?? e) >>= go k) return m
          where
            k = r ^. refShape
            e = format ("Missing shape " % stext %
                        " when determining constraints for " % stext %
                        " :: " % shown %
                        ", in possible matches " % partial)
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

    save :: (Monad m, Monoid a) => Text -> a -> StateT (Map Text a) m a
    save k x = modify (Map.insertWith (<>) k x) >> return x

datatype :: Monad m
         => Map Text Text
         -> Map Text Type
         -> Map Text (Set Constraint)
         -> Text
         -> Shape Identity
         -> Compiler m (Maybe (Data Identity))
datatype ps ts cs n = \case
    Struct i s -> do
        p <- Map.lookup n ps ?? "poo"
        Just <$> prod i s p
    Enum   {}  -> return Nothing
    _          -> return Nothing
  where
    prod i s@Struct'{..} (Text.toLower -> p) = Product i s
        <$> decl
        <*> pure []
        <*> ctor
        <*> pure lenses
      where
        decl = do
           fs <- traverse field (Map.toList _members)
           hoistEither . pretty $ DataDecl noLoc arity [] (ident n) []
               [ QualConDecl noLoc [] [] (RecDecl (ident n) fs)
               ] []

        -- Facets of Info for the field need to be layered on top
        -- of the type, such as nonempty, maybe, etc.
        field (k, v) = ([ident ("_" <> p <> k)],) <$> (Map.lookup t ts ?? e)
          where
            t = v ^. refShape
            e = format ("Missing type "       % stext %
                        " of field "          % stext %
                        " in record "         % stext %
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
    --
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

--replacements :: Map Text Type
replaced f =
      Map.fromList
    . map (_replaceName &&& f)
    . Map.elems
    . vMapMaybe _replacedBy
    . _typeOverrides

tycon :: Text -> Type
tycon = TyCon . unqual

unqual :: Text -> QName
unqual = UnQual . ident

ident :: Text -> Name
ident = Ident . Text.unpack
