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
import           HIndent
import           Language.Haskell.Exts        hiding (Int, List, Lit)
import           Language.Haskell.Exts.SrcLoc (noLoc)

annotateTypes :: Monad m
              => Service Identity Shape Shape
              -> Compiler m (Service Identity Data Data)
annotateTypes svc@Service{..} = do
    ps <- prefixes universe'
    ts <- solve universe'
    cs <- constraints universe'

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

solve :: Monad m => Map Text (Shape Identity) -> Compiler m (Map Text Type)
solve ss = evalStateT (Map.traverseWithKey go ss) mempty
  where
    go :: Monad m
       => Text
       -> Shape Identity
       -> StateT (Map Text Type) (Compiler m) Type
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

    memo :: Monad m
         => Text
         -> StateT (Map Text Type) (Compiler m) Type
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

    save :: Monad m => Text -> a -> StateT (Map Text a) m a
    save n t = modify (Map.insert n t) >> return t

constraints :: Monad m
            => Map Text (Shape Identity)
            -> Compiler m (Map Text (Set Constraint))
constraints ss = evalStateT (Map.traverseWithKey go ss) mempty
  where
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
        Struct _ s -> save n . Set.insert CGeneric =<< complex n s
        s          -> save n (simple s)

    complex :: Monad m
            => Text
            -> Struct Identity
            -> StateT (Map Text (Set Constraint)) (Compiler m) (Set Constraint)
    complex n s = fmap sect . traverse ref . toListOf references $ s
      where
        sect (x:xs) = Set.intersection str (foldl' Set.intersection x xs)
        sect _      = mempty

        ref r = lift (Map.lookup k ss ?? e) >>= go k
          where
            k = r ^. refShape
            e = format ("Missing Shape " % stext %
                        " when determining constraints for: " % stext % "\n" % string)
                       k n (show (s^..references))

    simple = \case
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
            e = format ("Missing type " % stext %
                        " of field "    % stext %
                        " in record "   % stext)
                       t k n

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

tycon :: Text -> Type
tycon = TyCon . unqual

unqual :: Text -> QName
unqual = UnQual . ident

ident :: Text -> Name
ident = Ident . Text.unpack
