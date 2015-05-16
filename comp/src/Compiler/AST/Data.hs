{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

-- Module      : Compiler.AST.Data
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.Data
    ( dataType
    ) where

import           Compiler.AST.Data.Field
import           Compiler.AST.Data.Syntax
import           Compiler.Protocol
import           Compiler.Types
import           Control.Comonad.Cofree
import           Control.Lens                 hiding (enum, mapping, (??))
import           Data.Bifunctor
import           Data.Char                    (isSpace)
import qualified Data.HashMap.Strict          as Map
import           Data.Monoid                  hiding (Product, Sum)
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Builder       as Build
import           HIndent
import           Language.Haskell.Exts.Pretty

-- FIXME: Should empty responses be a shared type, and
-- always succeed based on HTTP response code?

dataType :: Protocol
         -> Shape (Id ::: Maybe Text ::: Relation ::: Solved)
         -> Either Error (Maybe Data)
dataType proto ((n ::: p ::: r ::: t ::: ds ::: is) :< s) =
    case s of
        Enum   i vs -> Just <$> enum i vs
        Struct st   -> Just <$> struct st
        _           -> return Nothing
  where
    enum :: Info -> Map Id Text -> Either Error Data
    enum i vs = Sum i
        <$> format (dataDecl n (map conDecl (Map.keys branches)) ds)
        <*> pure branches
        <*> pure is
      where
        branches :: Map Text Text
        branches = vs & kvTraversal %~ first (^. ctorId p)

    struct :: StructF (Shape (Id ::: Maybe Text ::: Relation ::: Solved))
           -> Either Error Data
    struct st = Product (st ^. info)
        <$> format (dataDecl n [recDecl n fields] ds)
        <*> mkCtor
        <*> traverse mkLens fields
        <*> (Map.fromList <$> traverse mkInstance (instances proto r))
      where
        mkCtor :: Either Error Fun
        mkCtor = Fun (n ^. smartCtorId) h
            <$> plain  (ctorSig  n fields)
            <*> format (ctorDecl n fields)
          where
            -- FIXME: this should output haddock single quotes to ensure
            -- the type is linked properly, but the following outputs
            -- '@' delimiters, need to investigate pandoc.
            h = fromString . Text.unpack $
                "'" <> n ^. typeId <> "' smart constructor."

        mkLens :: Field -> Either Error Fun
        mkLens f = Fun (f ^. fieldLens) (f ^. fieldHelp)
            <$> plain (lensSig t f)
            <*> plain (lensDecl  f)

        -- | Creating an application of locationName <de/serialiser> accessor
        -- per field that satisfies the instance location requirement.
        mkInstance :: Instance -> Either Error (Instance, [LazyText])
        mkInstance k = (k,) <$> go k (satisfies k (^. fieldLocation) fields)
          where
            go :: Instance -> [Field] -> Either Error [LazyText]
            go x = traverse (plain . instanceExp proto x)

        fields :: [Field]
        fields = mkFields st

format, plain :: Pretty a => a -> Either Error LazyText
format = pretty True
plain  = pretty False

pretty :: Pretty a => Bool -> a -> Either Error LazyText
pretty fmt d
    | fmt       = bimap e Build.toLazyText (reformat johanTibell Nothing p)
    | otherwise = pure p
  where
    e = flip mappend (", when formatting datatype: " <> p) . LText.pack

    p = LText.dropWhile isSpace . LText.pack $
        prettyPrintStyleMode s defaultMode d

    s = style
        { mode           = PageMode
        , lineLength     = 80
        , ribbonsPerLine = 1.5
        }
