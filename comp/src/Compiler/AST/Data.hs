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
import           Compiler.AST.TypeOf
import           Compiler.Types
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Error
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

-- type Field = Id ::: TType ::: [Derive]

-- FIXME: Should empty responses be a shared type, and
-- always succeed based on HTTP response code?

dataType :: Protocol
         -> Shape (Id ::: Maybe Text ::: Relation ::: Solved)
         -> Either Error (Maybe Data)
dataType proto ((n ::: p ::: _ ::: t ::: ds ::: is) :< s) =
    case s of
        Enum   i vs -> Just <$> enum   i vs
        Struct i ms -> Just <$> struct i ms
        _           -> return Nothing
  where
    enum :: Info -> Map Id Text -> Either Error Data
    enum i vs = Sum i
        <$> format (dataDecl n (map conDecl (Map.keys bs)) ds)
        <*> pure bs
        <*> pure is
      where
        bs :: Map Text Text
        bs = vs & kvTraversal %~ first (^. ctorId p)

    struct :: Info
           -> StructF (Shape (Id ::: Maybe Text ::: Relation ::: Solved))
           -> Either Error Data
    struct i strf = Product i
        <$> format (dataDecl n [recDecl p n fs] ds)
        <*> ctor
        <*> traverse lens' fs
        <*> pure mempty -- insts fs
      where
        fs :: [Field]
        fs = map field . Map.toList $ strf ^. members

        ctor :: Either Error Fun
        ctor = Fun (n ^. smartCtorId) h
            <$> plain (ctorSig n fs)
            <*> format (ctorDecl p n fs)
          where
            -- FIXME: this should output haddock single quotes to ensure
            -- the type is linked properly, but the following outputs
            -- '@' delimiters, need to investigate pandoc.
            h = fromString $ Text.unpack ("'" <> n ^. typeId <> "' smart constructor.")

        lens' :: Field -> Either Error Fun
        lens' f = Fun (fieldId f ^. lensId p) (fieldHelp f)
            <$> plain (lensSig  p t f)
            <*> plain (lensDecl p f)

        -- FIXME: Facets of Info for the field need to be layered on top
        -- of the type, such as nonempty, maybe, etc.
        -- This should be layered via the Compiler.AST.TypeOf module.
        field :: (Id, RefF (Shape (Id ::: Maybe Text ::: Relation ::: Solved)))
              -> Field
        field (k, v) = Field
            { fieldId      = k
            , fieldType    = memberType k (strf ^. required) rt
            , fieldDerive  = rds
            , fieldHelp    =
                fromMaybe "FIXME: Undocumented reference."
                    (v ^. refDocumentation)
            }
          where
            _ ::: _ ::: _ ::: rt ::: rds ::: _ = extract (v ^. refAnn)

        -- insts :: [Field] -> Either Error (Map Inst [LazyText])
        -- insts fs = Map.fromList <$> traverse (\i -> (i,) <$> fgh i) is
        --   whereS
        --     fgh :: Inst -> Either Error [LazyText]
        --     fgh i = implement i $ satisfying i (_refLocation . fieldRef) fs

        --     implement :: Inst -> [Field] -> Either Error [LazyText]
        --     implement ToQuery xs = traverse function xs
        --       where
        --         function Field{..} = pretty False $ fun fieldRefShape
        --           where
        --             fun (List i e) =
        --                 app (app (var "toQueryList")
        --                          (str (parent <> maybe mempty (mappend ".") element)))
        --                     (var (fieldId ^. accessorId p))
        --               where
        --                 ((parent, element), _) =
        --                     listName proto (fieldId, fieldRef) (i, e)

        --             fun _         =
        --                 infixApp (str $ fst (memberName proto (fieldId, fieldRef)))
        --                          (qop "=?")
        --                          (var (fieldId ^. accessorId p))
        --     implement _ _ = pure []

format, plain :: Pretty a => a -> Either Error LazyText
format = pretty True
plain  = pretty False

pretty :: Pretty a => Bool -> a -> Either Error LazyText
pretty fmt d
    | fmt       = bimap e Build.toLazyText $ reformat johanTibell Nothing p
    | otherwise = return p
  where
    e = flip mappend (", when formatting datatype: " <> p) . LText.pack

    p = LText.dropWhile isSpace $ LText.pack (prettyPrintStyleMode s m d)

    s = style
        { mode           = PageMode
        , lineLength     = 80
        , ribbonsPerLine = 1.5
        }

    m = defaultMode
        -- { layout  = PPNoLayout
        -- }
