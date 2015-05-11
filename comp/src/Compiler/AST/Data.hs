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

import           Compiler.AST.Data.Syntax
import           Compiler.AST.Prefix
import           Compiler.Formatting          hiding (base)
import           Compiler.Protocol
import           Compiler.Types
import           Control.Arrow                ((&&&))
import           Control.Comonad
import           Control.Comonad.Cofree
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
import           Language.Haskell.Exts.Build  (app, infixApp, paren)
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.Syntax hiding (Int, List, Lit)

-- type Field = Id ::: TType ::: [Derive]

-- FIXME: Should empty responses be a shared type, and
-- always succeed based on HTTP response code?

dataType :: Protocol
         -> Shape (Id ::: Maybe Text ::: Direction ::: Solved)
         -> Either Error (Maybe Data)
dataType proto ((n ::: p ::: d ::: t ::: ds ::: is) :< s) =
    case s of
        Enum   i vs -> Just <$> enum i vs
        Struct i ms -> Just <$> struct i ms
        _           -> return Nothing
  where
    enum :: Info -> Map Id Text -> Either Error Data
    enum i vs = Sum i
        <$> formatted (dataDecl n (map conDecl (Map.keys bs)) ds)
        <*> pure bs
        <*> pure is
      where
        bs :: Map Text Text
        bs = vs & kvTraversal %~ first (^. ctorId p)

    struct :: Info
           -> StructF (Shape (Id ::: Maybe Text ::: Direction ::: Solved))
           -> Either Error Data
    struct i strct = Product i
        <$> formatted (dataDecl n [recDecl p n fs] ds)
        <*> ctor
        <*> traverse lens' fs
        <*> pure mempty -- insts fs
      where
        fs :: [Field]
        fs = zipWith field [1..] . Map.toList $ strct ^. members

        ctor :: Either Error Fun
        ctor = Fun (n ^. smartCtorId) h
            <$> unformatted (ctorSig n fs)
            <*> formatted (ctorDecl p n fs)
          where
            -- FIXME: this should output haddock single quotes to ensure
            -- the type is linked properly.
            h = fromString $ Text.unpack ("'" <> n ^. typeId <> "' smart constructor.")

        lens' :: Field -> Either Error Fun
        lens' f@Field{..} = Fun (fieldId ^. lensId p) fieldComment
            <$> unformatted (lensSig  p t f)
            <*> unformatted (lensDecl p f)

        -- FIXME: Facets of Info for the field need to be layered on top
        -- of the type, such as nonempty, maybe, etc.
--        field :: Int -> (Id, Ref) -> Either Error Field
        field :: Int
              -> (Id, RefF (Shape (Id ::: Maybe Text ::: Direction ::: Solved)))
              -> Field
        field o (k, v) = Field
            { fieldId      = k
            , fieldOrdinal = o
            , fieldType    = rt
            , fieldReq     = Set.member k (strct ^. required)
            , fieldDerive  = rds
            , fieldComment = h
            }
          where
            -- FIXME: need to use the correct member id for the type etc.
            _ ::: _ ::: _ ::: rt ::: rds ::: _ = extract (v ^. refAnn)

            h = fromMaybe "FIXME: Undocumented reference." $
                v ^. refDocumentation

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

formatted, unformatted :: Pretty a => a -> Either Error LazyText
formatted   = pretty True
unformatted = pretty False

pretty :: Pretty a => Bool -> a -> Either Error LazyText
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
        -- { spacing = False
        -- , layout  = PPNoLayout
        -- }


-- annotateTypes :: Config
--               -> Service Identity Shape Shape
--               -> Either Error (Service Identity Data Data)
-- annotateTypes cfg svc@Service{..} = do
--     ps <- prefixes universe'

--     let !ts = solve cfg universe'
--         !ds = directions (svc ^. operations) (svc ^. shapes)

--     cs <- constraints cfg universe'
--     ss <- kvTraverseMaybe (datatype (svc ^. protocol) ps ts cs ds) _shapes

--     return $! svc
--         { _operations = mempty
--         , _shapes     = ss
--         }
--   where
--     universe' = _shapes <> foldMap f _operations

--     f x = Map.fromList
--         [ (x ^. requestName,  x ^. opInput  . _Identity)
--         , (x ^. responseName, x ^. opOutput . _Identity)
--         ]
