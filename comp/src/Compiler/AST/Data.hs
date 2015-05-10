{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE ViewPatterns   #-}
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

data Field = Field
    { fieldParam    :: Name
    , fieldType     :: Type
    , fieldAccessor :: ([Name], Type)
    , fieldLens     :: Fun
    , fieldUpdate   :: FieldUpdate
    , fieldReq      :: !Bool
    }

-- FIXME: Should empty responses be a shared type, and
-- always succeed based on HTTP response code?

dataType :: Protocol
         -> Shape (Id ::: Direction ::: Maybe Text ::: Solved)
         -> Either Error (Maybe Data)
dataType proto ((n ::: d ::: p ::: t ::: ds ::: is) :< s) =
    case s of
        Enum   i vs -> Just <$> enum i vs
        Struct i ms -> Just <$> struct i ms
        _           -> return Nothing
  where
    enum :: Info -> Map Text Text -> Either Error Data
    enum i vs = Sum i
        <$> formatted (dataDecl n (map conDecl (Map.keys bs)) ds)
        <*> pure
        <*> pure is
      where
        bs :: Map Text Text
        bs = Map.keys vs & kvTraversal %~ first (f . upperHead)

        f :: Text -> Text
        f | Just x <- p = mappend (upperHead x)
          | otherwise   = id

    -- struct :: Info -> Struct
    --        -> Text
    --        -> Set Derive
    --        -> Either Error Data
    struct :: Info -> _ -> Either Error Data
    struct i s = do
        fs <- traverse (uncurry field) imembers
        Product i <$> decl fs <*> ctor fs <*> lenses fs <*> pure mempty -- insts fs
      where
        imembers :: [(Int, _)]
        imembers = zip [1..] . Map.toList $ s ^. members

        decl fs = formatted $ dataDecl (n ^. typeId)
            [ recDecl (n ^. typeId) (map fieldAccessor fs)
            ] ds

        ctor fs = Fun c h
            <$> unformatted sig
            <*> formatted bdy
          where
            sig = ctorSig n (map fieldType rs)
            bdy = ctorDecl p n (map fieldParam rs)

            -- FIXME: this should output haddock single quotes to ensure
            -- the type is linked properly.
            h = fromString $ Text.unpack ("'" <> n ^. typeId <> "' smart constructor.")

        lenses = pure . map fieldLens

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


        -- FIXME: Facets of Info for the field need to be layered on top
        -- of the type, such as nonempty, maybe, etc.
--        field :: Int -> (Id, Ref) -> Either Error Field
        field (param -> o) (k, v) = do
            let r = v ^. refShape
                a = k ^. accessorId p
                l = k ^. lensId p

            let req = Set.member k (s ^. required)

            let h = fromMaybe "FIXME: Undocumented reference."
                        (v ^. refShape . _refDocumentation)

            d <- Fun l h
                <$> unformatted (lensSig  p k t t)
                <*> unformatted (lensDecl p k t)

            return $! Field
                { fieldParam    = o
                , fieldType     = external t
                , fieldAccessor = ([ident a], internal t)
                , fieldLens     = d
                , fieldUpdate   = update a o req c
                , fieldReq      = req
                }

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
