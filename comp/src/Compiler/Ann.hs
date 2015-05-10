{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

-- Module      : Compiler.Ann
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Ann where
    -- ( annotateTypes
    -- ) where

import           Compiler.Formatting          hiding (base)
import           Compiler.Protocol
import           Compiler.Ann.Syntax
import           Compiler.Ann.TypeOf
import           Compiler.Prefix
import           Compiler.Types
import           Control.Arrow                ((&&&))
import           Control.Comonad
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

-- FIXME: Should empty responses be a shared type, and
-- always succeed based on HTTP response code?

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

-- data Field = Field
--     { fieldParam    :: Name
--     , fieldType     :: Type
--     , fieldAccessor :: ([Name], Type)
--     , fieldLens     :: Fun
--     , fieldUpdate   :: FieldUpdate
--     , fieldReq      :: !Bool
--     , fieldId       :: Id
--     , fieldRef      :: Ptr
--     , fieldRefShape :: Shape
--     }

-- datatype :: Protocol
--          -> Map Id Text
--          -> Map Id (Cofree Shape)
--          -> Map Id (Set Derive)
--          -> Map Id Direction
--          -> Id
--          -> Shape
--          -> Either Error (Maybe Data)
-- datatype proto ps ts cs ds n = \case
--     Enum   i vs -> satisfy (enum   i vs)
--     Struct i s  -> satisfy (struct i s)
--     _           -> return Nothing
--   where
--     satisfy f = Just <$> ((,) <$> prefix <*> fulfill n >>= uncurry f)

--     is = instances proto

--     prefix :: Either Error Text
--     prefix = flip note (Map.lookup n ps) $
--         format ("Missing prefix for shape " % iprimary %
--                 ", possible matches "       % partial)
--                n (n, ps)

--     fulfill :: Id -> Either Error (Set Derive)
--     fulfill k = flip note (Map.lookup k cs) $
--         format ("Missing constraints for shape " % iprimary %
--                 ", possible matches for "        % iprimary %
--                 ": "                             % partial)
--                n k (k, cs)

--     getTyped :: Id -> Either Error (Typed Shape)
--     getTyped k = flip note (Map.lookup k ts) $
--         format ("Missing type for shape " % iprimary %
--                 ", possible matches for " % iprimary %
--                 ": "                      % partial)
--                n k (k, ts)

--     direct :: Either Error Direction
--     direct = flip note (Map.lookup n ds) $
--         format ("Missing direction for shape " % iprimary %
--                 ", possible matches "          % partial)
--                n (n, ds)

--     name = n ^. typeId

--     enum :: Info
--          -> Map Text Text
--          -> Text
--          -> Set Derive
--          -> Either Error Data
--     enum i vs p ds = Sum i <$> decl <*> pure bs <*> pure is
--       where
--         bs :: Map Text Text
--         bs = vs & kvTraversal %~ first (f . upperHead)
--           where
--             f | Text.null p = id
--               | otherwise   = mappend (upperHead p)

--         decl :: Either Error LazyText
--         decl = pretty True $ dataDecl name (map conDecl $ Map.keys bs) ds

--     struct :: Info
--            -> Struct
--            -> Text
--            -> Set Derive
--            -> Either Error Data
--     struct i s p ds = do
--         fs <- traverse (uncurry field) (zip [1..] . Map.toList $ s ^. members)
--         Product i <$> decl fs <*> ctor fs <*> lenses fs <*> pure mempty -- insts fs
--       where
--         decl fs = pretty True $ dataDecl (n ^. typeId)
--             [ recDecl (n ^. typeId) (map fieldAccessor fs)
--             ] ds

--         ctor fs = Fun c h <$> pretty False sig <*> pretty True body
--           where
--             sig  = typeSig c (n ^. typeId . to tycon) (map fieldType rs)
--             body = funDecl c (map fieldParam rs) $
--                 RecConstr (n ^. typeId . to unqual) (map fieldUpdate fs)

--             c  = n ^. ctorId
--             rs = filter fieldReq fs -- Required fields

--             -- FIXME: this should output haddock single quotes to ensure
--             -- the type is linked properly.
--             h = fromString $ Text.unpack ("'" <> n ^. typeId <> "' smart constructor.")

--         lenses = pure . map fieldLens

--         -- insts :: [Field] -> Either Error (Map Inst [LazyText])
--         -- insts fs = Map.fromList <$> traverse (\i -> (i,) <$> fgh i) is
--         --   where
--         --     fgh :: Inst -> Either Error [LazyText]
--         --     fgh i = implement i $ satisfying i (_refLocation . fieldRef) fs

--         --     implement :: Inst -> [Field] -> Either Error [LazyText]
--         --     implement ToQuery xs = traverse function xs
--         --       where
--         --         function Field{..} = pretty False $ fun fieldRefShape
--         --           where
--         --             fun (List i e) =
--         --                 app (app (var "toQueryList")
--         --                          (str (parent <> maybe mempty (mappend ".") element)))
--         --                     (var (fieldId ^. accessorId p))
--         --               where
--         --                 ((parent, element), _) =
--         --                     listName proto (fieldId, fieldRef) (i, e)

--         --             fun _         =
--         --                 infixApp (str $ fst (memberName proto (fieldId, fieldRef)))
--         --                          (qop "=?")
--         --                          (var (fieldId ^. accessorId p))
--         --     implement _ _ = pure []


--         -- FIXME: Facets of Info for the field need to be layered on top
--         -- of the type, such as nonempty, maybe, etc.
--         field :: Int -> (Id, Ref) -> Either Error Field
--         field (param -> o) (k, v) = do
--             let r = v ^. refShape
--                 a = k ^. accessorId p
--                 l = k ^. lensId p

--             let req = Set.member k (s ^. required)

--             c   <- fulfill r
--             typ <- getTyped r

--             let t = optional req (typ ^. typeof)
--                 h = fromMaybe "FIXME: Undocumented reference."
--                         (v ^. refShape . _refDocumentation)

--             d   <- Fun l h
--                 <$> pretty False (lensSig l (n ^. typeId . to tycon) (external t))
--                 <*> pretty False (funDecl l [] (mapping t (lensBody a)))

--             return $! Field
--                 { fieldParam    = o
--                 , fieldType     = external t
--                 , fieldAccessor = ([ident a], internal t)
--                 , fieldLens     = d
--                 , fieldUpdate   = update a o req c
--                 , fieldReq      = req
--                 , fieldId       = k
--                 , fieldRef      = v
--                 , fieldRefShape = typ ^. typed
--                 }

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
