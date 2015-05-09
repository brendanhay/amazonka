{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

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
import           Compiler.Protocol
import           Compiler.Rewrite.Ann.Syntax
import           Compiler.Rewrite.Ann.TypeOf
import           Compiler.Rewrite.Prefix
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

-- directions :: Map Id Operation
--            -> Map Id Shape
--            -> Either Error (Map Id Bool)
-- directions os ss = execStateT (mapM_ go (Map.elems os)) mempty
--   where
--     go :: Operation Identity Shape -> StateT (Map Id Direction) (Either Error) ()
--     go o = do
--         modify (Map.insertWith (<>) (o ^. requestName) Input)
--         shape Input  (o ^. opInput  . _Identity)
--         modify (Map.insertWith (<>) (o ^. responseName) Output)
--         shape Output (o ^. opOutput . _Identity)

--     shape :: HasRefs a => Direction -> a -> StateT (Map Id Direction) (Either Error) ()
--     shape d = mapM_ count . toListOf references
--       where
--         count :: Ref -> StateT (Map Id Direction) (Either Error) ()
--         count r = do
--             let n = r ^. refShape
--             s <- lift $ note "Missing" (Map.lookup n ss)
--             modify (Map.insertWith (<>) n d)
--             shape d s

type Solve = State (Map Id TType)

solve :: Config -> Map Id (ShapeF Id) -> Map Id (ShapeF TType)
solve cfg ss = evalState (Map.traverseWithKey go ss) initial
  where
    initial :: Map Id TType
    initial = replaced (TType . _replaceName) cfg

    go :: Id -> Shape Ptr -> Solve (ShapeF TType)
    go n s = typed >>= save n
      where
        typed = case s of
            Struct {} -> pure (TType n)
            Enum   {} -> pure (TType n)
            List i e  -> sensitive i <$> (ctor <$> memo (e ^. refShape))
              where
                ctor | i ^. infoMin > Just 0 = TList1
                     | otherwise             = TList

            -- Map  i k v -> fmap (sensitive i . flatten i) $
            --     TMap ( "value"
            --          , k ^. refLocationName . _Identity
            --          , v ^. refLocationName . _Identity
            --          ) <$> memo (k ^. refShape)
            --            <*> memo (v ^. refShape)

            Lit i l -> pure . sensitive i $
                case l of
                    Int    -> natural i (TLit l)
                    Long   -> natural i (TLit l)
                    _      -> TLit l

    memo :: Id -> Solve TType
    memo k = do
        m <- gets (Map.lookup k)
        case m of
            Just x  -> return x
            Nothing ->
                case Map.lookup k ss of
                    Just x  -> extract <$> go k x
                    Nothing -> return (TType k)

--    save :: Id -> Typed Shape -> Solve (Typed Shape)
    save k v = modify (Map.insert k (extract v)) >> return v

type Constraints = StateT (Map Id (Set Derive)) (Either Error)

-- constraints :: Config
--             -> Map Id Shape
--             -> Either Error (Map Id (Set Derive))
-- constraints cfg ss = evalStateT (Map.traverseWithKey go ss) initial
--   where
--     initial :: Map Id (Set Derive)
--     initial = replaced _replaceDerives cfg

--     go :: Id -> Shape -> Constraints (Set Derive)
--     go n s = do
--         m <- gets (Map.lookup n)
--         case m of
--             Just cs -> return cs
--             Nothing -> derive n s

--     -- FIXME: Filter constraints based on things like min/max of lists etc.
--     derive n = save n <=< \case
--         Struct _ s -> cplx n s
--         List   {}  -> pure (base <> mono)
--         Map    {}  -> pure (base <> mono)
--         Enum   {}  -> pure (base <> enum)
--         Lit    _ l -> pure $
--             case l of
--                 Int    -> base <> num
--                 Long   -> base <> num
--                 Double -> base <> frac
--                 Text   -> base <> str
--                 Blob   -> [DShow]
--                 Time _ -> base <> enum
--                 Bool   -> base <> enum

--     cplx :: Id -> Struct -> Constraints (Set Derive)
--     cplx n s = combine <$> traverse ref (s ^.. references . refShape)
--       where
--         combine :: [Set Derive] -> Set Derive
--         combine [x]    = x
--         combine (x:xs) = Set.intersection (Fold.foldl' Set.intersection x xs) base
--         combine _      = base

--         ref :: Id -> Constraints (Set Derive)
--         ref k = cache >>= maybe miss return
--           where
--             cache = gets (Map.lookup k)
--             miss  = lift (note e (Map.lookup k ss)) >>= go k

--             e = format ("Missing shape "                     % iprimary %
--                         " when determining constraints for " % iprimary %
--                         " :: "                               % shown %
--                         ", in possible matches "             % partial)
--                        k n (s ^.. references) (k, ss)

--     str, num, frac, mono, enum, base :: Set Derive
--     str  = [DIsString]
--     num  = [DEnum, DNum, DIntegral, DReal]
--     frac = [DRealFrac, DRealFloat]
--     mono = [DMonoid, DSemigroup]
--     enum = [DEnum]
--     base = [DEq, DOrd, DRead, DShow]

--     save :: Id -> Set Derive -> Constraints (Set Derive)
--     save k x = modify (Map.insertWith (<>) k x) >> return x

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

replaced :: (Replace -> a) -> Config -> Map Id a
replaced f =
      Map.fromList
    . map (_replaceName &&& f)
    . Map.elems
    . vMapMaybe _replacedBy
    . _typeOverrides
