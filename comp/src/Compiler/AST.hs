{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.AST
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST where

import           Compiler.AST.Cofree
import           Compiler.AST.Data
import           Compiler.AST.Override
import           Compiler.AST.Prefix
import           Compiler.AST.Solve
import           Compiler.AST.Subst
import           Compiler.Formatting
import           Compiler.Types
import           Control.Error
import           Control.Lens
import           Control.Monad.Except  (throwError)
import           Control.Monad.State
import qualified Data.HashMap.Strict   as Map
import qualified Data.HashSet          as Set
import           Data.Monoid

-- FIXME: Relations need to be updated by the solving step.

-- FIXME: add pruning of unreferenced shapes, such as errors etc.

rewrite :: Versions
        -> Config
        -> Service Maybe (RefF ()) (ShapeF ())
        -> Either Error Library
rewrite v cfg s' = Library v cfg
    <$> (rewriteService cfg (deprecate s')
         >>= renderShapes cfg)

deprecate :: Service f a b -> Service f a b
deprecate = operations %~ Map.filter (not . view opDeprecated)

rewriteService :: Config
               -> Service Maybe (RefF ()) (ShapeF ())
               -> Either Error (Service Identity (RefF ()) (Shape Related))
rewriteService cfg s = do
        -- Determine which direction (input, output, or both) shapes are used.
    rs <- relations (s ^. operations) (s ^. shapes)
        -- Elaborate the shape map into a comonadic strucutre for traversing.
    elaborate (s ^. shapes)
        -- Annotate the comonadic tree with the associated
        -- bi/unidirectional (input/output/both) relation for shapes.
        >>= traverse (pure . attach Related rs)
        -- Apply the override configuration to the service, and default any
        -- optional fields from the JSON where needed.
        >>= return . (\ss -> override (cfg ^. typeOverrides) (s { _shapes = ss }))
        -- Ensure no empty operation references exist, and that operation shapes
        -- are considered 'unique', so they can be lifted into the operation's
        -- module, separately from .Types.
        >>= substitute

renderShapes :: Config
             -> Service Identity (RefF ()) (Shape Related)
             -> Either Error (Service Identity Data Data)
renderShapes cfg svc = do
        -- Generate unique prefixes for struct (product) members and
        -- enum (sum) branches to avoid ambiguity.
    (x, y) <- prefixes (svc ^. shapes)
        -- Determine the appropriate Haskell AST type, auto deriveable instances,
        -- and fully rendered instances.
        >>= return . solve cfg
        -- Separate the operation input/output shapes from the .Types shapes.
        >>= separate (svc ^. operations)

    -- Convert shape ASTs into a rendered Haskell AST declaration
    xs <- traverse (operationData svc) x
    ys <- kvTraverseMaybe (const (shapeData svc)) y

    return $! svc
        { _operations = xs
        , _shapes     = ys
        }

type MemoR = StateT (Map Id Relation) (Either Error)

-- | Determine the relation for operation payloads, both input and output.
--
-- /Note:/ This currently doesn't operate over the free AST, since it's also
-- used by 'setDefaults'.
relations :: Show b
          => Map Id (Operation Maybe (RefF a))
          -> Map Id (ShapeF b)
          -> Either Error (Map Id Relation)
relations os ss = execStateT (traverse go os) mempty
  where
    go :: Operation Maybe (RefF a) -> MemoR ()
    go o = count (o ^. opName) Input  (o ^? opInput  . _Just . refShape)
        >> count (o ^. opName) Output (o ^? opOutput . _Just . refShape)

    -- | Inserts a valid relation containing an referring shape's id,
    -- and the direction the parent is used in.
    count :: Id -> Direction -> Maybe Id -> MemoR ()
    count _ _       Nothing  = pure ()
    count p d (Just n) = do
        modify $ Map.insertWith (<>) n (mkRelation (Set.singleton p) d)
        s <- lift (safe n)
        shape n d s

    shape :: Id -> Direction -> ShapeF a -> MemoR ()
    shape p d = mapM_ (count p d . Just . view refShape)
        . toListOf references

    safe n = note
        (format ("Missing shape "            % iprimary %
                 " when counting relations " %
                 ", possible matches: "      % partial)
                n (n, ss))
        (Map.lookup n ss)

type MemoS a = StateT (Map Id a) (Either Error)

-- | Filter the ids representing operation input/outputs from the supplied map,
-- and attach the associated shape to the appropriate operation.
separate :: Show a => Map Id (Operation Identity (RefF b))
         -> Map Id a
         -> Either Error (Map Id (Operation Identity a), Map Id a)
separate os ss = runStateT (traverse go os) ss
  where
    go :: Operation Identity (RefF a) -> MemoS b (Operation Identity b)
    go o = do
        x <- remove (o ^. inputName)
        y <- remove (o ^. outputName)
        return $! o
            { _opInput  = Identity x
            , _opOutput = Identity y
            }

    remove :: Id -> MemoS a a
    remove n = do
        s <- get
        case Map.lookup n s of
            Just x  -> modify (Map.delete n) >> return x
            Nothing -> throwError $
                format ("Failure attempting to remove operation wrapper " % iprimary %
                        " from " % partial)
                       n (n, Map.map (const ()) s)

-- type Subst = StateT (Map Id Override, Map Id (ShapeF ())) (Either Error)

-- -- | Set some appropriate defaults where needed for later stages,
-- -- and ensure there are no vacant references to input/output shapes
-- -- by adding any empty request or response types where appropriate.
-- setDefaults :: Map Id Relation
--             -> Service Maybe (RefF ()) (ShapeF ())
--             -> Either Error (Service Identity (RefF ()) (ShapeF ()))
-- setDefaults rs svc@Service{..} = do
--     (os, (ovs, ss)) <-
--         runStateT (traverse operation _operations) initial
--     -- Apply any overrides that might have been returned for wrappers.
--     return $! override ovs $ svc
--         { _metadata'  = meta _metadata'
--         , _operations = os
--         , _shapes     = ss
--         }
--   where
--     initial :: (Map Id Override, Map Id (ShapeF ()))
--     initial = (mempty, Map.map shape _shapes)

--     meta :: Metadata Maybe -> Metadata Identity
--     meta m@Metadata{..} = m
--         { _timestampFormat = Identity ts
--         , _checksumFormat  = _checksumFormat  .! SHA256
--         }

--     ts :: Timestamp
--     ts = fromMaybe (timestamp (svc ^. protocol)) (svc ^. timestampFormat)

--     operation :: Operation Maybe (RefF ())
--               -> Subst (Operation Identity (RefF ()))
--     operation o@Operation{..} = do
--         inp <- subst (name Input  _opName) _opInput
--         out <- subst (name Output _opName) _opOutput
--         return $! o
--             { _opDocumentation =
--                 _opDocumentation .! "FIXME: Undocumented operation."
--             , _opHTTP          = http _opHTTP
--             , _opInput         = inp
--             , _opOutput        = out
--             }

--     http :: HTTP Maybe -> HTTP Identity
--     http h = h
--         { _responseCode = _responseCode h .! 200
--         }

--     shape :: ShapeF a -> ShapeF a
--     shape = \case
--         Lit i (Time Nothing) -> Lit i . Time $ Just ts
--         x                    -> x

--     -- FIXME: too complicated? Just copy the shape if it's shared, and since
--     -- this is an operation, consider it safe to remove the shape wholly?

--     -- Fill out missing Refs with a default Ref pointing to an empty Shape,
--     -- which is also inserted into the resulting Shape universe.
--     --
--     -- Likewise provide an appropriate wrapper over any shared Shape.
--     subst :: Id -> Maybe (RefF ()) -> Subst (Identity (RefF ()))
--     subst n (Just r)
--           -- Ref exists, and is not referred to by any other Shape.
--         | not (Set.member (r ^. refShape) shared) = do
--             -- Insert override to rename the Ref/Shape to the desired name.
--             _1 %= Map.insert (r ^. refShape) (defaultOverride & renamedTo ?~ n)
--             return $! Identity r

--           -- Ref exists and is referred to by other shapes.
--         | otherwise = do
--             -- Check that the desired name is not in use
--             -- to prevent accidental override.
--             verify n "Failed attempting to create wrapper"
--             -- Create a newtype wrapper which points to the shared Shape
--             -- and has 'StructF.wrapper' set.
--             _2 %= Map.insert n (emptyStruct [(r ^. refShape, r)] True)
--             -- Update the Ref to point to the new wrapper.
--             return $! Identity (r & refShape .~ n)

--     -- No Ref exists, safely insert an empty shape and return a related Ref.
--     subst n Nothing  = do
--         verify n "Failure attemptting to substitute fresh shape"
--         _2 %= Map.insert n (emptyStruct mempty False)
--         return $! Identity (emptyRef n)

--     verify n msg = do
--         p <- uses _2 (Map.member n)
--         when p . throwError $
--             format (msg % " for " % iprimary) n

--     name :: Direction -> Id -> Id
--     name Input  n = mkId (n ^. typeId)
--     name Output n = mkId (appendId n "Response" ^. typeId)

--     shared :: Set Id
--     shared = sharing rs

--     infixl 7 .!

--     (.!) :: Maybe a -> a -> Identity a
--     m .! x = maybe (Identity x) Identity m

--     emptyStruct ms = Struct . StructF i ms (Set.fromList (map fst ms)) Nothing
--       where
--         i = Info
--             { _infoDocumentation = Nothing
--             , _infoMin           = Nothing
--             , _infoMax           = Nothing
--             , _infoFlattened     = False
--             , _infoSensitive     = False
--             , _infoStreaming     = False
--             , _infoException     = False
--             }

--     emptyRef n = RefF
--         { _refAnn           = ()
--         , _refShape         = n
--         , _refDocumentation = Nothing
--         , _refLocation      = Nothing
--         , _refLocationName  = Nothing
--         , _refResultWrapper = Nothing
--         , _refQueryName     = Nothing
--         , _refStreaming     = False
--         , _refXMLAttribute  = False
--         , _refXMLNamespace  = Nothing
--         }
