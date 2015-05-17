{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

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
import           Compiler.AST.Prefix
import           Compiler.AST.Solve
import           Compiler.Formatting
import           Compiler.Override
import           Compiler.Protocol
import           Compiler.Types
import           Control.Error
import           Control.Lens
import           Control.Monad.Except (throwError)
import           Control.Monad.State
import qualified Data.HashMap.Strict  as Map
import           Data.List            (sort)
import           Data.Monoid

-- Order:
-- substitute
-- recase
-- override
-- default
-- prefix
-- type

-- FIXME:
-- For now ignore substitution/operations, since it'd be good do all the AST annotations
-- on a single env of [Shape ..]

rewrite :: Versions
        -> Config
        -> Service Maybe (RefF ()) (ShapeF ())
        -> Either Error Library
rewrite v cfg s'' = do
    -- Apply the override configuration to the service, and default any
    -- optional fields from the JSON where needed.
    s' <- setDefaults (override cfg s'')

    -- Perform the necessary rewrites and rendering
    -- of shapes Haskell data declarations.
    ss <- renderShapes cfg s'

    let s  = s' { _shapes = ss }

    let ns     = NS ["Network", "AWS", s ^. serviceAbbrev]
        other  = cfg ^. operationImports ++ cfg ^. typeImports
        expose = ns
               : ns <> "Types"
               : ns <> "Waiters"
               : map (mappend ns . textToNS)
                     (s ^.. operations . ifolded . asIndex . typeId)

    return $! Library v cfg s ns (sort expose) (sort other)

renderShapes :: Config
             -> Service Identity (RefF ()) (ShapeF ())
             -> Either Error (Map Id Data)
renderShapes cfg svc = do
    -- Determine which direction (input, output, or both) shapes are used.
    rs <- relations (svc ^. operations) (svc ^. shapes)
    -- Elaborate the map into a comonadic strucutre for traversing.
    elaborate (svc ^. shapes)
        -- Generate unique prefixes for struct (product) members and
        -- enum (sum) branches to avoid ambiguity.
        >>= prefixes
        -- Annotate the comonadic tree with the associated
        -- bi/unidirectional (input/output/both) relation for shapes
        >>= traverse (pure . attach rs)
        -- Determine the appropriate Haskell AST type, auto deriveable instances,
        -- and fully rendered instances.
        >>= pure . solve cfg (svc ^. protocol)
        -- Convert the shape AST into a rendered Haskell AST declaration
        >>= kvTraverseMaybe (const (dataType (svc ^. protocol) . fmap rassoc))

type MemoR = StateT (Map Id Relation) (Either Error)

-- FIXME:
-- Maybe make this more detailed and provide a map of which shapes are used
-- by which other shapes? This can be used to create
-- cross linked haddock markup like /See:/ Parent1, Parent2, etc.

-- | Determine the relation for operation payloads, both input and output.
relations :: Map Id (Operation Identity (RefF a))
          -> Map Id (ShapeF b)
          -> Either Error (Map Id Relation)
relations os ss = execStateT (traverse go os) mempty
  where
    go :: Operation Identity (RefF a) -> MemoR ()
    go o = mode Input requestName opInput >> mode Output responseName opOutput
      where
        mode (Uni -> d) f g = do
            modify (Map.insertWith (<>) (o ^. f) d)
            count d (o ^. g . _Identity . refShape)

    shape :: Relation -> ShapeF a -> MemoR ()
    shape d = mapM_ (count d . view refShape) . toListOf references

    count :: Relation -> Id -> MemoR ()
    count d n = do
        modify (Map.insertWith (<>) n d)
        s <- lift $
            note (format ("Unable to find shape " % iprimary %
                          " when counting relations")
                         n)
                 (Map.lookup n ss)
        shape d s

type Subst = StateT (Map Id (ShapeF ())) (Either Error)

-- | Set some appropriate defaults where needed for later stages,
-- and ensure there are no vacant references to input/output shapes
-- by adding any empty request or response types where appropriate.
setDefaults :: Service Maybe (RefF ()) (ShapeF ())
            -> Either Error (Service Identity (RefF ()) (ShapeF ()))
setDefaults svc@Service{..} = do
    (os, ss) <- runStateT (traverse operation _operations) _shapes
    return $! svc
        { _metadata'  = meta _metadata'
        , _operations = os
        , _shapes     = ss
        }
  where
    meta :: Metadata Maybe -> Metadata Identity
    meta m@Metadata{..} = m
        { _timestampFormat = _timestampFormat .! timestamp _protocol
        , _checksumFormat  = _checksumFormat  .! SHA256
        }

    operation :: Operation Maybe (RefF ())
              -> Subst (Operation Identity (RefF ()))
    operation o@Operation{..} = do
        rq <- subst (name _opName Input) _opInput
        rs <- subst (name _opName Output) _opOutput
        return $! o
            { _opDocumentation =
                _opDocumentation .! "FIXME: Undocumented operation."
            , _opHTTP          = http _opHTTP
            , _opInput         = rq
            , _opOutput        = rs
            }

    http :: HTTP Maybe -> HTTP Identity
    http h = h
        { _responseCode = _responseCode h .! 200
        }

    name :: Id -> Direction -> Id
    name o Input  = o
    name o Output = appendId o "Response"

    subst :: Id -> Maybe (RefF ()) -> Subst (Identity (RefF ()))
    subst _ (Just r) = pure (Identity r)
    subst n Nothing  = do
          p <- gets (Map.member n)
          when p . throwError $
              format ("Failling attempt to substitute a fresh shape for " %
                     iprimary) n
          modify (Map.insert n s)
          return (Identity r)
      where
        s = Struct (StructF i mempty mempty Nothing False)
        i = Info
            { _infoDocumentation = Nothing
            , _infoMin           = Nothing
            , _infoMax           = Nothing
            , _infoFlattened     = False
            , _infoSensitive     = False
            , _infoStreaming     = False
            , _infoException     = False
            }
        r = RefF
            { _refAnn           = ()
            , _refShape         = n
            , _refDocumentation = Nothing
            , _refLocation      = Nothing
            , _refLocationName  = Nothing
            , _refResultWrapper = Nothing
            , _refQueryName     = Nothing
            , _refStreaming     = False
            , _refXMLAttribute  = False
            , _refXMLNamespace  = Nothing
            }

    infixl 7 .!

    (.!) :: Maybe a -> a -> Identity a
    m .! x = maybe (Identity x) Identity m
