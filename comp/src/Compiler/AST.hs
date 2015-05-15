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
import           Control.Monad.State
import qualified Data.HashMap.Strict as Map
import           Data.List           (sort)
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
    -- Apply the configured overrides to the service.
    let s' = override cfg s''
--    substitute $ defaults .
    ss <- rewriteShapes cfg s'

--    let s = s' & shapes .~ ss
    s''' <- setDefaults s'



    let s  = s''' { _shapes = ss }

    let ns     = NS ["Network", "AWS", s ^. serviceAbbrev]
        other  = cfg ^. operationImports ++ cfg ^. typeImports
        expose = ns
               : ns <> "Types"
               : ns <> "Waiters"
               : map (mappend ns . textToNS)
                     (s ^.. operations . ifolded . asIndex . typeId)

    return $! Library v cfg s ns (sort expose) (sort other)

-- substitute :: Config
--            -> Service Maybe (RefF ()) (ShapeF ())
--            -> Service Maybe ())

rewriteShapes :: Config
              -> Service Maybe (RefF ()) (ShapeF ())
              -> Either Error (Map Id Data)
rewriteShapes cfg svc = do
    -- Determine which direction (input, output, or both) shapes are used.
    rs <- relations (svc ^. operations) (svc ^. shapes)
    -- Elaborate the map into a comonadic strucutre for traversing.
    elaborate (svc ^. shapes)
        -- Generate unique prefixes for struct members and enums to avoid ambiguity.
        >>= prefixes
        -- Annotate the comonadic tree with the associated directions.
        >>= traverse (pure . attach rs)
        -- Determine the Haskell AST type, auto derived instances, and manual instances.
        >>= pure . solve cfg (svc ^. protocol)
        -- Convert the shape AST into a rendered Haskell AST declaration
        >>= kvTraverseMaybe (const (dataType (svc ^. protocol) . fmap rassoc))

type MemoR = StateT (Map Id Relation) (Either Error)

-- | Determine the relation for operation payloads, both input and output.
relations :: Map Id (Operation Maybe (RefF a))
          -> Map Id (ShapeF b)
          -> Either Error (Map Id Relation)
relations os ss = execStateT (traverse go os) mempty
  where
    go :: Operation Maybe (RefF a) -> MemoR ()
    go o = mode Input requestName opInput >> mode Output responseName opOutput
      where
        mode (Uni -> d) f g = do
            modify (Map.insertWith (<>) (o ^. f) d)
            count d (o ^? g . _Just . refShape)

    shape :: Relation -> ShapeF a -> MemoR ()
    shape d = mapM_ (count d . Just . view refShape) . toListOf references

    count :: Relation -> Maybe Id -> MemoR ()
    count _ Nothing  = pure ()
    count d (Just n) = do
        modify (Map.insertWith (<>) n d)
        s <- lift $
            note (format ("Unable to find shape " % iprimary %
                          " when counting relations")
                         n)
                 (Map.lookup n ss)
        shape d s

setDefaults :: Service Maybe (RefF ()) a
            -> Either Error (Service Identity (RefF ()) a)
setDefaults svc@Service{..} = do
    os <- traverse operation _operations
    return $! svc
        { _metadata'  = meta _metadata'
        , _operations = os
        }
  where
    meta m@Metadata{..} = m
        { _timestampFormat = _timestampFormat .! timestamp _protocol
        , _checksumFormat  = _checksumFormat  .! SHA256
        }

    operation o@Operation{..} = do
        let h = _opDocumentation .! "FIXME: Undocumented operation."
            e = format ("Vacant operation input/output: " % iprimary) _opName
            f = fmap Identity . note e
        rq <- f _opInput
        rs <- f _opOutput
        return $! o
            { _opDocumentation = h
            , _opHTTP          = http _opHTTP
            , _opInput         = rq
            , _opOutput        = rs
            }

    http h = h
        { _responseCode = _responseCode h .! 200
        }

infixl 7 .!

(.!) :: Maybe a -> a -> Identity a
m .! x = maybe (Identity x) Identity m
