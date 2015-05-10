{-# LANGUAGE BangPatterns      #-}
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
import           Compiler.Formatting
import           Compiler.Protocol
import           Control.Monad.State
import qualified Data.HashMap.Strict    as Map
import           Debug.Trace
--import           Compiler.AST.Data
import           Compiler.AST.Solve
import           Compiler.Override
import           Compiler.Subst
import           Compiler.Types
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Error
import           Control.Lens
import           Data.List              (sort)
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
rewrite v c s' = do
    -- Apply the configured overrides to the service.
    let s  = override c s'
--    substitute $ defaults .

    -- Elaborate the map into a comonadic strucutre for traversing.
    s1 <- elaborate (s ^. shapes)

    -- Determine which direction (input, output, or both) shapes are used.
    s2 <- flip map s1 . attach <$> directions (s ^. operations) (s ^. shapes)

    -- Generate unique prefixes for struct members and enums to avoid ambiguity.
    s3 <- prefixes s2

    -- Determine the Haskell AST Type, auto derived instances, and manual instances.
    let !s4 = solve c s2

--    so now map over the list of shapes, turning them into 'Data' decls.

--    s <- annotateTypes c =<< defaults (substitute $ override c s')
    s5 <- traverse (dataType (s ^. protocol)) s4

    undefined

    -- let ns     = NS ["Network", "AWS", s ^. serviceAbbrev]
    --     other  = c ^. operationImports ++ c ^. typeImports
    --     expose = ns
    --            : ns <> "Types"
    --            : ns <> "Waiters"
    --            : map (mappend ns . textToNS)
    --                  (s ^.. operations . ifolded . asIndex . ctorId)

    -- return $! Library v c s ns (sort expose) (sort other)

type Dir = StateT (Map Id Direction) (Either Error)

directions :: Map Id (Operation Maybe (RefF a))
           -> Map Id (ShapeF b)
           -> Either Error (Map Id Direction)
directions os ss = execStateT (traverse go os) mempty
  where
    go :: Operation Maybe (RefF a) -> Dir ()
    go o = mode Input requestName opInput >> mode Output responseName opOutput
      where
        mode (Mode -> d) f g = do
            modify (Map.insertWith (<>) (o ^. f) d)
            count d (o ^? g . _Just . refShape)

    shape :: Direction -> ShapeF a -> Dir ()
    shape d = mapM_ (count d . Just . view refShape) . toListOf references

    count :: Direction -> Maybe Id -> Dir ()
    count d Nothing  = pure ()
    count d (Just n) = do
        modify (Map.insertWith (<>) n d)
        s <- lift $
            note (format ("Unable to find shape " % iprimary) n)
                 (Map.lookup n ss)
        shape d s

elaborate :: Map Id (ShapeF a) -> Either Error [Shape Id]
elaborate ss = Map.elems <$> Map.traverseWithKey shape ss
  where
    shape :: Id -> ShapeF a -> Either Error (Shape Id)
    shape n s = (n :<) <$>
        case s of
            List   i e   -> List   i <$> ref e
            Map    i k v -> Map    i <$> ref k <*> ref v
            Struct i o   -> Struct i <$> traverseOf (members . each) ref o
            Enum   i vs  -> pure (Enum i vs)
            Lit    i l   -> pure (Lit  i l)

    ref :: RefF a -> Either Error (RefF (Shape Id))
    ref r = flip (set refAnn) r <$> (safe n >>= shape n)
      where
        n = r ^. refShape

    safe n = note (format ("Missing shape " % iprimary) n) (Map.lookup n ss)

infixl 7 .!

(.!) :: Maybe a -> a -> Identity a
m .! x = maybe (Identity x) Identity m

defaults :: Service Maybe (ShapeF ()) (ShapeF ())
         -> Either Error (Service Identity (ShapeF ()) (ShapeF ()))
defaults svc@Service{..} = do
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
