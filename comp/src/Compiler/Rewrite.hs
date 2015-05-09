{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.Rewrite
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite where

import           Compiler.Formatting
import           Compiler.Protocol
import qualified Data.HashMap.Strict       as Map
import           Debug.Trace
--import           Compiler.Rewrite.Ann
import           Compiler.Rewrite.Override
import           Compiler.Rewrite.Solve
import           Compiler.Rewrite.Subst
import           Compiler.Types
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Error
import           Control.Lens
import           Data.List                 (sort)
import           Data.Monoid

-- Order:
-- substitute
-- recase
-- override
-- default
-- prefix
-- type

rewrite :: Versions
        -> Config
        -> Service Maybe (RefF ()) (ShapeF ())
        -> Either Error Library
rewrite v c s' = do
    s   <- defaults . substitute $ override c s'
    ss  <- elaborate (s ^. shapes)

    let !ss' = solve c ss

--    s <- annotateTypes c =<< defaults (substitute $ override c s')
    undefined
    -- let ns     = NS ["Network", "AWS", s ^. serviceAbbrev]
    --     other  = c ^. operationImports ++ c ^. typeImports
    --     expose = ns
    --            : ns <> "Types"
    --            : ns <> "Waiters"
    --            : map (mappend ns . textToNS)
    --                  (s ^.. operations . ifolded . asIndex . ctorId)

    -- return $! Library v c s ns (sort expose) (sort other)

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
