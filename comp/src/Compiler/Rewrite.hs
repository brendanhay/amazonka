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
import           Compiler.Rewrite.Ann
import           Compiler.Rewrite.Override
import           Compiler.Rewrite.Subst
import           Compiler.Types
import           Control.Error
import           Control.Lens
import           Data.List                 (sort)
import           Data.Monoid
import           Control.Comonad
import           Control.Comonad.Cofree

-- Order:
-- substitute
-- recase
-- override
-- default
-- prefix
-- type

rewrite :: Versions
        -> Config
        -> Service Maybe Ref Shape
        -> Either Error Library
rewrite v c s' = do
    s <- annotateTypes c =<< defaults (substitute $ override c s')

    let ns     = NS ["Network", "AWS", s ^. serviceAbbrev]
        other  = c ^. operationImports ++ c ^. typeImports
        expose = ns
               : ns <> "Types"
               : ns <> "Waiters"
               : map (mappend ns . textToNS)
                     (s ^.. operations . ifolded . asIndex . ctorId)

    return $! Library v c s ns (sort expose) (sort other)

annotate :: Map Id (ShapeF Id) -> Either Error (Map Id (Shape ()))
annotate ss = traverse (fmap cofree . shape) ss
  where
    shape :: ShapeF Id -> Either Error (Mu ShapeF)
    shape = fmap Mu . \case
        List   i e   -> List   i <$> ref e
        Map    i k v -> Map    i <$> ref k <*> ref v
        Struct i s   -> Struct i <$> traverseOf (members . each) ref s
        s            -> pure s
        -- Enum   i vs  -> pure (Enum i vs)
        -- Lit    i l   -> pure (Lit  i l)

    ref :: Ref Id -> Either Error (Ref (Mu ShapeF))
    ref r = flip (set refShape) r <$> (ptr r >>= shape)

    ptr :: Ref Id -> Either Error (ShapeF Id)
    ptr r = do
        let k = r ^. refAnn
        note (format ("Missing shape " % iprimary) k)
             (Map.lookup k ss)

defaults :: Service Maybe Shape Shape
         -> Either Error (Service Identity Shape Shape)
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
        let e = format ("Vacant operation input/output: " % iprimary) _opName
            f = fmap Identity . note e

        rq <- f _opInput
        rs <- f _opOutput

        return $! o
            { _opDocumentation = _opDocumentation .! "FIXME: Undocumented operation."
            , _opHTTP          = http _opHTTP
            , _opInput         = rq
            , _opOutput        = rs
            }

    http h@HTTP{..} = h
        { _responseCode = _responseCode .! 200
        }

    -- shape' = \case
    --     List   i e   -> List   i (ref' e)
    --     Map    i k v -> Map    i (ref' k) (ref' v)
    --     Struct i s   -> Struct i (over (members . each) ref' s)
    --     Enum   i m   -> Enum   i m
    --     Lit    i l   -> Lit    i (lit' l)

    -- ref' r@Ref{..} = r
    --     { _refDocumentation = _refDocumentation .! "FIXME: Undocumented reference."
    --     , _refLocation      = _refLocation      .! Querystring -- FIXME: This is based upon the protocol
    --     , _refLocationName  = _refLocationName  .! _refShape ^. primaryId -- FIXME: This is based up on the protocol and shape type (list == member etc.)
    --     , _refQueryName     = _refQueryName     .! _refShape ^. primaryId
    --     , _refXMLNamespace  = _refXMLNamespace  .! XML' "" ""
    --     }

    -- lit' = \case
    --     Time t -> Time . Identity $ fromMaybe timestamp t
    --     Int    -> Int
    --     Long   -> Long
    --     Double -> Double
    --     Text   -> Text
    --     Blob   -> Blob
    --     Bool   -> Bool

infixl 7 .!

(.!) :: Maybe a -> a -> Identity a
m .! x = maybe (Identity x) Identity m
