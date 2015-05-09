{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Compiler.Rewrite.Default
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.Default
    ( setDefaults
    ) where

import           Compiler.Formatting
import           Compiler.Types
import           Control.Error
import           Control.Lens
import qualified Data.HashMap.Strict as Map

-- | Set defaults for variousOf fields post-parsing as determined by the
-- protocol and service type.
setDefaults :: Service Maybe Shape Shape
            -> Either Error (Service Identity Shape Shape)
setDefaults svc@Service{..} = do
    os <- traverse operation' _operations
    return $! svc
        { _metadata'  = meta' _metadata'
        , _operations = os
        , _shapes     = Map.map shape' _shapes
        }
  where
    meta' m@Metadata{..} = m
        { _timestampFormat = _timestampFormat .! timestamp
        , _checksumFormat  = _checksumFormat  .! SHA256
        }

    operation' o@Operation{..} = do
        let e = format ("Vacant operation input/output: " % iprimary) _opName
            f = fmap (Identity . shape') . note e
        rq <- f _opInput
        rs <- f _opOutput
        return $! o
            { _opDocumentation = _opDocumentation .! "FIXME: Undocumented operation."
            , _opHTTP          = http' _opHTTP
            , _opInput         = rq
            , _opOutput        = rs
            }

    http' h@HTTP{..} = h
        { _responseCode = _responseCode .! 200
        }

    shape' = \case
        List   i e   -> List   i (ref' e)
        Map    i k v -> Map    i (ref' k) (ref' v)
        Struct i s   -> Struct i (over (members . each) ref' s)
        Enum   i m   -> Enum   i m
        Lit    i l   -> Lit    i (lit' l)

    ref' r@Ref{..} = r
        { _refDocumentation = _refDocumentation .! "FIXME: Undocumented reference."
        , _refLocation      = _refLocation      .! Querystring -- FIXME: This is based upon the protocol
        , _refLocationName  = _refLocationName  .! _refShape ^. primaryId -- FIXME: This is based up on the protocol and shape type (list == member etc.)
        , _refQueryName     = _refQueryName     .! _refShape ^. primaryId
        , _refXMLNamespace  = _refXMLNamespace  .! XML' "" ""
        }

    lit' = \case
        Time t -> Time . Identity $ fromMaybe timestamp t
        Int    -> Int
        Long   -> Long
        Double -> Double
        Text   -> Text
        Blob   -> Blob
        Bool   -> Bool

infixl 7 .!

(.!) :: Maybe a -> a -> Identity a
m .! x = maybe (Identity x) Identity m
