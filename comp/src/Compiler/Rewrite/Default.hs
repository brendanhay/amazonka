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

import           Compiler.Types
import           Control.Error
import           Control.Lens
import qualified Data.HashMap.Strict as Map
import           Data.Monoid
import qualified Data.Text.Lazy      as LText

-- | Set defaults for variousOf fields post-parsing as determined by the
-- protocol and service type.
setDefaults :: Monad m
            => Service Maybe Shape Shape
            -> Compiler m (Service Identity Shape Shape)
setDefaults svc@Service{..} = hoistEither $ do
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
        let may m f = Identity . shape' <$>
                          note (m <> LText.fromStrict (_opName ^. keyOriginal)) f
        rq <- may "Vacant operation input: "  _opInput
        rs <- may "Vacant operation output: " _opOutput
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
        , _refLocationName  = _refLocationName  .! _refShape ^. keyOriginal -- FIXME: This is based up on the protocol and shape type (list == member etc.)
        , _refQueryName     = _refQueryName     .! _refShape ^. keyOriginal
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

    timestamp =
        case svc ^. protocol of
            JSON     -> POSIX
            RestJSON -> POSIX
            XML      -> ISO8601
            RestXML  -> ISO8601
            Query    -> ISO8601
            EC2      -> ISO8601

infixl 7 .!

(.!) :: Maybe a -> a -> Identity a
m .! x = maybe (Identity x) Identity m
