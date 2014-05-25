-- Module      : Network.AWS.Data
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Serialisation classes and primitives for the various
--   formats used to communicate with AWS.
module Network.AWS.Data
    (
    -- * ByteStrings
      BS.ToByteString  (..)
    , BS.showByteString

    -- * Text
    , Text.FromText    (..)
    , Text.fromText
    , Text.readText

    , Text.ToText      (..)
    , Text.showText

    -- * Time
    , Time.AWSTime     (..)
    , Time.RFC822Time  (..)
    , Time.ISO8601Time (..)
    , Time.BasicTime   (..)

    -- * XML
    -- , FromXML       (..)
    -- , decodeXML

    -- , ToXML         (..)
    -- , encodeXML

    -- * HTTP
    -- ** Headers

    -- ** Paths
    , Path.ToPath      (..)

    -- ** QueryStrings
    ) where

import qualified Network.AWS.Data.ByteString as BS
-- import qualified Network.AWS.Data.Header     as Header
import qualified Network.AWS.Data.Path       as Path
-- import qualified Network.AWS.Data.Query      as Query
import qualified Network.AWS.Data.Text       as Text
import qualified Network.AWS.Data.Time       as Time
-- import qualified Network.AWS.Data.XML        as XML
