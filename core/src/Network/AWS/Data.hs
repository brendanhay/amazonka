-- Module      : Network.AWS.Data
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    -- * ByteString
      module Network.AWS.Data.Internal.ByteString
    , module Network.AWS.Data.Internal.Base64

    -- * Text
    , module Network.AWS.Data.Internal.Text

    -- * Numeric
    , module Network.AWS.Data.Internal.Numeric

    -- * Time
    , module Network.AWS.Data.Internal.Time

    -- * Sensitive
    , module Network.AWS.Data.Internal.Sensitive

    -- * HTTP
    -- ** Body
    , module Network.AWS.Data.Internal.Body

    -- ** Headers
    , module Network.AWS.Data.Internal.Header

    -- ** Path
    , module Network.AWS.Data.Internal.Path

    -- ** Query
    , module Network.AWS.Data.Internal.Query

    -- ** URI
    , module Network.AWS.Data.Internal.URI

    -- * XML
    , module Network.AWS.Data.Internal.XML

    -- * JSON
    , module Network.AWS.Data.Internal.JSON

    -- * Collections
    , module Network.AWS.Data.Internal.List
    , module Network.AWS.Data.Internal.Map
    ) where

import Network.AWS.Data.Internal.Base64
import Network.AWS.Data.Internal.Body
import Network.AWS.Data.Internal.ByteString
import Network.AWS.Data.Internal.Header
import Network.AWS.Data.Internal.JSON
import Network.AWS.Data.Internal.List
import Network.AWS.Data.Internal.Map
import Network.AWS.Data.Internal.Numeric
import Network.AWS.Data.Internal.Path
import Network.AWS.Data.Internal.Query
import Network.AWS.Data.Internal.Sensitive
import Network.AWS.Data.Internal.Text
import Network.AWS.Data.Internal.Time
import Network.AWS.Data.Internal.URI
import Network.AWS.Data.Internal.XML
