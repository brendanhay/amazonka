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
    -- * ByteString
      module Network.AWS.Internal.Data.ByteString

    -- * Text
    , module Network.AWS.Internal.Data.Text

    -- * Time
    , module Network.AWS.Internal.Data.Time

    -- * HTTP
    -- ** Body
    , module Network.AWS.Internal.Data.Body

    -- ** Headers
    , module Network.AWS.Internal.Data.Header

    -- ** Path
    , module Network.AWS.Internal.Data.Path

    -- ** Query
    , module Network.AWS.Internal.Data.Query

    -- ** URI
    , module Network.AWS.Internal.Data.URI

    -- * XML
    , module Network.AWS.Internal.Data.XML

    -- * Collections
    -- ** Non-empty List
    , module Network.AWS.Data.List1
    -- ** Unordered Map
    , module Network.AWS.Data.Map
    ) where

import Network.AWS.Data.List1               (List1, list1)
import Network.AWS.Data.Map                 (Map, nullMap)
import Network.AWS.Internal.Data.Body
import Network.AWS.Internal.Data.ByteString
import Network.AWS.Internal.Data.Header
import Network.AWS.Internal.Data.Path
import Network.AWS.Internal.Data.Query
import Network.AWS.Internal.Data.Text
import Network.AWS.Internal.Data.Time
import Network.AWS.Internal.Data.URI
import Network.AWS.Internal.Data.XML
