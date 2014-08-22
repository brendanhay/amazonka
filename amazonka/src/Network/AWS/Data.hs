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
      module BS

    -- * Text
    , module Text

    -- * Time
    , module Time

    -- * XML
    , module XML

    -- * HTTP
    , module Header
    , module Path
    , module Query
    , module Body
    , module URI
    ) where

import Network.AWS.Internal.Data.Body       as Body
import Network.AWS.Internal.Data.ByteString as BS
import Network.AWS.Internal.Data.Header     as Header
import Network.AWS.Internal.Data.Path       as Path
import Network.AWS.Internal.Data.Query      as Query
import Network.AWS.Internal.Data.Text       as Text
import Network.AWS.Internal.Data.Time       as Time
import Network.AWS.Internal.Data.URI        as URI
import Network.AWS.Internal.Data.XML        as XML
