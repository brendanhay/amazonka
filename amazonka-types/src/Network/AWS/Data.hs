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
      module BS

    -- * Text
    , module Text

    -- * Time
    , module Time

    -- * XML
    -- , FromXML       (..)
    -- , decodeXML

    , module XML

    -- * HTTP
    , module Header
    , module Path
    , module Query
    , module Body
    ) where

import Network.AWS.Data.Body       as Body
import Network.AWS.Data.ByteString as BS
import Network.AWS.Data.Header     as Header
import Network.AWS.Data.Path       as Path
import Network.AWS.Data.Query      as Query
import Network.AWS.Data.Text       as Text
import Network.AWS.Data.Time       as Time
import Network.AWS.Data.XML        as XML
