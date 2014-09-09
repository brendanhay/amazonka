-- Module      : Network.AWS.S3.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Current version of the S3 service, monadic version.
module Network.AWS.S3.Monadic
    ( module Network.AWS.S3.V2006_03_01.Monadic
    , module Network.AWS.S3.Internal.Types
    ) where

import Network.AWS.S3.V2006_03_01.Monadic
import Network.AWS.S3.Internal.Types
