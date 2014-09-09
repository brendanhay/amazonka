-- Module      : Network.AWS.Route53.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Current version of the Route53 service, monadic version.
module Network.AWS.Route53.Trans
    ( module Network.AWS.Route53.V2013_04_01.Trans
    , module Network.AWS.Route53.Internal.Types
    ) where

import Network.AWS.Route53.V2013_04_01.Trans
import Network.AWS.Route53.Internal.Types
