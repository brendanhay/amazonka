-- Module      : Network.AWS.Config
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Config is a fully managed service that provides you with an Amazon
-- resource inventory, configuration history, and configuration change
-- notifications to enable security and governance. With Amazon Config you can
-- discover existing Amazon resources, export a complete inventory of your
-- Amazon resources with all configuration details, and determine how a resource
-- was configured at any point in time. These capabilities enable compliance
-- auditing, security analysis, resource change tracking, and troubleshooting.
module Network.AWS.Config
    ( module Network.AWS.Config.DeleteDeliveryChannel
    , module Network.AWS.Config.DeliverConfigSnapshot
    , module Network.AWS.Config.DescribeConfigurationRecorderStatus
    , module Network.AWS.Config.DescribeConfigurationRecorders
    , module Network.AWS.Config.DescribeDeliveryChannelStatus
    , module Network.AWS.Config.DescribeDeliveryChannels
    , module Network.AWS.Config.GetResourceConfigHistory
    , module Network.AWS.Config.PutConfigurationRecorder
    , module Network.AWS.Config.PutDeliveryChannel
    , module Network.AWS.Config.StartConfigurationRecorder
    , module Network.AWS.Config.StopConfigurationRecorder
    , module Network.AWS.Config.Types
    ) where

import Network.AWS.Config.DeleteDeliveryChannel
import Network.AWS.Config.DeliverConfigSnapshot
import Network.AWS.Config.DescribeConfigurationRecorderStatus
import Network.AWS.Config.DescribeConfigurationRecorders
import Network.AWS.Config.DescribeDeliveryChannelStatus
import Network.AWS.Config.DescribeDeliveryChannels
import Network.AWS.Config.GetResourceConfigHistory
import Network.AWS.Config.PutConfigurationRecorder
import Network.AWS.Config.PutDeliveryChannel
import Network.AWS.Config.StartConfigurationRecorder
import Network.AWS.Config.StopConfigurationRecorder
import Network.AWS.Config.Types
