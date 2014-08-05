{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DeleteNotificationConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes notifications created by PutNotificationConfiguration.
module Network.AWS.AutoScaling.V2011_01_01.DeleteNotificationConfiguration where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

data DeleteNotificationConfiguration = DeleteNotificationConfiguration
    { _dnctTopicARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification
      -- Service (SNS) topic.
    , _dnctAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group.
    } deriving (Show, Generic)

makeLenses ''DeleteNotificationConfiguration

instance ToQuery DeleteNotificationConfiguration where
    toQuery = genericToQuery def

data DeleteNotificationConfigurationResponse = DeleteNotificationConfigurationResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteNotificationConfigurationResponse

instance AWSRequest DeleteNotificationConfiguration where
    type Sv DeleteNotificationConfiguration = AutoScaling
    type Rs DeleteNotificationConfiguration = DeleteNotificationConfigurationResponse

    request = post "DeleteNotificationConfiguration"
    response _ _ = return (Right DeleteNotificationConfigurationResponse)
