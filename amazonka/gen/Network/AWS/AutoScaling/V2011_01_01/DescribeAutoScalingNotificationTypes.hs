{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingNotificationTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of all notification types that are supported by Auto
-- Scaling.
module Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingNotificationTypes where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

data DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes
    deriving (Eq, Show, Generic)

makeLenses ''DescribeAutoScalingNotificationTypes

instance ToQuery DescribeAutoScalingNotificationTypes where
    toQuery = genericToQuery def

data DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse
    { _dasntaAutoScalingNotificationTypes :: [Text]
      -- ^ Returns a list of all notification types supported by Auto
      -- Scaling. They are: autoscaling:EC2_INSTANCE_LAUNCH
      -- autoscaling:EC2_INSTANCE_LAUNCH_ERROR
      -- autoscaling:EC2_INSTANCE_TERMINATE
      -- autoscaling:EC2_INSTANCE_TERMINATE_ERROR
      -- autoscaling:TEST_NOTIFICATION
      -- https://autoscaling.amazonaws.com/?Version=2011-01-01&Action=DescribeAutoScalingNotificationTypes
      -- &AUTHPARAMS autoscaling:EC2_INSTANCE_LAUNCH
      -- autoscaling:EC2_INSTANCE_LAUNCH_ERROR
      -- autoscaling:EC2_INSTANCE_TERMINATE
      -- autoscaling:EC2_INSTANCE_TERMINATE_ERROR
      -- autoscaling:TEST_NOTIFICATION
      -- 42fc6794-bf21-11e2-a1cf-ff3dEXAMPLE.
    } deriving (Show, Generic)

makeLenses ''DescribeAutoScalingNotificationTypesResponse

instance AWSRequest DescribeAutoScalingNotificationTypes where
    type Sv DescribeAutoScalingNotificationTypes = AutoScaling
    type Rs DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypesResponse

    request = post "DescribeAutoScalingNotificationTypes"
    response _ = cursorResponse $ \hs xml ->
        pure DescribeAutoScalingNotificationTypesResponse
            <*> xml %| "AutoScalingNotificationTypes"
