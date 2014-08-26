{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.EnterStandby
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Move instances in an Auto Scaling group into a Standby mode. To learn more
-- about how to put instances into a Standby mode, see Auto Scaling InService
-- State.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-asg&ShouldDecrementDesiredCapacity=true&InstanceIds.member.1=i-5b73d709&Version=2011-01-01&Action=
-- 
-- EnterStandby&SignatureVersion=2&SignatureMethod=HmacSHA256&Timestamp=2014-06-13T22%3A35%3A50.567Z&AUTHPARAMS
-- 462b4bc3-ad3b-4e67-a58d-96cd00f02f9e 50 InProgress 2014-06-13T22:35:50.884Z
-- At 2014-06-13T22:35:50Z instance i-5b73d709 was moved to standby in
-- response to a user request, shrinking the capacity from 4 to 3. my-asg
-- {"Availability Zone":"us-east-1a"} Moving EC2 instance to Standby:
-- i-5b73d709 126f2f31-f34b-11e3-bc51-b35178f0274f.
module Network.AWS.AutoScaling.V2011_01_01.EnterStandby where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'EnterStandby' request.
enterStandby :: Text -- ^ '_esqAutoScalingGroupName'
             -> Bool -- ^ '_esqShouldDecrementDesiredCapacity'
             -> EnterStandby
enterStandby p1 p2 = EnterStandby
    { _esqAutoScalingGroupName = p1
    , _esqShouldDecrementDesiredCapacity = p2
    , _esqInstanceIds = mempty
    }

data EnterStandby = EnterStandby
    { _esqAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group from which to move instances
      -- into Standby mode.
    , _esqShouldDecrementDesiredCapacity :: Bool
      -- ^ Specifies whether the instances moved to Standby mode count as
      -- part of the Auto Scaling group's desired capacity. If set, the
      -- desired capacity for the Auto Scaling group decrements by the
      -- number of instances moved to Standby mode.
    , _esqInstanceIds :: [Text]
      -- ^ The instances to move into Standby mode. You must specify at
      -- least one instance ID.
    } deriving (Show, Generic)

makeLenses ''EnterStandby

instance ToQuery EnterStandby where
    toQuery = genericQuery def

data EnterStandbyResponse = EnterStandbyResponse
    { _esaActivities :: [Activity]
      -- ^ A list describing the activities related to moving instances into
      -- Standby mode.
    } deriving (Show, Generic)

makeLenses ''EnterStandbyResponse

instance FromXML EnterStandbyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest EnterStandby where
    type Sv EnterStandby = AutoScaling
    type Rs EnterStandby = EnterStandbyResponse

    request = post "EnterStandby"
    response _ = xmlResponse
