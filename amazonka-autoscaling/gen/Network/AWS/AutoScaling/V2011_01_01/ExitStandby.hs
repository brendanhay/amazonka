{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.ExitStandby
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Move an instance out of Standby mode. To learn more about how to put
-- instances that are in a Standby mode back into service, see Auto Scaling
-- InService State.
-- https://autoscaling.amazonaws.com/?InstanceIds.member.1=i-5b73d709&AutoScalingGroupName=my-asg&Version=2011-01-01&Action=ExitStandby&SignatureVersion=2&SignatureMet
-- hod=HmacSHA256&Timestamp=2014-06-13T22%3A43%3A53.182Z&AUTHPARAMS
-- dca4efcf-eea6-4844-8064-cab1fecd1aa2 30 PreInService
-- 2014-06-13T22:43:53.523Z At 2014-06-13T22:43:53Z instance i-5b73d709 was
-- moved out of standby in response to a user request, increasing the capacity
-- from 3 to 4. my-asg {"Availability Zone":"us-east-1a"} Moving EC2 instance
-- out of Standby: i-5b73d709 321a11c8-f34c-11e3-a434-7f10009d5849.
module Network.AWS.AutoScaling.V2011_01_01.ExitStandby
    (
    -- * Request
      ExitStandby
    -- ** Request constructor
    , mkExitStandby
    -- ** Request lenses
    , es1InstanceIds
    , es1AutoScalingGroupName

    -- * Response
    , ExitStandbyResponse
    -- ** Response constructor
    , mkExitStandbyResponse
    -- ** Response lenses
    , esrrActivities
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | 
data ExitStandby = ExitStandby
    { _es1InstanceIds :: [Text]
    , _es1AutoScalingGroupName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ExitStandby' request.
mkExitStandby :: Text -- ^ 'es1AutoScalingGroupName'
              -> ExitStandby
mkExitStandby p2 = ExitStandby
    { _es1InstanceIds = mempty
    , _es1AutoScalingGroupName = p2
    }

-- | A list of instances to move out of Standby mode. You must specify at least
-- one instance ID.
es1InstanceIds :: Lens' ExitStandby [Text]
es1InstanceIds = lens _es1InstanceIds (\s a -> s { _es1InstanceIds = a })

-- | The name of the Auto Scaling group from which to move instances out of
-- Standby mode.
es1AutoScalingGroupName :: Lens' ExitStandby Text
es1AutoScalingGroupName =
    lens _es1AutoScalingGroupName
         (\s a -> s { _es1AutoScalingGroupName = a })

instance ToQuery ExitStandby where
    toQuery = genericQuery def

-- | The output of the ExitStandby action.
newtype ExitStandbyResponse = ExitStandbyResponse
    { _esrrActivities :: [Activity]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ExitStandbyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkExitStandbyResponse :: ExitStandbyResponse
mkExitStandbyResponse = ExitStandbyResponse
    { _esrrActivities = mempty
    }

-- | A list describing the activities related to moving instances out of Standby
-- mode.
esrrActivities :: Lens' ExitStandbyResponse [Activity]
esrrActivities = lens _esrrActivities (\s a -> s { _esrrActivities = a })

instance FromXML ExitStandbyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ExitStandby where
    type Sv ExitStandby = AutoScaling
    type Rs ExitStandby = ExitStandbyResponse

    request = post "ExitStandby"
    response _ = xmlResponse
