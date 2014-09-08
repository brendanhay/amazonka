{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.PutScalingPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates or updates a policy for an Auto Scaling group. To update an
-- existing policy, use the existing policy name and set the parameter(s) you
-- want to change. Any existing parameter not changed in an update to an
-- existing policy is not changed in this update request.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &ScalingAdjustment=30 &AdjustmentType=PercentChangeInCapacity
-- &PolicyName=my-scaleout-policy &Version=2011-01-01 &Action=PutScalingPolicy
-- &AUTHPARAMS
-- arn:aws:autoscaling:us-east-1:803981987763:scalingPolicy:b0dcf5e8
-- -02e6-4e31-9719-0675d0dc31ae:autoScalingGroupName/my-test-asg:policyName/my-scal
-- eout-policy 3cfc6fef-c08b-11e2-a697-2922EXAMPLE.
module Network.AWS.AutoScaling.V2011_01_01.PutScalingPolicy
    (
    -- * Request
      PutScalingPolicy
    -- ** Request constructor
    , mkPutScalingPolicy
    -- ** Request lenses
    , pspAutoScalingGroupName
    , pspPolicyName
    , pspScalingAdjustment
    , pspAdjustmentType
    , pspCooldown
    , pspMinAdjustmentStep

    -- * Response
    , PutScalingPolicyResponse
    -- ** Response lenses
    , psprPolicyARN
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | 
data PutScalingPolicy = PutScalingPolicy
    { _pspAutoScalingGroupName :: Text
    , _pspPolicyName :: Text
    , _pspScalingAdjustment :: Integer
    , _pspAdjustmentType :: Text
    , _pspCooldown :: Maybe Integer
    , _pspMinAdjustmentStep :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutScalingPolicy' request.
mkPutScalingPolicy :: Text -- ^ 'pspAutoScalingGroupName'
                   -> Text -- ^ 'pspPolicyName'
                   -> Integer -- ^ 'pspScalingAdjustment'
                   -> Text -- ^ 'pspAdjustmentType'
                   -> PutScalingPolicy
mkPutScalingPolicy p1 p2 p3 p4 = PutScalingPolicy
    { _pspAutoScalingGroupName = p1
    , _pspPolicyName = p2
    , _pspScalingAdjustment = p3
    , _pspAdjustmentType = p4
    , _pspCooldown = Nothing
    , _pspMinAdjustmentStep = Nothing
    }

-- | The name or ARN of the Auto Scaling group.
pspAutoScalingGroupName :: Lens' PutScalingPolicy Text
pspAutoScalingGroupName =
    lens _pspAutoScalingGroupName
         (\s a -> s { _pspAutoScalingGroupName = a })

-- | The name of the policy you want to create or update.
pspPolicyName :: Lens' PutScalingPolicy Text
pspPolicyName = lens _pspPolicyName (\s a -> s { _pspPolicyName = a })

-- | The number of instances by which to scale. AdjustmentType determines the
-- interpretation of this number (e.g., as an absolute number or as a
-- percentage of the existing Auto Scaling group size). A positive increment
-- adds to the current capacity and a negative value removes from the current
-- capacity.
pspScalingAdjustment :: Lens' PutScalingPolicy Integer
pspScalingAdjustment =
    lens _pspScalingAdjustment (\s a -> s { _pspScalingAdjustment = a })

-- | Specifies whether the ScalingAdjustment is an absolute number or a
-- percentage of the current capacity. Valid values are ChangeInCapacity,
-- ExactCapacity, and PercentChangeInCapacity. For more information about the
-- adjustment types supported by Auto Scaling, see Scale Based on Demand.
pspAdjustmentType :: Lens' PutScalingPolicy Text
pspAdjustmentType =
    lens _pspAdjustmentType (\s a -> s { _pspAdjustmentType = a })

-- | The amount of time, in seconds, after a scaling activity completes and
-- before the next scaling activity can start. For more information, see
-- Cooldown Period.
pspCooldown :: Lens' PutScalingPolicy (Maybe Integer)
pspCooldown = lens _pspCooldown (\s a -> s { _pspCooldown = a })

-- | Used with AdjustmentType with the value PercentChangeInCapacity, the
-- scaling policy changes the DesiredCapacity of the Auto Scaling group by at
-- least the number of instances specified in the value. You will get a
-- ValidationError if you use MinAdjustmentStep on a policy with an
-- AdjustmentType other than PercentChangeInCapacity.
pspMinAdjustmentStep :: Lens' PutScalingPolicy (Maybe Integer)
pspMinAdjustmentStep =
    lens _pspMinAdjustmentStep (\s a -> s { _pspMinAdjustmentStep = a })

instance ToQuery PutScalingPolicy where
    toQuery = genericQuery def

-- | The PolicyARNType data type.
newtype PutScalingPolicyResponse = PutScalingPolicyResponse
    { _psprPolicyARN :: Maybe Text
    } deriving (Show, Generic)

-- | A policy's Amazon Resource Name (ARN).
psprPolicyARN :: Lens' PutScalingPolicyResponse (Maybe Text)
psprPolicyARN = lens _psprPolicyARN (\s a -> s { _psprPolicyARN = a })

instance FromXML PutScalingPolicyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest PutScalingPolicy where
    type Sv PutScalingPolicy = AutoScaling
    type Rs PutScalingPolicy = PutScalingPolicyResponse

    request = post "PutScalingPolicy"
    response _ = xmlResponse
