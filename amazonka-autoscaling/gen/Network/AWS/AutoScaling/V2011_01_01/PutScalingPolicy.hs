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
    , mkPutScalingPolicyType
    -- ** Request lenses
    , psptAutoScalingGroupName
    , psptPolicyName
    , psptScalingAdjustment
    , psptAdjustmentType
    , psptCooldown
    , psptMinAdjustmentStep

    -- * Response
    , PutScalingPolicyResponse
    -- ** Response lenses
    , parntPolicyARN
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutScalingPolicy' request.
mkPutScalingPolicyType :: Text -- ^ 'psptAutoScalingGroupName'
                       -> Text -- ^ 'psptPolicyName'
                       -> Integer -- ^ 'psptScalingAdjustment'
                       -> Text -- ^ 'psptAdjustmentType'
                       -> PutScalingPolicy
mkPutScalingPolicyType p1 p2 p3 p4 = PutScalingPolicy
    { _psptAutoScalingGroupName = p1
    , _psptPolicyName = p2
    , _psptScalingAdjustment = p3
    , _psptAdjustmentType = p4
    , _psptCooldown = Nothing
    , _psptMinAdjustmentStep = Nothing
    }
{-# INLINE mkPutScalingPolicyType #-}

data PutScalingPolicy = PutScalingPolicy
    { _psptAutoScalingGroupName :: Text
      -- ^ The name or ARN of the Auto Scaling group.
    , _psptPolicyName :: Text
      -- ^ The name of the policy you want to create or update.
    , _psptScalingAdjustment :: Integer
      -- ^ The number of instances by which to scale. AdjustmentType
      -- determines the interpretation of this number (e.g., as an
      -- absolute number or as a percentage of the existing Auto Scaling
      -- group size). A positive increment adds to the current capacity
      -- and a negative value removes from the current capacity.
    , _psptAdjustmentType :: Text
      -- ^ Specifies whether the ScalingAdjustment is an absolute number or
      -- a percentage of the current capacity. Valid values are
      -- ChangeInCapacity, ExactCapacity, and PercentChangeInCapacity. For
      -- more information about the adjustment types supported by Auto
      -- Scaling, see Scale Based on Demand.
    , _psptCooldown :: Maybe Integer
      -- ^ The amount of time, in seconds, after a scaling activity
      -- completes and before the next scaling activity can start. For
      -- more information, see Cooldown Period.
    , _psptMinAdjustmentStep :: Maybe Integer
      -- ^ Used with AdjustmentType with the value PercentChangeInCapacity,
      -- the scaling policy changes the DesiredCapacity of the Auto
      -- Scaling group by at least the number of instances specified in
      -- the value. You will get a ValidationError if you use
      -- MinAdjustmentStep on a policy with an AdjustmentType other than
      -- PercentChangeInCapacity.
    } deriving (Show, Generic)

-- | The name or ARN of the Auto Scaling group.
psptAutoScalingGroupName :: Lens' PutScalingPolicy (Text)
psptAutoScalingGroupName = lens _psptAutoScalingGroupName (\s a -> s { _psptAutoScalingGroupName = a })
{-# INLINE psptAutoScalingGroupName #-}

-- | The name of the policy you want to create or update.
psptPolicyName :: Lens' PutScalingPolicy (Text)
psptPolicyName = lens _psptPolicyName (\s a -> s { _psptPolicyName = a })
{-# INLINE psptPolicyName #-}

-- | The number of instances by which to scale. AdjustmentType determines the
-- interpretation of this number (e.g., as an absolute number or as a
-- percentage of the existing Auto Scaling group size). A positive increment
-- adds to the current capacity and a negative value removes from the current
-- capacity.
psptScalingAdjustment :: Lens' PutScalingPolicy (Integer)
psptScalingAdjustment = lens _psptScalingAdjustment (\s a -> s { _psptScalingAdjustment = a })
{-# INLINE psptScalingAdjustment #-}

-- | Specifies whether the ScalingAdjustment is an absolute number or a
-- percentage of the current capacity. Valid values are ChangeInCapacity,
-- ExactCapacity, and PercentChangeInCapacity. For more information about the
-- adjustment types supported by Auto Scaling, see Scale Based on Demand.
psptAdjustmentType :: Lens' PutScalingPolicy (Text)
psptAdjustmentType = lens _psptAdjustmentType (\s a -> s { _psptAdjustmentType = a })
{-# INLINE psptAdjustmentType #-}

-- | The amount of time, in seconds, after a scaling activity completes and
-- before the next scaling activity can start. For more information, see
-- Cooldown Period.
psptCooldown :: Lens' PutScalingPolicy (Maybe Integer)
psptCooldown = lens _psptCooldown (\s a -> s { _psptCooldown = a })
{-# INLINE psptCooldown #-}

-- | Used with AdjustmentType with the value PercentChangeInCapacity, the
-- scaling policy changes the DesiredCapacity of the Auto Scaling group by at
-- least the number of instances specified in the value. You will get a
-- ValidationError if you use MinAdjustmentStep on a policy with an
-- AdjustmentType other than PercentChangeInCapacity.
psptMinAdjustmentStep :: Lens' PutScalingPolicy (Maybe Integer)
psptMinAdjustmentStep = lens _psptMinAdjustmentStep (\s a -> s { _psptMinAdjustmentStep = a })
{-# INLINE psptMinAdjustmentStep #-}

instance ToQuery PutScalingPolicy where
    toQuery = genericQuery def

newtype PutScalingPolicyResponse = PutScalingPolicyResponse
    { _parntPolicyARN :: Maybe Text
      -- ^ A policy's Amazon Resource Name (ARN).
    } deriving (Show, Generic)

-- | A policy's Amazon Resource Name (ARN).
parntPolicyARN :: Lens' PutScalingPolicyResponse (Maybe Text)
parntPolicyARN = lens _parntPolicyARN (\s a -> s { _parntPolicyARN = a })
{-# INLINE parntPolicyARN #-}

instance FromXML PutScalingPolicyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest PutScalingPolicy where
    type Sv PutScalingPolicy = AutoScaling
    type Rs PutScalingPolicy = PutScalingPolicyResponse

    request = post "PutScalingPolicy"
    response _ = xmlResponse
