{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.AutoScaling.PutScalingPolicy
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
module Network.AWS.AutoScaling.PutScalingPolicy
    (
    -- * Request
      PutScalingPolicyType
    -- ** Request constructor
    , putScalingPolicyType
    -- ** Request lenses
    , psptAdjustmentType
    , psptAutoScalingGroupName
    , psptCooldown
    , psptMinAdjustmentStep
    , psptPolicyName
    , psptScalingAdjustment

    -- * Response
    , PolicyARNType
    -- ** Response constructor
    , policyARNType
    -- ** Response lenses
    , parntPolicyARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data PutScalingPolicyType = PutScalingPolicyType
    { _psptAdjustmentType       :: Text
    , _psptAutoScalingGroupName :: Text
    , _psptCooldown             :: Maybe Int
    , _psptMinAdjustmentStep    :: Maybe Int
    , _psptPolicyName           :: Text
    , _psptScalingAdjustment    :: Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'PutScalingPolicyType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'psptAdjustmentType' @::@ 'Text'
--
-- * 'psptAutoScalingGroupName' @::@ 'Text'
--
-- * 'psptCooldown' @::@ 'Maybe' 'Int'
--
-- * 'psptMinAdjustmentStep' @::@ 'Maybe' 'Int'
--
-- * 'psptPolicyName' @::@ 'Text'
--
-- * 'psptScalingAdjustment' @::@ 'Int'
--
putScalingPolicyType :: Text -- ^ 'psptAutoScalingGroupName'
                     -> Text -- ^ 'psptPolicyName'
                     -> Int -- ^ 'psptScalingAdjustment'
                     -> Text -- ^ 'psptAdjustmentType'
                     -> PutScalingPolicyType
putScalingPolicyType p1 p2 p3 p4 = PutScalingPolicyType
    { _psptAutoScalingGroupName = p1
    , _psptPolicyName           = p2
    , _psptScalingAdjustment    = p3
    , _psptAdjustmentType       = p4
    , _psptCooldown             = Nothing
    , _psptMinAdjustmentStep    = Nothing
    }

-- | Specifies whether the ScalingAdjustment is an absolute number or a
-- percentage of the current capacity. Valid values are ChangeInCapacity,
-- ExactCapacity, and PercentChangeInCapacity. For more information about
-- the adjustment types supported by Auto Scaling, see Scale Based on
-- Demand.
psptAdjustmentType :: Lens' PutScalingPolicyType Text
psptAdjustmentType =
    lens _psptAdjustmentType (\s a -> s { _psptAdjustmentType = a })

-- | The name or ARN of the Auto Scaling group.
psptAutoScalingGroupName :: Lens' PutScalingPolicyType Text
psptAutoScalingGroupName =
    lens _psptAutoScalingGroupName
        (\s a -> s { _psptAutoScalingGroupName = a })

-- | The amount of time, in seconds, after a scaling activity completes and
-- before the next scaling activity can start. For more information, see
-- Cooldown Period.
psptCooldown :: Lens' PutScalingPolicyType (Maybe Int)
psptCooldown = lens _psptCooldown (\s a -> s { _psptCooldown = a })

-- | Used with AdjustmentType with the value PercentChangeInCapacity, the
-- scaling policy changes the DesiredCapacity of the Auto Scaling group by
-- at least the number of instances specified in the value. You will get a
-- ValidationError if you use MinAdjustmentStep on a policy with an
-- AdjustmentType other than PercentChangeInCapacity.
psptMinAdjustmentStep :: Lens' PutScalingPolicyType (Maybe Int)
psptMinAdjustmentStep =
    lens _psptMinAdjustmentStep (\s a -> s { _psptMinAdjustmentStep = a })

-- | The name of the policy you want to create or update.
psptPolicyName :: Lens' PutScalingPolicyType Text
psptPolicyName = lens _psptPolicyName (\s a -> s { _psptPolicyName = a })

-- | The number of instances by which to scale. AdjustmentType determines the
-- interpretation of this number (e.g., as an absolute number or as a
-- percentage of the existing Auto Scaling group size). A positive increment
-- adds to the current capacity and a negative value removes from the
-- current capacity.
psptScalingAdjustment :: Lens' PutScalingPolicyType Int
psptScalingAdjustment =
    lens _psptScalingAdjustment (\s a -> s { _psptScalingAdjustment = a })

instance ToPath PutScalingPolicyType where
    toPath = const "/"

instance ToQuery PutScalingPolicyType

newtype PolicyARNType = PolicyARNType
    { _parntPolicyARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'PolicyARNType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'parntPolicyARN' @::@ 'Maybe' 'Text'
--
policyARNType :: PolicyARNType
policyARNType = PolicyARNType
    { _parntPolicyARN = Nothing
    }

-- | A policy's Amazon Resource Name (ARN).
parntPolicyARN :: Lens' PolicyARNType (Maybe Text)
parntPolicyARN = lens _parntPolicyARN (\s a -> s { _parntPolicyARN = a })

instance AWSRequest PutScalingPolicyType where
    type Sv PutScalingPolicyType = AutoScaling
    type Rs PutScalingPolicyType = PolicyARNType

    request  = post "PutScalingPolicy"
    response = const . xmlResponse $ \h x -> PolicyARNType
newtype
