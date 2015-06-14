{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.PutScalingPolicy
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

-- | Creates or updates a policy for an Auto Scaling group. To update an
-- existing policy, use the existing policy name and set the parameters you
-- want to change. Any existing parameter not changed in an update to an
-- existing policy is not changed in this update request.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_PutScalingPolicy.html>
module Network.AWS.AutoScaling.PutScalingPolicy
    (
    -- * Request
      PutScalingPolicy
    -- ** Request constructor
    , putScalingPolicy
    -- ** Request lenses
    , pspMinAdjustmentStep
    , pspCooldown
    , pspAutoScalingGroupName
    , pspPolicyName
    , pspScalingAdjustment
    , pspAdjustmentType

    -- * Response
    , PutScalingPolicyResponse
    -- ** Response constructor
    , putScalingPolicyResponse
    -- ** Response lenses
    , psprPolicyARN
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.AutoScaling.Types

-- | /See:/ 'putScalingPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pspMinAdjustmentStep'
--
-- * 'pspCooldown'
--
-- * 'pspAutoScalingGroupName'
--
-- * 'pspPolicyName'
--
-- * 'pspScalingAdjustment'
--
-- * 'pspAdjustmentType'
data PutScalingPolicy = PutScalingPolicy'{_pspMinAdjustmentStep :: Maybe Int, _pspCooldown :: Maybe Int, _pspAutoScalingGroupName :: Text, _pspPolicyName :: Text, _pspScalingAdjustment :: Int, _pspAdjustmentType :: Text} deriving (Eq, Read, Show)

-- | 'PutScalingPolicy' smart constructor.
putScalingPolicy :: Text -> Text -> Int -> Text -> PutScalingPolicy
putScalingPolicy pAutoScalingGroupName pPolicyName pScalingAdjustment pAdjustmentType = PutScalingPolicy'{_pspMinAdjustmentStep = Nothing, _pspCooldown = Nothing, _pspAutoScalingGroupName = pAutoScalingGroupName, _pspPolicyName = pPolicyName, _pspScalingAdjustment = pScalingAdjustment, _pspAdjustmentType = pAdjustmentType};

-- | Used with @AdjustmentType@ with the value @PercentChangeInCapacity@, the
-- scaling policy changes the @DesiredCapacity@ of the Auto Scaling group
-- by at least the number of instances specified in the value.
--
-- You will get a @ValidationError@ if you use @MinAdjustmentStep@ on a
-- policy with an @AdjustmentType@ other than @PercentChangeInCapacity@.
pspMinAdjustmentStep :: Lens' PutScalingPolicy (Maybe Int)
pspMinAdjustmentStep = lens _pspMinAdjustmentStep (\ s a -> s{_pspMinAdjustmentStep = a});

-- | The amount of time, in seconds, after a scaling activity completes and
-- before the next scaling activity can start.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/Cooldown.html Understanding Auto Scaling Cooldowns>
-- in the /Auto Scaling Developer Guide/.
pspCooldown :: Lens' PutScalingPolicy (Maybe Int)
pspCooldown = lens _pspCooldown (\ s a -> s{_pspCooldown = a});

-- | The name or ARN of the group.
pspAutoScalingGroupName :: Lens' PutScalingPolicy Text
pspAutoScalingGroupName = lens _pspAutoScalingGroupName (\ s a -> s{_pspAutoScalingGroupName = a});

-- | The name of the policy.
pspPolicyName :: Lens' PutScalingPolicy Text
pspPolicyName = lens _pspPolicyName (\ s a -> s{_pspPolicyName = a});

-- | The number of instances by which to scale. @AdjustmentType@ determines
-- the interpretation of this number (e.g., as an absolute number or as a
-- percentage of the existing Auto Scaling group size). A positive
-- increment adds to the current capacity and a negative value removes from
-- the current capacity.
pspScalingAdjustment :: Lens' PutScalingPolicy Int
pspScalingAdjustment = lens _pspScalingAdjustment (\ s a -> s{_pspScalingAdjustment = a});

-- | Specifies whether the @ScalingAdjustment@ is an absolute number or a
-- percentage of the current capacity. Valid values are @ChangeInCapacity@,
-- @ExactCapacity@, and @PercentChangeInCapacity@.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/as-scale-based-on-demand.html Dynamic Scaling>
-- in the /Auto Scaling Developer Guide/.
pspAdjustmentType :: Lens' PutScalingPolicy Text
pspAdjustmentType = lens _pspAdjustmentType (\ s a -> s{_pspAdjustmentType = a});

instance AWSRequest PutScalingPolicy where
        type Sv PutScalingPolicy = AutoScaling
        type Rs PutScalingPolicy = PutScalingPolicyResponse
        request = post
        response
          = receiveXMLWrapper "PutScalingPolicyResult"
              (\ s h x ->
                 PutScalingPolicyResponse' <$> x .@? "PolicyARN")

instance ToHeaders PutScalingPolicy where
        toHeaders = const mempty

instance ToPath PutScalingPolicy where
        toPath = const "/"

instance ToQuery PutScalingPolicy where
        toQuery PutScalingPolicy'{..}
          = mconcat
              ["Action" =: ("PutScalingPolicy" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "MinAdjustmentStep" =: _pspMinAdjustmentStep,
               "Cooldown" =: _pspCooldown,
               "AutoScalingGroupName" =: _pspAutoScalingGroupName,
               "PolicyName" =: _pspPolicyName,
               "ScalingAdjustment" =: _pspScalingAdjustment,
               "AdjustmentType" =: _pspAdjustmentType]

-- | /See:/ 'putScalingPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'psprPolicyARN'
newtype PutScalingPolicyResponse = PutScalingPolicyResponse'{_psprPolicyARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'PutScalingPolicyResponse' smart constructor.
putScalingPolicyResponse :: PutScalingPolicyResponse
putScalingPolicyResponse = PutScalingPolicyResponse'{_psprPolicyARN = Nothing};

-- | The Amazon Resource Name (ARN) of the policy.
psprPolicyARN :: Lens' PutScalingPolicyResponse (Maybe Text)
psprPolicyARN = lens _psprPolicyARN (\ s a -> s{_psprPolicyARN = a});
