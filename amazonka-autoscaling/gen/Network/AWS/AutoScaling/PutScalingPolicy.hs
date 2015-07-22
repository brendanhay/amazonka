{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.PutScalingPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a policy for an Auto Scaling group. To update an
-- existing policy, use the existing policy name and set the parameters you
-- want to change. Any existing parameter not changed in an update to an
-- existing policy is not changed in this update request.
--
-- If you exceed your maximum limit of step adjustments, which by default
-- is 20 per region, the call fails. For information about updating this
-- limit, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html AWS Service Limits>
-- in the /Amazon Web Services General Reference/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_PutScalingPolicy.html>
module Network.AWS.AutoScaling.PutScalingPolicy
    (
    -- * Request
      PutScalingPolicy
    -- ** Request constructor
    , putScalingPolicy
    -- ** Request lenses
    , psprqEstimatedInstanceWarmup
    , psprqMinAdjustmentStep
    , psprqPolicyType
    , psprqStepAdjustments
    , psprqScalingAdjustment
    , psprqCooldown
    , psprqMetricAggregationType
    , psprqMinAdjustmentMagnitude
    , psprqAutoScalingGroupName
    , psprqPolicyName
    , psprqAdjustmentType

    -- * Response
    , PutScalingPolicyResponse
    -- ** Response constructor
    , putScalingPolicyResponse
    -- ** Response lenses
    , psprsPolicyARN
    , psprsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putScalingPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'psprqEstimatedInstanceWarmup'
--
-- * 'psprqMinAdjustmentStep'
--
-- * 'psprqPolicyType'
--
-- * 'psprqStepAdjustments'
--
-- * 'psprqScalingAdjustment'
--
-- * 'psprqCooldown'
--
-- * 'psprqMetricAggregationType'
--
-- * 'psprqMinAdjustmentMagnitude'
--
-- * 'psprqAutoScalingGroupName'
--
-- * 'psprqPolicyName'
--
-- * 'psprqAdjustmentType'
data PutScalingPolicy = PutScalingPolicy'
    { _psprqEstimatedInstanceWarmup :: !(Maybe Int)
    , _psprqMinAdjustmentStep       :: !(Maybe Int)
    , _psprqPolicyType              :: !(Maybe Text)
    , _psprqStepAdjustments         :: !(Maybe [StepAdjustment])
    , _psprqScalingAdjustment       :: !(Maybe Int)
    , _psprqCooldown                :: !(Maybe Int)
    , _psprqMetricAggregationType   :: !(Maybe Text)
    , _psprqMinAdjustmentMagnitude  :: !(Maybe Int)
    , _psprqAutoScalingGroupName    :: !Text
    , _psprqPolicyName              :: !Text
    , _psprqAdjustmentType          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutScalingPolicy' smart constructor.
putScalingPolicy :: Text -> Text -> Text -> PutScalingPolicy
putScalingPolicy pAutoScalingGroupName pPolicyName pAdjustmentType =
    PutScalingPolicy'
    { _psprqEstimatedInstanceWarmup = Nothing
    , _psprqMinAdjustmentStep = Nothing
    , _psprqPolicyType = Nothing
    , _psprqStepAdjustments = Nothing
    , _psprqScalingAdjustment = Nothing
    , _psprqCooldown = Nothing
    , _psprqMetricAggregationType = Nothing
    , _psprqMinAdjustmentMagnitude = Nothing
    , _psprqAutoScalingGroupName = pAutoScalingGroupName
    , _psprqPolicyName = pPolicyName
    , _psprqAdjustmentType = pAdjustmentType
    }

-- | The estimated time, in seconds, until a newly launched instance can
-- contribute to the CloudWatch metrics. The default is to use the value
-- specified for the default cooldown period for the group.
--
-- This parameter is not supported if the policy type is @SimpleScaling@.
psprqEstimatedInstanceWarmup :: Lens' PutScalingPolicy (Maybe Int)
psprqEstimatedInstanceWarmup = lens _psprqEstimatedInstanceWarmup (\ s a -> s{_psprqEstimatedInstanceWarmup = a});

-- | Available for backward compatibility. Use @MinAdjustmentMagnitude@
-- instead.
psprqMinAdjustmentStep :: Lens' PutScalingPolicy (Maybe Int)
psprqMinAdjustmentStep = lens _psprqMinAdjustmentStep (\ s a -> s{_psprqMinAdjustmentStep = a});

-- | The policy type. Valid values are @SimpleScaling@ and @StepScaling@. If
-- the policy type is null, the value is treated as @SimpleScaling@.
psprqPolicyType :: Lens' PutScalingPolicy (Maybe Text)
psprqPolicyType = lens _psprqPolicyType (\ s a -> s{_psprqPolicyType = a});

-- | A set of adjustments that enable you to scale based on the size of the
-- alarm breach.
--
-- This parameter is required if the policy type is @StepScaling@ and not
-- supported otherwise.
psprqStepAdjustments :: Lens' PutScalingPolicy [StepAdjustment]
psprqStepAdjustments = lens _psprqStepAdjustments (\ s a -> s{_psprqStepAdjustments = a}) . _Default;

-- | The amount by which to scale, based on the specified adjustment type. A
-- positive value adds to the current capacity while a negative number
-- removes from the current capacity.
--
-- This parameter is required if the policy type is @SimpleScaling@ and not
-- supported otherwise.
psprqScalingAdjustment :: Lens' PutScalingPolicy (Maybe Int)
psprqScalingAdjustment = lens _psprqScalingAdjustment (\ s a -> s{_psprqScalingAdjustment = a});

-- | The amount of time, in seconds, after a scaling activity completes and
-- before the next scaling activity can start. If this parameter is not
-- specified, the default cooldown period for the group applies.
--
-- This parameter is not supported unless the policy type is
-- @SimpleScaling@.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/Cooldown.html Understanding Auto Scaling Cooldowns>
-- in the /Auto Scaling Developer Guide/.
psprqCooldown :: Lens' PutScalingPolicy (Maybe Int)
psprqCooldown = lens _psprqCooldown (\ s a -> s{_psprqCooldown = a});

-- | The aggregation type for the CloudWatch metrics. Valid values are
-- @Minimum@, @Maximum@, and @Average@. If the aggregation type is null,
-- the value is treated as @Average@.
--
-- This parameter is not supported if the policy type is @SimpleScaling@.
psprqMetricAggregationType :: Lens' PutScalingPolicy (Maybe Text)
psprqMetricAggregationType = lens _psprqMetricAggregationType (\ s a -> s{_psprqMetricAggregationType = a});

-- | The minimum number of instances to scale. If the value of
-- @AdjustmentType@ is @PercentChangeInCapacity@, the scaling policy
-- changes the @DesiredCapacity@ of the Auto Scaling group by at least this
-- many instances. Otherwise, the error is @ValidationError@.
psprqMinAdjustmentMagnitude :: Lens' PutScalingPolicy (Maybe Int)
psprqMinAdjustmentMagnitude = lens _psprqMinAdjustmentMagnitude (\ s a -> s{_psprqMinAdjustmentMagnitude = a});

-- | The name or ARN of the group.
psprqAutoScalingGroupName :: Lens' PutScalingPolicy Text
psprqAutoScalingGroupName = lens _psprqAutoScalingGroupName (\ s a -> s{_psprqAutoScalingGroupName = a});

-- | The name of the policy.
psprqPolicyName :: Lens' PutScalingPolicy Text
psprqPolicyName = lens _psprqPolicyName (\ s a -> s{_psprqPolicyName = a});

-- | The adjustment type. Valid values are @ChangeInCapacity@,
-- @ExactCapacity@, and @PercentChangeInCapacity@.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/as-scale-based-on-demand.html Dynamic Scaling>
-- in the /Auto Scaling Developer Guide/.
psprqAdjustmentType :: Lens' PutScalingPolicy Text
psprqAdjustmentType = lens _psprqAdjustmentType (\ s a -> s{_psprqAdjustmentType = a});

instance AWSRequest PutScalingPolicy where
        type Sv PutScalingPolicy = AutoScaling
        type Rs PutScalingPolicy = PutScalingPolicyResponse
        request = post
        response
          = receiveXMLWrapper "PutScalingPolicyResult"
              (\ s h x ->
                 PutScalingPolicyResponse' <$>
                   (x .@? "PolicyARN") <*> (pure (fromEnum s)))

instance ToHeaders PutScalingPolicy where
        toHeaders = const mempty

instance ToPath PutScalingPolicy where
        toPath = const "/"

instance ToQuery PutScalingPolicy where
        toQuery PutScalingPolicy'{..}
          = mconcat
              ["Action" =: ("PutScalingPolicy" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "EstimatedInstanceWarmup" =:
                 _psprqEstimatedInstanceWarmup,
               "MinAdjustmentStep" =: _psprqMinAdjustmentStep,
               "PolicyType" =: _psprqPolicyType,
               "StepAdjustments" =:
                 toQuery
                   (toQueryList "member" <$> _psprqStepAdjustments),
               "ScalingAdjustment" =: _psprqScalingAdjustment,
               "Cooldown" =: _psprqCooldown,
               "MetricAggregationType" =:
                 _psprqMetricAggregationType,
               "MinAdjustmentMagnitude" =:
                 _psprqMinAdjustmentMagnitude,
               "AutoScalingGroupName" =: _psprqAutoScalingGroupName,
               "PolicyName" =: _psprqPolicyName,
               "AdjustmentType" =: _psprqAdjustmentType]

-- | /See:/ 'putScalingPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'psprsPolicyARN'
--
-- * 'psprsStatus'
data PutScalingPolicyResponse = PutScalingPolicyResponse'
    { _psprsPolicyARN :: !(Maybe Text)
    , _psprsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutScalingPolicyResponse' smart constructor.
putScalingPolicyResponse :: Int -> PutScalingPolicyResponse
putScalingPolicyResponse pStatus =
    PutScalingPolicyResponse'
    { _psprsPolicyARN = Nothing
    , _psprsStatus = pStatus
    }

-- | The Amazon Resource Name (ARN) of the policy.
psprsPolicyARN :: Lens' PutScalingPolicyResponse (Maybe Text)
psprsPolicyARN = lens _psprsPolicyARN (\ s a -> s{_psprsPolicyARN = a});

-- | FIXME: Undocumented member.
psprsStatus :: Lens' PutScalingPolicyResponse Int
psprsStatus = lens _psprsStatus (\ s a -> s{_psprsStatus = a});
