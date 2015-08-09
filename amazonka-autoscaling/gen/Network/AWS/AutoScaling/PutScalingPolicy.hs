{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.PutScalingPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_PutScalingPolicy.html AWS API Reference> for PutScalingPolicy.
module Network.AWS.AutoScaling.PutScalingPolicy
    (
    -- * Creating a Request
      PutScalingPolicy
    , putScalingPolicy
    -- * Request Lenses
    , pspEstimatedInstanceWarmup
    , pspMinAdjustmentStep
    , pspPolicyType
    , pspStepAdjustments
    , pspScalingAdjustment
    , pspCooldown
    , pspMetricAggregationType
    , pspMinAdjustmentMagnitude
    , pspAutoScalingGroupName
    , pspPolicyName
    , pspAdjustmentType

    -- * Destructuring the Response
    , PutScalingPolicyResponse
    , putScalingPolicyResponse
    -- * Response Lenses
    , psprsPolicyARN
    , psprsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putScalingPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pspEstimatedInstanceWarmup'
--
-- * 'pspMinAdjustmentStep'
--
-- * 'pspPolicyType'
--
-- * 'pspStepAdjustments'
--
-- * 'pspScalingAdjustment'
--
-- * 'pspCooldown'
--
-- * 'pspMetricAggregationType'
--
-- * 'pspMinAdjustmentMagnitude'
--
-- * 'pspAutoScalingGroupName'
--
-- * 'pspPolicyName'
--
-- * 'pspAdjustmentType'
data PutScalingPolicy = PutScalingPolicy'
    { _pspEstimatedInstanceWarmup :: !(Maybe Int)
    , _pspMinAdjustmentStep       :: !(Maybe Int)
    , _pspPolicyType              :: !(Maybe Text)
    , _pspStepAdjustments         :: !(Maybe [StepAdjustment])
    , _pspScalingAdjustment       :: !(Maybe Int)
    , _pspCooldown                :: !(Maybe Int)
    , _pspMetricAggregationType   :: !(Maybe Text)
    , _pspMinAdjustmentMagnitude  :: !(Maybe Int)
    , _pspAutoScalingGroupName    :: !Text
    , _pspPolicyName              :: !Text
    , _pspAdjustmentType          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutScalingPolicy' smart constructor.
putScalingPolicy :: Text -> Text -> Text -> PutScalingPolicy
putScalingPolicy pAutoScalingGroupName_ pPolicyName_ pAdjustmentType_ =
    PutScalingPolicy'
    { _pspEstimatedInstanceWarmup = Nothing
    , _pspMinAdjustmentStep = Nothing
    , _pspPolicyType = Nothing
    , _pspStepAdjustments = Nothing
    , _pspScalingAdjustment = Nothing
    , _pspCooldown = Nothing
    , _pspMetricAggregationType = Nothing
    , _pspMinAdjustmentMagnitude = Nothing
    , _pspAutoScalingGroupName = pAutoScalingGroupName_
    , _pspPolicyName = pPolicyName_
    , _pspAdjustmentType = pAdjustmentType_
    }

-- | The estimated time, in seconds, until a newly launched instance can
-- contribute to the CloudWatch metrics. The default is to use the value
-- specified for the default cooldown period for the group.
--
-- This parameter is not supported if the policy type is @SimpleScaling@.
pspEstimatedInstanceWarmup :: Lens' PutScalingPolicy (Maybe Int)
pspEstimatedInstanceWarmup = lens _pspEstimatedInstanceWarmup (\ s a -> s{_pspEstimatedInstanceWarmup = a});

-- | Available for backward compatibility. Use @MinAdjustmentMagnitude@
-- instead.
pspMinAdjustmentStep :: Lens' PutScalingPolicy (Maybe Int)
pspMinAdjustmentStep = lens _pspMinAdjustmentStep (\ s a -> s{_pspMinAdjustmentStep = a});

-- | The policy type. Valid values are @SimpleScaling@ and @StepScaling@. If
-- the policy type is null, the value is treated as @SimpleScaling@.
pspPolicyType :: Lens' PutScalingPolicy (Maybe Text)
pspPolicyType = lens _pspPolicyType (\ s a -> s{_pspPolicyType = a});

-- | A set of adjustments that enable you to scale based on the size of the
-- alarm breach.
--
-- This parameter is required if the policy type is @StepScaling@ and not
-- supported otherwise.
pspStepAdjustments :: Lens' PutScalingPolicy [StepAdjustment]
pspStepAdjustments = lens _pspStepAdjustments (\ s a -> s{_pspStepAdjustments = a}) . _Default . _Coerce;

-- | The amount by which to scale, based on the specified adjustment type. A
-- positive value adds to the current capacity while a negative number
-- removes from the current capacity.
--
-- This parameter is required if the policy type is @SimpleScaling@ and not
-- supported otherwise.
pspScalingAdjustment :: Lens' PutScalingPolicy (Maybe Int)
pspScalingAdjustment = lens _pspScalingAdjustment (\ s a -> s{_pspScalingAdjustment = a});

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
pspCooldown :: Lens' PutScalingPolicy (Maybe Int)
pspCooldown = lens _pspCooldown (\ s a -> s{_pspCooldown = a});

-- | The aggregation type for the CloudWatch metrics. Valid values are
-- @Minimum@, @Maximum@, and @Average@. If the aggregation type is null,
-- the value is treated as @Average@.
--
-- This parameter is not supported if the policy type is @SimpleScaling@.
pspMetricAggregationType :: Lens' PutScalingPolicy (Maybe Text)
pspMetricAggregationType = lens _pspMetricAggregationType (\ s a -> s{_pspMetricAggregationType = a});

-- | The minimum number of instances to scale. If the value of
-- @AdjustmentType@ is @PercentChangeInCapacity@, the scaling policy
-- changes the @DesiredCapacity@ of the Auto Scaling group by at least this
-- many instances. Otherwise, the error is @ValidationError@.
pspMinAdjustmentMagnitude :: Lens' PutScalingPolicy (Maybe Int)
pspMinAdjustmentMagnitude = lens _pspMinAdjustmentMagnitude (\ s a -> s{_pspMinAdjustmentMagnitude = a});

-- | The name or ARN of the group.
pspAutoScalingGroupName :: Lens' PutScalingPolicy Text
pspAutoScalingGroupName = lens _pspAutoScalingGroupName (\ s a -> s{_pspAutoScalingGroupName = a});

-- | The name of the policy.
pspPolicyName :: Lens' PutScalingPolicy Text
pspPolicyName = lens _pspPolicyName (\ s a -> s{_pspPolicyName = a});

-- | The adjustment type. Valid values are @ChangeInCapacity@,
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
        request = postQuery
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
                 _pspEstimatedInstanceWarmup,
               "MinAdjustmentStep" =: _pspMinAdjustmentStep,
               "PolicyType" =: _pspPolicyType,
               "StepAdjustments" =:
                 toQuery
                   (toQueryList "member" <$> _pspStepAdjustments),
               "ScalingAdjustment" =: _pspScalingAdjustment,
               "Cooldown" =: _pspCooldown,
               "MetricAggregationType" =: _pspMetricAggregationType,
               "MinAdjustmentMagnitude" =:
                 _pspMinAdjustmentMagnitude,
               "AutoScalingGroupName" =: _pspAutoScalingGroupName,
               "PolicyName" =: _pspPolicyName,
               "AdjustmentType" =: _pspAdjustmentType]

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
putScalingPolicyResponse pStatus_ =
    PutScalingPolicyResponse'
    { _psprsPolicyARN = Nothing
    , _psprsStatus = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the policy.
psprsPolicyARN :: Lens' PutScalingPolicyResponse (Maybe Text)
psprsPolicyARN = lens _psprsPolicyARN (\ s a -> s{_psprsPolicyARN = a});

-- | Undocumented member.
psprsStatus :: Lens' PutScalingPolicyResponse Int
psprsStatus = lens _psprsStatus (\ s a -> s{_psprsStatus = a});
