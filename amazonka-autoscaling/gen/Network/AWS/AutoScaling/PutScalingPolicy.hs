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
-- Copyright   : (c) 2013-2016 Brendan Hay
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
module Network.AWS.AutoScaling.PutScalingPolicy
    (
    -- * Creating a Request
      putScalingPolicy
    , PutScalingPolicy
    -- * Request Lenses
    , pspMinAdjustmentStep
    , pspEstimatedInstanceWarmup
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
    , putScalingPolicyResponse
    , PutScalingPolicyResponse
    -- * Response Lenses
    , psprsPolicyARN
    , psprsResponseStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putScalingPolicy' smart constructor.
data PutScalingPolicy = PutScalingPolicy'
    { _pspMinAdjustmentStep       :: !(Maybe Int)
    , _pspEstimatedInstanceWarmup :: !(Maybe Int)
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

-- | Creates a value of 'PutScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pspMinAdjustmentStep'
--
-- * 'pspEstimatedInstanceWarmup'
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
putScalingPolicy
    :: Text -- ^ 'pspAutoScalingGroupName'
    -> Text -- ^ 'pspPolicyName'
    -> Text -- ^ 'pspAdjustmentType'
    -> PutScalingPolicy
putScalingPolicy pAutoScalingGroupName_ pPolicyName_ pAdjustmentType_ =
    PutScalingPolicy'
    { _pspMinAdjustmentStep = Nothing
    , _pspEstimatedInstanceWarmup = Nothing
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

-- | Available for backward compatibility. Use 'MinAdjustmentMagnitude'
-- instead.
pspMinAdjustmentStep :: Lens' PutScalingPolicy (Maybe Int)
pspMinAdjustmentStep = lens _pspMinAdjustmentStep (\ s a -> s{_pspMinAdjustmentStep = a});

-- | The estimated time, in seconds, until a newly launched instance can
-- contribute to the CloudWatch metrics. The default is to use the value
-- specified for the default cooldown period for the group.
--
-- This parameter is not supported if the policy type is 'SimpleScaling'.
pspEstimatedInstanceWarmup :: Lens' PutScalingPolicy (Maybe Int)
pspEstimatedInstanceWarmup = lens _pspEstimatedInstanceWarmup (\ s a -> s{_pspEstimatedInstanceWarmup = a});

-- | The policy type. Valid values are 'SimpleScaling' and 'StepScaling'. If
-- the policy type is null, the value is treated as 'SimpleScaling'.
pspPolicyType :: Lens' PutScalingPolicy (Maybe Text)
pspPolicyType = lens _pspPolicyType (\ s a -> s{_pspPolicyType = a});

-- | A set of adjustments that enable you to scale based on the size of the
-- alarm breach.
--
-- This parameter is required if the policy type is 'StepScaling' and not
-- supported otherwise.
pspStepAdjustments :: Lens' PutScalingPolicy [StepAdjustment]
pspStepAdjustments = lens _pspStepAdjustments (\ s a -> s{_pspStepAdjustments = a}) . _Default . _Coerce;

-- | The amount by which to scale, based on the specified adjustment type. A
-- positive value adds to the current capacity while a negative number
-- removes from the current capacity.
--
-- This parameter is required if the policy type is 'SimpleScaling' and not
-- supported otherwise.
pspScalingAdjustment :: Lens' PutScalingPolicy (Maybe Int)
pspScalingAdjustment = lens _pspScalingAdjustment (\ s a -> s{_pspScalingAdjustment = a});

-- | The amount of time, in seconds, after a scaling activity completes and
-- before the next scaling activity can start. If this parameter is not
-- specified, the default cooldown period for the group applies.
--
-- This parameter is not supported unless the policy type is
-- 'SimpleScaling'.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/Cooldown.html Auto Scaling Cooldowns>
-- in the /Auto Scaling Developer Guide/.
pspCooldown :: Lens' PutScalingPolicy (Maybe Int)
pspCooldown = lens _pspCooldown (\ s a -> s{_pspCooldown = a});

-- | The aggregation type for the CloudWatch metrics. Valid values are
-- 'Minimum', 'Maximum', and 'Average'. If the aggregation type is null,
-- the value is treated as 'Average'.
--
-- This parameter is not supported if the policy type is 'SimpleScaling'.
pspMetricAggregationType :: Lens' PutScalingPolicy (Maybe Text)
pspMetricAggregationType = lens _pspMetricAggregationType (\ s a -> s{_pspMetricAggregationType = a});

-- | The minimum number of instances to scale. If the value of
-- 'AdjustmentType' is 'PercentChangeInCapacity', the scaling policy
-- changes the 'DesiredCapacity' of the Auto Scaling group by at least this
-- many instances. Otherwise, the error is 'ValidationError'.
pspMinAdjustmentMagnitude :: Lens' PutScalingPolicy (Maybe Int)
pspMinAdjustmentMagnitude = lens _pspMinAdjustmentMagnitude (\ s a -> s{_pspMinAdjustmentMagnitude = a});

-- | The name or ARN of the group.
pspAutoScalingGroupName :: Lens' PutScalingPolicy Text
pspAutoScalingGroupName = lens _pspAutoScalingGroupName (\ s a -> s{_pspAutoScalingGroupName = a});

-- | The name of the policy.
pspPolicyName :: Lens' PutScalingPolicy Text
pspPolicyName = lens _pspPolicyName (\ s a -> s{_pspPolicyName = a});

-- | The adjustment type. Valid values are 'ChangeInCapacity',
-- 'ExactCapacity', and 'PercentChangeInCapacity'.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/as-scale-based-on-demand.html Dynamic Scaling>
-- in the /Auto Scaling Developer Guide/.
pspAdjustmentType :: Lens' PutScalingPolicy Text
pspAdjustmentType = lens _pspAdjustmentType (\ s a -> s{_pspAdjustmentType = a});

instance AWSRequest PutScalingPolicy where
        type Rs PutScalingPolicy = PutScalingPolicyResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper "PutScalingPolicyResult"
              (\ s h x ->
                 PutScalingPolicyResponse' <$>
                   (x .@? "PolicyARN") <*> (pure (fromEnum s)))

instance Hashable PutScalingPolicy

instance NFData PutScalingPolicy

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
               "EstimatedInstanceWarmup" =:
                 _pspEstimatedInstanceWarmup,
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
data PutScalingPolicyResponse = PutScalingPolicyResponse'
    { _psprsPolicyARN      :: !(Maybe Text)
    , _psprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutScalingPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psprsPolicyARN'
--
-- * 'psprsResponseStatus'
putScalingPolicyResponse
    :: Int -- ^ 'psprsResponseStatus'
    -> PutScalingPolicyResponse
putScalingPolicyResponse pResponseStatus_ =
    PutScalingPolicyResponse'
    { _psprsPolicyARN = Nothing
    , _psprsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the policy.
psprsPolicyARN :: Lens' PutScalingPolicyResponse (Maybe Text)
psprsPolicyARN = lens _psprsPolicyARN (\ s a -> s{_psprsPolicyARN = a});

-- | The response status code.
psprsResponseStatus :: Lens' PutScalingPolicyResponse Int
psprsResponseStatus = lens _psprsResponseStatus (\ s a -> s{_psprsResponseStatus = a});
