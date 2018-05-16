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
-- Module      : Network.AWS.AutoScaling.ExecutePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Executes the specified policy.
--
--
module Network.AWS.AutoScaling.ExecutePolicy
    (
    -- * Creating a Request
      executePolicy
    , ExecutePolicy
    -- * Request Lenses
    , epHonorCooldown
    , epMetricValue
    , epAutoScalingGroupName
    , epBreachThreshold
    , epPolicyName

    -- * Destructuring the Response
    , executePolicyResponse
    , ExecutePolicyResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'executePolicy' smart constructor.
data ExecutePolicy = ExecutePolicy'
  { _epHonorCooldown        :: !(Maybe Bool)
  , _epMetricValue          :: !(Maybe Double)
  , _epAutoScalingGroupName :: !(Maybe Text)
  , _epBreachThreshold      :: !(Maybe Double)
  , _epPolicyName           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExecutePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epHonorCooldown' - Indicates whether Auto Scaling waits for the cooldown period to complete before executing the policy. This parameter is not supported if the policy type is @StepScaling@ . For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/Cooldown.html Auto Scaling Cooldowns> in the /Auto Scaling User Guide/ .
--
-- * 'epMetricValue' - The metric value to compare to @BreachThreshold@ . This enables you to execute a policy of type @StepScaling@ and determine which step adjustment to use. For example, if the breach threshold is 50 and you want to use a step adjustment with a lower bound of 0 and an upper bound of 10, you can set the metric value to 59. If you specify a metric value that doesn't correspond to a step adjustment for the policy, the call returns an error. This parameter is required if the policy type is @StepScaling@ and not supported otherwise.
--
-- * 'epAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'epBreachThreshold' - The breach threshold for the alarm. This parameter is required if the policy type is @StepScaling@ and not supported otherwise.
--
-- * 'epPolicyName' - The name or ARN of the policy.
executePolicy
    :: Text -- ^ 'epPolicyName'
    -> ExecutePolicy
executePolicy pPolicyName_ =
  ExecutePolicy'
    { _epHonorCooldown = Nothing
    , _epMetricValue = Nothing
    , _epAutoScalingGroupName = Nothing
    , _epBreachThreshold = Nothing
    , _epPolicyName = pPolicyName_
    }


-- | Indicates whether Auto Scaling waits for the cooldown period to complete before executing the policy. This parameter is not supported if the policy type is @StepScaling@ . For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/Cooldown.html Auto Scaling Cooldowns> in the /Auto Scaling User Guide/ .
epHonorCooldown :: Lens' ExecutePolicy (Maybe Bool)
epHonorCooldown = lens _epHonorCooldown (\ s a -> s{_epHonorCooldown = a})

-- | The metric value to compare to @BreachThreshold@ . This enables you to execute a policy of type @StepScaling@ and determine which step adjustment to use. For example, if the breach threshold is 50 and you want to use a step adjustment with a lower bound of 0 and an upper bound of 10, you can set the metric value to 59. If you specify a metric value that doesn't correspond to a step adjustment for the policy, the call returns an error. This parameter is required if the policy type is @StepScaling@ and not supported otherwise.
epMetricValue :: Lens' ExecutePolicy (Maybe Double)
epMetricValue = lens _epMetricValue (\ s a -> s{_epMetricValue = a})

-- | The name of the Auto Scaling group.
epAutoScalingGroupName :: Lens' ExecutePolicy (Maybe Text)
epAutoScalingGroupName = lens _epAutoScalingGroupName (\ s a -> s{_epAutoScalingGroupName = a})

-- | The breach threshold for the alarm. This parameter is required if the policy type is @StepScaling@ and not supported otherwise.
epBreachThreshold :: Lens' ExecutePolicy (Maybe Double)
epBreachThreshold = lens _epBreachThreshold (\ s a -> s{_epBreachThreshold = a})

-- | The name or ARN of the policy.
epPolicyName :: Lens' ExecutePolicy Text
epPolicyName = lens _epPolicyName (\ s a -> s{_epPolicyName = a})

instance AWSRequest ExecutePolicy where
        type Rs ExecutePolicy = ExecutePolicyResponse
        request = postQuery autoScaling
        response = receiveNull ExecutePolicyResponse'

instance Hashable ExecutePolicy where

instance NFData ExecutePolicy where

instance ToHeaders ExecutePolicy where
        toHeaders = const mempty

instance ToPath ExecutePolicy where
        toPath = const "/"

instance ToQuery ExecutePolicy where
        toQuery ExecutePolicy'{..}
          = mconcat
              ["Action" =: ("ExecutePolicy" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "HonorCooldown" =: _epHonorCooldown,
               "MetricValue" =: _epMetricValue,
               "AutoScalingGroupName" =: _epAutoScalingGroupName,
               "BreachThreshold" =: _epBreachThreshold,
               "PolicyName" =: _epPolicyName]

-- | /See:/ 'executePolicyResponse' smart constructor.
data ExecutePolicyResponse =
  ExecutePolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExecutePolicyResponse' with the minimum fields required to make a request.
--
executePolicyResponse
    :: ExecutePolicyResponse
executePolicyResponse = ExecutePolicyResponse'


instance NFData ExecutePolicyResponse where
