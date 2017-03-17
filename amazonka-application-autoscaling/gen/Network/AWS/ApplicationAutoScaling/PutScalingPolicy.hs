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
-- Module      : Network.AWS.ApplicationAutoScaling.PutScalingPolicy
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a policy for an Application Auto Scaling scalable target.
--
--
-- Each scalable target is identified by a service namespace, resource ID, and scalable dimension. A scaling policy applies to the scalable target identified by those three attributes. You cannot create a scaling policy without first registering a scalable target using 'RegisterScalableTarget' .
--
-- To update a policy, specify its policy name and the parameters that you want to change. Any parameters that you don't specify are not changed by this update request.
--
-- You can view the scaling policies for a service namespace using 'DescribeScalingPolicies' . If you are no longer using a scaling policy, you can delete it using 'DeleteScalingPolicy' .
--
module Network.AWS.ApplicationAutoScaling.PutScalingPolicy
    (
    -- * Creating a Request
      putScalingPolicy
    , PutScalingPolicy
    -- * Request Lenses
    , pspPolicyType
    , pspStepScalingPolicyConfiguration
    , pspPolicyName
    , pspServiceNamespace
    , pspResourceId
    , pspScalableDimension

    -- * Destructuring the Response
    , putScalingPolicyResponse
    , PutScalingPolicyResponse
    -- * Response Lenses
    , psprsResponseStatus
    , psprsPolicyARN
    ) where

import           Network.AWS.ApplicationAutoScaling.Types
import           Network.AWS.ApplicationAutoScaling.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putScalingPolicy' smart constructor.
data PutScalingPolicy = PutScalingPolicy'
    { _pspPolicyType                     :: !(Maybe PolicyType)
    , _pspStepScalingPolicyConfiguration :: !(Maybe StepScalingPolicyConfiguration)
    , _pspPolicyName                     :: !Text
    , _pspServiceNamespace               :: !ServiceNamespace
    , _pspResourceId                     :: !Text
    , _pspScalableDimension              :: !ScalableDimension
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pspPolicyType' - The policy type. If you are creating a new policy, this parameter is required. If you are updating a policy, this parameter is not required.
--
-- * 'pspStepScalingPolicyConfiguration' - The configuration for the step scaling policy. If you are creating a new policy, this parameter is required. If you are updating a policy, this parameter is not required. For more information, see 'StepScalingPolicyConfiguration' and 'StepAdjustment' .
--
-- * 'pspPolicyName' - The name of the scaling policy.
--
-- * 'pspServiceNamespace' - The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
--
-- * 'pspResourceId' - The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .
--
-- * 'pspScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.
putScalingPolicy
    :: Text -- ^ 'pspPolicyName'
    -> ServiceNamespace -- ^ 'pspServiceNamespace'
    -> Text -- ^ 'pspResourceId'
    -> ScalableDimension -- ^ 'pspScalableDimension'
    -> PutScalingPolicy
putScalingPolicy pPolicyName_ pServiceNamespace_ pResourceId_ pScalableDimension_ =
    PutScalingPolicy'
    { _pspPolicyType = Nothing
    , _pspStepScalingPolicyConfiguration = Nothing
    , _pspPolicyName = pPolicyName_
    , _pspServiceNamespace = pServiceNamespace_
    , _pspResourceId = pResourceId_
    , _pspScalableDimension = pScalableDimension_
    }

-- | The policy type. If you are creating a new policy, this parameter is required. If you are updating a policy, this parameter is not required.
pspPolicyType :: Lens' PutScalingPolicy (Maybe PolicyType)
pspPolicyType = lens _pspPolicyType (\ s a -> s{_pspPolicyType = a});

-- | The configuration for the step scaling policy. If you are creating a new policy, this parameter is required. If you are updating a policy, this parameter is not required. For more information, see 'StepScalingPolicyConfiguration' and 'StepAdjustment' .
pspStepScalingPolicyConfiguration :: Lens' PutScalingPolicy (Maybe StepScalingPolicyConfiguration)
pspStepScalingPolicyConfiguration = lens _pspStepScalingPolicyConfiguration (\ s a -> s{_pspStepScalingPolicyConfiguration = a});

-- | The name of the scaling policy.
pspPolicyName :: Lens' PutScalingPolicy Text
pspPolicyName = lens _pspPolicyName (\ s a -> s{_pspPolicyName = a});

-- | The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
pspServiceNamespace :: Lens' PutScalingPolicy ServiceNamespace
pspServiceNamespace = lens _pspServiceNamespace (\ s a -> s{_pspServiceNamespace = a});

-- | The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .
pspResourceId :: Lens' PutScalingPolicy Text
pspResourceId = lens _pspResourceId (\ s a -> s{_pspResourceId = a});

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.
pspScalableDimension :: Lens' PutScalingPolicy ScalableDimension
pspScalableDimension = lens _pspScalableDimension (\ s a -> s{_pspScalableDimension = a});

instance AWSRequest PutScalingPolicy where
        type Rs PutScalingPolicy = PutScalingPolicyResponse
        request = postJSON applicationAutoScaling
        response
          = receiveJSON
              (\ s h x ->
                 PutScalingPolicyResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "PolicyARN"))

instance Hashable PutScalingPolicy

instance NFData PutScalingPolicy

instance ToHeaders PutScalingPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleFrontendService.PutScalingPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutScalingPolicy where
        toJSON PutScalingPolicy'{..}
          = object
              (catMaybes
                 [("PolicyType" .=) <$> _pspPolicyType,
                  ("StepScalingPolicyConfiguration" .=) <$>
                    _pspStepScalingPolicyConfiguration,
                  Just ("PolicyName" .= _pspPolicyName),
                  Just ("ServiceNamespace" .= _pspServiceNamespace),
                  Just ("ResourceId" .= _pspResourceId),
                  Just ("ScalableDimension" .= _pspScalableDimension)])

instance ToPath PutScalingPolicy where
        toPath = const "/"

instance ToQuery PutScalingPolicy where
        toQuery = const mempty

-- | /See:/ 'putScalingPolicyResponse' smart constructor.
data PutScalingPolicyResponse = PutScalingPolicyResponse'
    { _psprsResponseStatus :: !Int
    , _psprsPolicyARN      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutScalingPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psprsResponseStatus' - -- | The response status code.
--
-- * 'psprsPolicyARN' - The Amazon Resource Name (ARN) of the resulting scaling policy.
putScalingPolicyResponse
    :: Int -- ^ 'psprsResponseStatus'
    -> Text -- ^ 'psprsPolicyARN'
    -> PutScalingPolicyResponse
putScalingPolicyResponse pResponseStatus_ pPolicyARN_ =
    PutScalingPolicyResponse'
    { _psprsResponseStatus = pResponseStatus_
    , _psprsPolicyARN = pPolicyARN_
    }

-- | -- | The response status code.
psprsResponseStatus :: Lens' PutScalingPolicyResponse Int
psprsResponseStatus = lens _psprsResponseStatus (\ s a -> s{_psprsResponseStatus = a});

-- | The Amazon Resource Name (ARN) of the resulting scaling policy.
psprsPolicyARN :: Lens' PutScalingPolicyResponse Text
psprsPolicyARN = lens _psprsPolicyARN (\ s a -> s{_psprsPolicyARN = a});

instance NFData PutScalingPolicyResponse
