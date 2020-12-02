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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a policy for an Application Auto Scaling scalable target.
--
--
-- Each scalable target is identified by a service namespace, resource ID, and scalable dimension. A scaling policy applies to the scalable target identified by those three attributes. You cannot create a scaling policy until you register the scalable target using 'RegisterScalableTarget' .
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
    , pspTargetTrackingScalingPolicyConfiguration
    , pspStepScalingPolicyConfiguration
    , pspPolicyName
    , pspServiceNamespace
    , pspResourceId
    , pspScalableDimension

    -- * Destructuring the Response
    , putScalingPolicyResponse
    , PutScalingPolicyResponse
    -- * Response Lenses
    , psprsAlarms
    , psprsResponseStatus
    , psprsPolicyARN
    ) where

import Network.AWS.ApplicationAutoScaling.Types
import Network.AWS.ApplicationAutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putScalingPolicy' smart constructor.
data PutScalingPolicy = PutScalingPolicy'
  { _pspPolicyType :: !(Maybe PolicyType)
  , _pspTargetTrackingScalingPolicyConfiguration :: !(Maybe TargetTrackingScalingPolicyConfiguration)
  , _pspStepScalingPolicyConfiguration :: !(Maybe StepScalingPolicyConfiguration)
  , _pspPolicyName :: !Text
  , _pspServiceNamespace :: !ServiceNamespace
  , _pspResourceId :: !Text
  , _pspScalableDimension :: !ScalableDimension
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pspPolicyType' - The policy type. This parameter is required if you are creating a policy. For DynamoDB, only @TargetTrackingScaling@ is supported. For Amazon ECS, Spot Fleet, and Amazon RDS, both @StepScaling@ and @TargetTrackingScaling@ are supported. For any other service, only @StepScaling@ is supported.
--
-- * 'pspTargetTrackingScalingPolicyConfiguration' - A target tracking policy. This parameter is required if you are creating a policy and the policy type is @TargetTrackingScaling@ .
--
-- * 'pspStepScalingPolicyConfiguration' - A step scaling policy. This parameter is required if you are creating a policy and the policy type is @StepScaling@ .
--
-- * 'pspPolicyName' - The name of the scaling policy.
--
-- * 'pspServiceNamespace' - The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
--
-- * 'pspResourceId' - The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
--
-- * 'pspScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
putScalingPolicy
    :: Text -- ^ 'pspPolicyName'
    -> ServiceNamespace -- ^ 'pspServiceNamespace'
    -> Text -- ^ 'pspResourceId'
    -> ScalableDimension -- ^ 'pspScalableDimension'
    -> PutScalingPolicy
putScalingPolicy pPolicyName_ pServiceNamespace_ pResourceId_ pScalableDimension_ =
  PutScalingPolicy'
    { _pspPolicyType = Nothing
    , _pspTargetTrackingScalingPolicyConfiguration = Nothing
    , _pspStepScalingPolicyConfiguration = Nothing
    , _pspPolicyName = pPolicyName_
    , _pspServiceNamespace = pServiceNamespace_
    , _pspResourceId = pResourceId_
    , _pspScalableDimension = pScalableDimension_
    }


-- | The policy type. This parameter is required if you are creating a policy. For DynamoDB, only @TargetTrackingScaling@ is supported. For Amazon ECS, Spot Fleet, and Amazon RDS, both @StepScaling@ and @TargetTrackingScaling@ are supported. For any other service, only @StepScaling@ is supported.
pspPolicyType :: Lens' PutScalingPolicy (Maybe PolicyType)
pspPolicyType = lens _pspPolicyType (\ s a -> s{_pspPolicyType = a})

-- | A target tracking policy. This parameter is required if you are creating a policy and the policy type is @TargetTrackingScaling@ .
pspTargetTrackingScalingPolicyConfiguration :: Lens' PutScalingPolicy (Maybe TargetTrackingScalingPolicyConfiguration)
pspTargetTrackingScalingPolicyConfiguration = lens _pspTargetTrackingScalingPolicyConfiguration (\ s a -> s{_pspTargetTrackingScalingPolicyConfiguration = a})

-- | A step scaling policy. This parameter is required if you are creating a policy and the policy type is @StepScaling@ .
pspStepScalingPolicyConfiguration :: Lens' PutScalingPolicy (Maybe StepScalingPolicyConfiguration)
pspStepScalingPolicyConfiguration = lens _pspStepScalingPolicyConfiguration (\ s a -> s{_pspStepScalingPolicyConfiguration = a})

-- | The name of the scaling policy.
pspPolicyName :: Lens' PutScalingPolicy Text
pspPolicyName = lens _pspPolicyName (\ s a -> s{_pspPolicyName = a})

-- | The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
pspServiceNamespace :: Lens' PutScalingPolicy ServiceNamespace
pspServiceNamespace = lens _pspServiceNamespace (\ s a -> s{_pspServiceNamespace = a})

-- | The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
pspResourceId :: Lens' PutScalingPolicy Text
pspResourceId = lens _pspResourceId (\ s a -> s{_pspResourceId = a})

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
pspScalableDimension :: Lens' PutScalingPolicy ScalableDimension
pspScalableDimension = lens _pspScalableDimension (\ s a -> s{_pspScalableDimension = a})

instance AWSRequest PutScalingPolicy where
        type Rs PutScalingPolicy = PutScalingPolicyResponse
        request = postJSON applicationAutoScaling
        response
          = receiveJSON
              (\ s h x ->
                 PutScalingPolicyResponse' <$>
                   (x .?> "Alarms" .!@ mempty) <*> (pure (fromEnum s))
                     <*> (x .:> "PolicyARN"))

instance Hashable PutScalingPolicy where

instance NFData PutScalingPolicy where

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
                  ("TargetTrackingScalingPolicyConfiguration" .=) <$>
                    _pspTargetTrackingScalingPolicyConfiguration,
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
  { _psprsAlarms         :: !(Maybe [Alarm])
  , _psprsResponseStatus :: !Int
  , _psprsPolicyARN      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutScalingPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psprsAlarms' - The CloudWatch alarms created for the target tracking policy.
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
    { _psprsAlarms = Nothing
    , _psprsResponseStatus = pResponseStatus_
    , _psprsPolicyARN = pPolicyARN_
    }


-- | The CloudWatch alarms created for the target tracking policy.
psprsAlarms :: Lens' PutScalingPolicyResponse [Alarm]
psprsAlarms = lens _psprsAlarms (\ s a -> s{_psprsAlarms = a}) . _Default . _Coerce

-- | -- | The response status code.
psprsResponseStatus :: Lens' PutScalingPolicyResponse Int
psprsResponseStatus = lens _psprsResponseStatus (\ s a -> s{_psprsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the resulting scaling policy.
psprsPolicyARN :: Lens' PutScalingPolicyResponse Text
psprsPolicyARN = lens _psprsPolicyARN (\ s a -> s{_psprsPolicyARN = a})

instance NFData PutScalingPolicyResponse where
