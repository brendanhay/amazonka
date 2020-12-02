{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.PutScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scaling policy for an Application Auto Scaling scalable target.
--
--
-- Each scalable target is identified by a service namespace, resource ID, and scalable dimension. A scaling policy applies to the scalable target identified by those three attributes. You cannot create a scaling policy until you have registered the resource as a scalable target.
--
-- Multiple scaling policies can be in force at the same time for the same scalable target. You can have one or more target tracking scaling policies, one or more step scaling policies, or both. However, there is a chance that multiple policies could conflict, instructing the scalable target to scale out or in at the same time. Application Auto Scaling gives precedence to the policy that provides the largest capacity for both scale out and scale in. For example, if one policy increases capacity by 3, another policy increases capacity by 200 percent, and the current capacity is 10, Application Auto Scaling uses the policy with the highest calculated capacity (200% of 10 = 20) and scales out to 30.
--
-- We recommend caution, however, when using target tracking scaling policies with step scaling policies because conflicts between these policies can cause undesirable behavior. For example, if the step scaling policy initiates a scale-in activity before the target tracking policy is ready to scale in, the scale-in activity will not be blocked. After the scale-in activity completes, the target tracking policy could instruct the scalable target to scale out again.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html Target Tracking Scaling Policies> and <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-step-scaling-policies.html Step Scaling Policies> in the /Application Auto Scaling User Guide/ .
module Network.AWS.ApplicationAutoScaling.PutScalingPolicy
  ( -- * Creating a Request
    putScalingPolicy,
    PutScalingPolicy,

    -- * Request Lenses
    pspPolicyType,
    pspTargetTrackingScalingPolicyConfiguration,
    pspStepScalingPolicyConfiguration,
    pspPolicyName,
    pspServiceNamespace,
    pspResourceId,
    pspScalableDimension,

    -- * Destructuring the Response
    putScalingPolicyResponse,
    PutScalingPolicyResponse,

    -- * Response Lenses
    psprsAlarms,
    psprsResponseStatus,
    psprsPolicyARN,
  )
where

import Network.AWS.ApplicationAutoScaling.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putScalingPolicy' smart constructor.
data PutScalingPolicy = PutScalingPolicy'
  { _pspPolicyType ::
      !(Maybe PolicyType),
    _pspTargetTrackingScalingPolicyConfiguration ::
      !(Maybe TargetTrackingScalingPolicyConfiguration),
    _pspStepScalingPolicyConfiguration ::
      !(Maybe StepScalingPolicyConfiguration),
    _pspPolicyName :: !Text,
    _pspServiceNamespace :: !ServiceNamespace,
    _pspResourceId :: !Text,
    _pspScalableDimension :: !ScalableDimension
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pspPolicyType' - The policy type. This parameter is required if you are creating a scaling policy. The following policy types are supported:  @TargetTrackingScaling@ —Not supported for Amazon EMR @StepScaling@ —Not supported for DynamoDB, Amazon Comprehend, Lambda, Amazon Keyspaces (for Apache Cassandra), or Amazon MSK. For more information, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html Target Tracking Scaling Policies> and <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-step-scaling-policies.html Step Scaling Policies> in the /Application Auto Scaling User Guide/ .
--
-- * 'pspTargetTrackingScalingPolicyConfiguration' - A target tracking scaling policy. Includes support for predefined or customized metrics. This parameter is required if you are creating a policy and the policy type is @TargetTrackingScaling@ .
--
-- * 'pspStepScalingPolicyConfiguration' - A step scaling policy. This parameter is required if you are creating a policy and the policy type is @StepScaling@ .
--
-- * 'pspPolicyName' - The name of the scaling policy.
--
-- * 'pspServiceNamespace' - The namespace of the AWS service that provides the resource. For a resource provided by your own application or service, use @custom-resource@ instead.
--
-- * 'pspResourceId' - The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Amazon Comprehend entity recognizer endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .     * Amazon MSK cluster - The resource type and unique identifier are specified using the cluster ARN. Example: @arn:aws:kafka:us-east-1:123456789012:cluster/demo-cluster-1/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@ .
--
-- * 'pspScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend entity recognizer endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.     * @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in GiB) for brokers in an Amazon MSK cluster.
putScalingPolicy ::
  -- | 'pspPolicyName'
  Text ->
  -- | 'pspServiceNamespace'
  ServiceNamespace ->
  -- | 'pspResourceId'
  Text ->
  -- | 'pspScalableDimension'
  ScalableDimension ->
  PutScalingPolicy
putScalingPolicy
  pPolicyName_
  pServiceNamespace_
  pResourceId_
  pScalableDimension_ =
    PutScalingPolicy'
      { _pspPolicyType = Nothing,
        _pspTargetTrackingScalingPolicyConfiguration = Nothing,
        _pspStepScalingPolicyConfiguration = Nothing,
        _pspPolicyName = pPolicyName_,
        _pspServiceNamespace = pServiceNamespace_,
        _pspResourceId = pResourceId_,
        _pspScalableDimension = pScalableDimension_
      }

-- | The policy type. This parameter is required if you are creating a scaling policy. The following policy types are supported:  @TargetTrackingScaling@ —Not supported for Amazon EMR @StepScaling@ —Not supported for DynamoDB, Amazon Comprehend, Lambda, Amazon Keyspaces (for Apache Cassandra), or Amazon MSK. For more information, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html Target Tracking Scaling Policies> and <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-step-scaling-policies.html Step Scaling Policies> in the /Application Auto Scaling User Guide/ .
pspPolicyType :: Lens' PutScalingPolicy (Maybe PolicyType)
pspPolicyType = lens _pspPolicyType (\s a -> s {_pspPolicyType = a})

-- | A target tracking scaling policy. Includes support for predefined or customized metrics. This parameter is required if you are creating a policy and the policy type is @TargetTrackingScaling@ .
pspTargetTrackingScalingPolicyConfiguration :: Lens' PutScalingPolicy (Maybe TargetTrackingScalingPolicyConfiguration)
pspTargetTrackingScalingPolicyConfiguration = lens _pspTargetTrackingScalingPolicyConfiguration (\s a -> s {_pspTargetTrackingScalingPolicyConfiguration = a})

-- | A step scaling policy. This parameter is required if you are creating a policy and the policy type is @StepScaling@ .
pspStepScalingPolicyConfiguration :: Lens' PutScalingPolicy (Maybe StepScalingPolicyConfiguration)
pspStepScalingPolicyConfiguration = lens _pspStepScalingPolicyConfiguration (\s a -> s {_pspStepScalingPolicyConfiguration = a})

-- | The name of the scaling policy.
pspPolicyName :: Lens' PutScalingPolicy Text
pspPolicyName = lens _pspPolicyName (\s a -> s {_pspPolicyName = a})

-- | The namespace of the AWS service that provides the resource. For a resource provided by your own application or service, use @custom-resource@ instead.
pspServiceNamespace :: Lens' PutScalingPolicy ServiceNamespace
pspServiceNamespace = lens _pspServiceNamespace (\s a -> s {_pspServiceNamespace = a})

-- | The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Amazon Comprehend entity recognizer endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .     * Amazon MSK cluster - The resource type and unique identifier are specified using the cluster ARN. Example: @arn:aws:kafka:us-east-1:123456789012:cluster/demo-cluster-1/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@ .
pspResourceId :: Lens' PutScalingPolicy Text
pspResourceId = lens _pspResourceId (\s a -> s {_pspResourceId = a})

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend entity recognizer endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.     * @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in GiB) for brokers in an Amazon MSK cluster.
pspScalableDimension :: Lens' PutScalingPolicy ScalableDimension
pspScalableDimension = lens _pspScalableDimension (\s a -> s {_pspScalableDimension = a})

instance AWSRequest PutScalingPolicy where
  type Rs PutScalingPolicy = PutScalingPolicyResponse
  request = postJSON applicationAutoScaling
  response =
    receiveJSON
      ( \s h x ->
          PutScalingPolicyResponse'
            <$> (x .?> "Alarms" .!@ mempty)
            <*> (pure (fromEnum s))
            <*> (x .:> "PolicyARN")
      )

instance Hashable PutScalingPolicy

instance NFData PutScalingPolicy

instance ToHeaders PutScalingPolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AnyScaleFrontendService.PutScalingPolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutScalingPolicy where
  toJSON PutScalingPolicy' {..} =
    object
      ( catMaybes
          [ ("PolicyType" .=) <$> _pspPolicyType,
            ("TargetTrackingScalingPolicyConfiguration" .=)
              <$> _pspTargetTrackingScalingPolicyConfiguration,
            ("StepScalingPolicyConfiguration" .=)
              <$> _pspStepScalingPolicyConfiguration,
            Just ("PolicyName" .= _pspPolicyName),
            Just ("ServiceNamespace" .= _pspServiceNamespace),
            Just ("ResourceId" .= _pspResourceId),
            Just ("ScalableDimension" .= _pspScalableDimension)
          ]
      )

instance ToPath PutScalingPolicy where
  toPath = const "/"

instance ToQuery PutScalingPolicy where
  toQuery = const mempty

-- | /See:/ 'putScalingPolicyResponse' smart constructor.
data PutScalingPolicyResponse = PutScalingPolicyResponse'
  { _psprsAlarms ::
      !(Maybe [Alarm]),
    _psprsResponseStatus :: !Int,
    _psprsPolicyARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutScalingPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psprsAlarms' - The CloudWatch alarms created for the target tracking scaling policy.
--
-- * 'psprsResponseStatus' - -- | The response status code.
--
-- * 'psprsPolicyARN' - The Amazon Resource Name (ARN) of the resulting scaling policy.
putScalingPolicyResponse ::
  -- | 'psprsResponseStatus'
  Int ->
  -- | 'psprsPolicyARN'
  Text ->
  PutScalingPolicyResponse
putScalingPolicyResponse pResponseStatus_ pPolicyARN_ =
  PutScalingPolicyResponse'
    { _psprsAlarms = Nothing,
      _psprsResponseStatus = pResponseStatus_,
      _psprsPolicyARN = pPolicyARN_
    }

-- | The CloudWatch alarms created for the target tracking scaling policy.
psprsAlarms :: Lens' PutScalingPolicyResponse [Alarm]
psprsAlarms = lens _psprsAlarms (\s a -> s {_psprsAlarms = a}) . _Default . _Coerce

-- | -- | The response status code.
psprsResponseStatus :: Lens' PutScalingPolicyResponse Int
psprsResponseStatus = lens _psprsResponseStatus (\s a -> s {_psprsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the resulting scaling policy.
psprsPolicyARN :: Lens' PutScalingPolicyResponse Text
psprsPolicyARN = lens _psprsPolicyARN (\s a -> s {_psprsPolicyARN = a})

instance NFData PutScalingPolicyResponse
