{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- Each scalable target is identified by a service namespace, resource ID, and scalable dimension. A scaling policy applies to the scalable target identified by those three attributes. You cannot create a scaling policy until you have registered the resource as a scalable target.
-- Multiple scaling policies can be in force at the same time for the same scalable target. You can have one or more target tracking scaling policies, one or more step scaling policies, or both. However, there is a chance that multiple policies could conflict, instructing the scalable target to scale out or in at the same time. Application Auto Scaling gives precedence to the policy that provides the largest capacity for both scale out and scale in. For example, if one policy increases capacity by 3, another policy increases capacity by 200 percent, and the current capacity is 10, Application Auto Scaling uses the policy with the highest calculated capacity (200% of 10 = 20) and scales out to 30.
-- We recommend caution, however, when using target tracking scaling policies with step scaling policies because conflicts between these policies can cause undesirable behavior. For example, if the step scaling policy initiates a scale-in activity before the target tracking policy is ready to scale in, the scale-in activity will not be blocked. After the scale-in activity completes, the target tracking policy could instruct the scalable target to scale out again.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html Target Tracking Scaling Policies> and <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-step-scaling-policies.html Step Scaling Policies> in the /Application Auto Scaling User Guide/ .
module Network.AWS.ApplicationAutoScaling.PutScalingPolicy
  ( -- * Creating a request
    PutScalingPolicy (..),
    mkPutScalingPolicy,

    -- ** Request lenses
    pspPolicyType,
    pspTargetTrackingScalingPolicyConfiguration,
    pspStepScalingPolicyConfiguration,
    pspPolicyName,
    pspServiceNamespace,
    pspResourceId,
    pspScalableDimension,

    -- * Destructuring the response
    PutScalingPolicyResponse (..),
    mkPutScalingPolicyResponse,

    -- ** Response lenses
    psprsAlarms,
    psprsResponseStatus,
    psprsPolicyARN,
  )
where

import Network.AWS.ApplicationAutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutScalingPolicy' smart constructor.
data PutScalingPolicy = PutScalingPolicy'
  { policyType ::
      Lude.Maybe PolicyType,
    targetTrackingScalingPolicyConfiguration ::
      Lude.Maybe TargetTrackingScalingPolicyConfiguration,
    stepScalingPolicyConfiguration ::
      Lude.Maybe StepScalingPolicyConfiguration,
    policyName :: Lude.Text,
    serviceNamespace :: ServiceNamespace,
    resourceId :: Lude.Text,
    scalableDimension :: ScalableDimension
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutScalingPolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the scaling policy.
-- * 'policyType' - The policy type. This parameter is required if you are creating a scaling policy.
--
-- The following policy types are supported:
-- @TargetTrackingScaling@ —Not supported for Amazon EMR
-- @StepScaling@ —Not supported for DynamoDB, Amazon Comprehend, Lambda, Amazon Keyspaces (for Apache Cassandra), or Amazon MSK.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html Target Tracking Scaling Policies> and <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-step-scaling-policies.html Step Scaling Policies> in the /Application Auto Scaling User Guide/ .
-- * 'resourceId' - The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.
--
--
--     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .
--
--
--     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .
--
--
--     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .
--
--
--     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .
--
--
--     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .
--
--
--     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .
--
--
--     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
--
--
--     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
--
--
--     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .
--
--
--     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .
--
--
--     * Amazon Comprehend entity recognizer endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint/EXAMPLE@ .
--
--
--     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .
--
--
--     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
--
--
--     * Amazon MSK cluster - The resource type and unique identifier are specified using the cluster ARN. Example: @arn:aws:kafka:us-east-1:123456789012:cluster/demo-cluster-1/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@ .
--
--
-- * 'scalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property.
--
--
--     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.
--
--
--     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.
--
--
--     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.
--
--
--     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.
--
--
--     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.
--
--
--     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.
--
--
--     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.
--
--
--     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.
--
--
--     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.
--
--
--     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
--
--
--     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.
--
--
--     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.
--
--
--     * @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend entity recognizer endpoint.
--
--
--     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.
--
--
--     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.
--
--
--     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
--
--
--     * @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in GiB) for brokers in an Amazon MSK cluster.
--
--
-- * 'serviceNamespace' - The namespace of the AWS service that provides the resource. For a resource provided by your own application or service, use @custom-resource@ instead.
-- * 'stepScalingPolicyConfiguration' - A step scaling policy.
--
-- This parameter is required if you are creating a policy and the policy type is @StepScaling@ .
-- * 'targetTrackingScalingPolicyConfiguration' - A target tracking scaling policy. Includes support for predefined or customized metrics.
--
-- This parameter is required if you are creating a policy and the policy type is @TargetTrackingScaling@ .
mkPutScalingPolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'scalableDimension'
  ScalableDimension ->
  PutScalingPolicy
mkPutScalingPolicy
  pPolicyName_
  pServiceNamespace_
  pResourceId_
  pScalableDimension_ =
    PutScalingPolicy'
      { policyType = Lude.Nothing,
        targetTrackingScalingPolicyConfiguration = Lude.Nothing,
        stepScalingPolicyConfiguration = Lude.Nothing,
        policyName = pPolicyName_,
        serviceNamespace = pServiceNamespace_,
        resourceId = pResourceId_,
        scalableDimension = pScalableDimension_
      }

-- | The policy type. This parameter is required if you are creating a scaling policy.
--
-- The following policy types are supported:
-- @TargetTrackingScaling@ —Not supported for Amazon EMR
-- @StepScaling@ —Not supported for DynamoDB, Amazon Comprehend, Lambda, Amazon Keyspaces (for Apache Cassandra), or Amazon MSK.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html Target Tracking Scaling Policies> and <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-step-scaling-policies.html Step Scaling Policies> in the /Application Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspPolicyType :: Lens.Lens' PutScalingPolicy (Lude.Maybe PolicyType)
pspPolicyType = Lens.lens (policyType :: PutScalingPolicy -> Lude.Maybe PolicyType) (\s a -> s {policyType = a} :: PutScalingPolicy)
{-# DEPRECATED pspPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

-- | A target tracking scaling policy. Includes support for predefined or customized metrics.
--
-- This parameter is required if you are creating a policy and the policy type is @TargetTrackingScaling@ .
--
-- /Note:/ Consider using 'targetTrackingScalingPolicyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspTargetTrackingScalingPolicyConfiguration :: Lens.Lens' PutScalingPolicy (Lude.Maybe TargetTrackingScalingPolicyConfiguration)
pspTargetTrackingScalingPolicyConfiguration = Lens.lens (targetTrackingScalingPolicyConfiguration :: PutScalingPolicy -> Lude.Maybe TargetTrackingScalingPolicyConfiguration) (\s a -> s {targetTrackingScalingPolicyConfiguration = a} :: PutScalingPolicy)
{-# DEPRECATED pspTargetTrackingScalingPolicyConfiguration "Use generic-lens or generic-optics with 'targetTrackingScalingPolicyConfiguration' instead." #-}

-- | A step scaling policy.
--
-- This parameter is required if you are creating a policy and the policy type is @StepScaling@ .
--
-- /Note:/ Consider using 'stepScalingPolicyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspStepScalingPolicyConfiguration :: Lens.Lens' PutScalingPolicy (Lude.Maybe StepScalingPolicyConfiguration)
pspStepScalingPolicyConfiguration = Lens.lens (stepScalingPolicyConfiguration :: PutScalingPolicy -> Lude.Maybe StepScalingPolicyConfiguration) (\s a -> s {stepScalingPolicyConfiguration = a} :: PutScalingPolicy)
{-# DEPRECATED pspStepScalingPolicyConfiguration "Use generic-lens or generic-optics with 'stepScalingPolicyConfiguration' instead." #-}

-- | The name of the scaling policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspPolicyName :: Lens.Lens' PutScalingPolicy Lude.Text
pspPolicyName = Lens.lens (policyName :: PutScalingPolicy -> Lude.Text) (\s a -> s {policyName = a} :: PutScalingPolicy)
{-# DEPRECATED pspPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The namespace of the AWS service that provides the resource. For a resource provided by your own application or service, use @custom-resource@ instead.
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspServiceNamespace :: Lens.Lens' PutScalingPolicy ServiceNamespace
pspServiceNamespace = Lens.lens (serviceNamespace :: PutScalingPolicy -> ServiceNamespace) (\s a -> s {serviceNamespace = a} :: PutScalingPolicy)
{-# DEPRECATED pspServiceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead." #-}

-- | The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.
--
--
--     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .
--
--
--     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .
--
--
--     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .
--
--
--     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .
--
--
--     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .
--
--
--     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .
--
--
--     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
--
--
--     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
--
--
--     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .
--
--
--     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .
--
--
--     * Amazon Comprehend entity recognizer endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint/EXAMPLE@ .
--
--
--     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .
--
--
--     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
--
--
--     * Amazon MSK cluster - The resource type and unique identifier are specified using the cluster ARN. Example: @arn:aws:kafka:us-east-1:123456789012:cluster/demo-cluster-1/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@ .
--
--
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspResourceId :: Lens.Lens' PutScalingPolicy Lude.Text
pspResourceId = Lens.lens (resourceId :: PutScalingPolicy -> Lude.Text) (\s a -> s {resourceId = a} :: PutScalingPolicy)
{-# DEPRECATED pspResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.
--
--
--     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.
--
--
--     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.
--
--
--     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.
--
--
--     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.
--
--
--     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.
--
--
--     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.
--
--
--     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.
--
--
--     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.
--
--
--     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.
--
--
--     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
--
--
--     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.
--
--
--     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.
--
--
--     * @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend entity recognizer endpoint.
--
--
--     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.
--
--
--     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.
--
--
--     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
--
--
--     * @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in GiB) for brokers in an Amazon MSK cluster.
--
--
--
-- /Note:/ Consider using 'scalableDimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspScalableDimension :: Lens.Lens' PutScalingPolicy ScalableDimension
pspScalableDimension = Lens.lens (scalableDimension :: PutScalingPolicy -> ScalableDimension) (\s a -> s {scalableDimension = a} :: PutScalingPolicy)
{-# DEPRECATED pspScalableDimension "Use generic-lens or generic-optics with 'scalableDimension' instead." #-}

instance Lude.AWSRequest PutScalingPolicy where
  type Rs PutScalingPolicy = PutScalingPolicyResponse
  request = Req.postJSON applicationAutoScalingService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutScalingPolicyResponse'
            Lude.<$> (x Lude..?> "Alarms" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "PolicyARN")
      )

instance Lude.ToHeaders PutScalingPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AnyScaleFrontendService.PutScalingPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutScalingPolicy where
  toJSON PutScalingPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PolicyType" Lude..=) Lude.<$> policyType,
            ("TargetTrackingScalingPolicyConfiguration" Lude..=)
              Lude.<$> targetTrackingScalingPolicyConfiguration,
            ("StepScalingPolicyConfiguration" Lude..=)
              Lude.<$> stepScalingPolicyConfiguration,
            Lude.Just ("PolicyName" Lude..= policyName),
            Lude.Just ("ServiceNamespace" Lude..= serviceNamespace),
            Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("ScalableDimension" Lude..= scalableDimension)
          ]
      )

instance Lude.ToPath PutScalingPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutScalingPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutScalingPolicyResponse' smart constructor.
data PutScalingPolicyResponse = PutScalingPolicyResponse'
  { alarms ::
      Lude.Maybe [Alarm],
    responseStatus :: Lude.Int,
    policyARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutScalingPolicyResponse' with the minimum fields required to make a request.
--
-- * 'alarms' - The CloudWatch alarms created for the target tracking scaling policy.
-- * 'policyARN' - The Amazon Resource Name (ARN) of the resulting scaling policy.
-- * 'responseStatus' - The response status code.
mkPutScalingPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'policyARN'
  Lude.Text ->
  PutScalingPolicyResponse
mkPutScalingPolicyResponse pResponseStatus_ pPolicyARN_ =
  PutScalingPolicyResponse'
    { alarms = Lude.Nothing,
      responseStatus = pResponseStatus_,
      policyARN = pPolicyARN_
    }

-- | The CloudWatch alarms created for the target tracking scaling policy.
--
-- /Note:/ Consider using 'alarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psprsAlarms :: Lens.Lens' PutScalingPolicyResponse (Lude.Maybe [Alarm])
psprsAlarms = Lens.lens (alarms :: PutScalingPolicyResponse -> Lude.Maybe [Alarm]) (\s a -> s {alarms = a} :: PutScalingPolicyResponse)
{-# DEPRECATED psprsAlarms "Use generic-lens or generic-optics with 'alarms' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psprsResponseStatus :: Lens.Lens' PutScalingPolicyResponse Lude.Int
psprsResponseStatus = Lens.lens (responseStatus :: PutScalingPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutScalingPolicyResponse)
{-# DEPRECATED psprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the resulting scaling policy.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psprsPolicyARN :: Lens.Lens' PutScalingPolicyResponse Lude.Text
psprsPolicyARN = Lens.lens (policyARN :: PutScalingPolicyResponse -> Lude.Text) (\s a -> s {policyARN = a} :: PutScalingPolicyResponse)
{-# DEPRECATED psprsPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}
