{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.DescribeScalingPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Application Auto Scaling scaling policies for the specified service namespace.
--
-- You can filter the results using @ResourceId@ , @ScalableDimension@ , and @PolicyNames@ .
-- For more information, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html Target Tracking Scaling Policies> and <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-step-scaling-policies.html Step Scaling Policies> in the /Application Auto Scaling User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.ApplicationAutoScaling.DescribeScalingPolicies
  ( -- * Creating a request
    DescribeScalingPolicies (..),
    mkDescribeScalingPolicies,

    -- ** Request lenses
    dspPolicyNames,
    dspScalableDimension,
    dspResourceId,
    dspServiceNamespace,
    dspNextToken,
    dspMaxResults,

    -- * Destructuring the response
    DescribeScalingPoliciesResponse (..),
    mkDescribeScalingPoliciesResponse,

    -- ** Response lenses
    drsNextToken,
    drsScalingPolicies,
    drsResponseStatus,
  )
where

import Network.AWS.ApplicationAutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeScalingPolicies' smart constructor.
data DescribeScalingPolicies = DescribeScalingPolicies'
  { -- | The names of the scaling policies to describe.
    policyNames :: Lude.Maybe [Lude.Text],
    -- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property. If you specify a scalable dimension, you must also specify a resource ID.
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
    scalableDimension :: Lude.Maybe ScalableDimension,
    -- | The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier. If you specify a scalable dimension, you must also specify a resource ID.
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
    resourceId :: Lude.Maybe Lude.Text,
    -- | The namespace of the AWS service that provides the resource. For a resource provided by your own application or service, use @custom-resource@ instead.
    serviceNamespace :: ServiceNamespace,
    -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of scalable targets. This value can be between 1 and 50. The default value is 50.
    --
    -- If this parameter is used, the operation returns up to @MaxResults@ results at a time, along with a @NextToken@ value. To get the next set of results, include the @NextToken@ value in a subsequent call. If this parameter is not used, the operation returns up to 50 results and a @NextToken@ value, if applicable.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScalingPolicies' with the minimum fields required to make a request.
--
-- * 'policyNames' - The names of the scaling policies to describe.
-- * 'scalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property. If you specify a scalable dimension, you must also specify a resource ID.
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
-- * 'resourceId' - The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier. If you specify a scalable dimension, you must also specify a resource ID.
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
-- * 'serviceNamespace' - The namespace of the AWS service that provides the resource. For a resource provided by your own application or service, use @custom-resource@ instead.
-- * 'nextToken' - The token for the next set of results.
-- * 'maxResults' - The maximum number of scalable targets. This value can be between 1 and 50. The default value is 50.
--
-- If this parameter is used, the operation returns up to @MaxResults@ results at a time, along with a @NextToken@ value. To get the next set of results, include the @NextToken@ value in a subsequent call. If this parameter is not used, the operation returns up to 50 results and a @NextToken@ value, if applicable.
mkDescribeScalingPolicies ::
  -- | 'serviceNamespace'
  ServiceNamespace ->
  DescribeScalingPolicies
mkDescribeScalingPolicies pServiceNamespace_ =
  DescribeScalingPolicies'
    { policyNames = Lude.Nothing,
      scalableDimension = Lude.Nothing,
      resourceId = Lude.Nothing,
      serviceNamespace = pServiceNamespace_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The names of the scaling policies to describe.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspPolicyNames :: Lens.Lens' DescribeScalingPolicies (Lude.Maybe [Lude.Text])
dspPolicyNames = Lens.lens (policyNames :: DescribeScalingPolicies -> Lude.Maybe [Lude.Text]) (\s a -> s {policyNames = a} :: DescribeScalingPolicies)
{-# DEPRECATED dspPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property. If you specify a scalable dimension, you must also specify a resource ID.
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
dspScalableDimension :: Lens.Lens' DescribeScalingPolicies (Lude.Maybe ScalableDimension)
dspScalableDimension = Lens.lens (scalableDimension :: DescribeScalingPolicies -> Lude.Maybe ScalableDimension) (\s a -> s {scalableDimension = a} :: DescribeScalingPolicies)
{-# DEPRECATED dspScalableDimension "Use generic-lens or generic-optics with 'scalableDimension' instead." #-}

-- | The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier. If you specify a scalable dimension, you must also specify a resource ID.
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
dspResourceId :: Lens.Lens' DescribeScalingPolicies (Lude.Maybe Lude.Text)
dspResourceId = Lens.lens (resourceId :: DescribeScalingPolicies -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: DescribeScalingPolicies)
{-# DEPRECATED dspResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The namespace of the AWS service that provides the resource. For a resource provided by your own application or service, use @custom-resource@ instead.
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspServiceNamespace :: Lens.Lens' DescribeScalingPolicies ServiceNamespace
dspServiceNamespace = Lens.lens (serviceNamespace :: DescribeScalingPolicies -> ServiceNamespace) (\s a -> s {serviceNamespace = a} :: DescribeScalingPolicies)
{-# DEPRECATED dspServiceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspNextToken :: Lens.Lens' DescribeScalingPolicies (Lude.Maybe Lude.Text)
dspNextToken = Lens.lens (nextToken :: DescribeScalingPolicies -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScalingPolicies)
{-# DEPRECATED dspNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of scalable targets. This value can be between 1 and 50. The default value is 50.
--
-- If this parameter is used, the operation returns up to @MaxResults@ results at a time, along with a @NextToken@ value. To get the next set of results, include the @NextToken@ value in a subsequent call. If this parameter is not used, the operation returns up to 50 results and a @NextToken@ value, if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspMaxResults :: Lens.Lens' DescribeScalingPolicies (Lude.Maybe Lude.Int)
dspMaxResults = Lens.lens (maxResults :: DescribeScalingPolicies -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeScalingPolicies)
{-# DEPRECATED dspMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeScalingPolicies where
  page rq rs
    | Page.stop (rs Lens.^. drsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drsScalingPolicies) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dspNextToken Lens..~ rs Lens.^. drsNextToken

instance Lude.AWSRequest DescribeScalingPolicies where
  type Rs DescribeScalingPolicies = DescribeScalingPoliciesResponse
  request = Req.postJSON applicationAutoScalingService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeScalingPoliciesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ScalingPolicies" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeScalingPolicies where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AnyScaleFrontendService.DescribeScalingPolicies" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeScalingPolicies where
  toJSON DescribeScalingPolicies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PolicyNames" Lude..=) Lude.<$> policyNames,
            ("ScalableDimension" Lude..=) Lude.<$> scalableDimension,
            ("ResourceId" Lude..=) Lude.<$> resourceId,
            Lude.Just ("ServiceNamespace" Lude..= serviceNamespace),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeScalingPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeScalingPolicies where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeScalingPoliciesResponse' smart constructor.
data DescribeScalingPoliciesResponse = DescribeScalingPoliciesResponse'
  { -- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the scaling policies.
    scalingPolicies :: Lude.Maybe [ScalingPolicy],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScalingPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token required to get the next set of results. This value is @null@ if there are no more results to return.
-- * 'scalingPolicies' - Information about the scaling policies.
-- * 'responseStatus' - The response status code.
mkDescribeScalingPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeScalingPoliciesResponse
mkDescribeScalingPoliciesResponse pResponseStatus_ =
  DescribeScalingPoliciesResponse'
    { nextToken = Lude.Nothing,
      scalingPolicies = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeScalingPoliciesResponse (Lude.Maybe Lude.Text)
drsNextToken = Lens.lens (nextToken :: DescribeScalingPoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScalingPoliciesResponse)
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the scaling policies.
--
-- /Note:/ Consider using 'scalingPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsScalingPolicies :: Lens.Lens' DescribeScalingPoliciesResponse (Lude.Maybe [ScalingPolicy])
drsScalingPolicies = Lens.lens (scalingPolicies :: DescribeScalingPoliciesResponse -> Lude.Maybe [ScalingPolicy]) (\s a -> s {scalingPolicies = a} :: DescribeScalingPoliciesResponse)
{-# DEPRECATED drsScalingPolicies "Use generic-lens or generic-optics with 'scalingPolicies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeScalingPoliciesResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeScalingPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScalingPoliciesResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
