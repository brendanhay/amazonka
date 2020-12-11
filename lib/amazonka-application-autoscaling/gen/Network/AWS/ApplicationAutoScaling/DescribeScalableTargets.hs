{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.DescribeScalableTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the scalable targets in the specified namespace.
--
-- You can filter the results using @ResourceIds@ and @ScalableDimension@ .
--
-- This operation returns paginated results.
module Network.AWS.ApplicationAutoScaling.DescribeScalableTargets
  ( -- * Creating a request
    DescribeScalableTargets (..),
    mkDescribeScalableTargets,

    -- ** Request lenses
    dstResourceIds,
    dstScalableDimension,
    dstNextToken,
    dstMaxResults,
    dstServiceNamespace,

    -- * Destructuring the response
    DescribeScalableTargetsResponse (..),
    mkDescribeScalableTargetsResponse,

    -- ** Response lenses
    dstsrsNextToken,
    dstsrsScalableTargets,
    dstsrsResponseStatus,
  )
where

import Network.AWS.ApplicationAutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeScalableTargets' smart constructor.
data DescribeScalableTargets = DescribeScalableTargets'
  { resourceIds ::
      Lude.Maybe [Lude.Text],
    scalableDimension ::
      Lude.Maybe ScalableDimension,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int,
    serviceNamespace :: ServiceNamespace
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScalableTargets' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of scalable targets. This value can be between 1 and 50. The default value is 50.
--
-- If this parameter is used, the operation returns up to @MaxResults@ results at a time, along with a @NextToken@ value. To get the next set of results, include the @NextToken@ value in a subsequent call. If this parameter is not used, the operation returns up to 50 results and a @NextToken@ value, if applicable.
-- * 'nextToken' - The token for the next set of results.
-- * 'resourceIds' - The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier. If you specify a scalable dimension, you must also specify a resource ID.
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
-- * 'scalableDimension' - The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property. If you specify a scalable dimension, you must also specify a resource ID.
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
mkDescribeScalableTargets ::
  -- | 'serviceNamespace'
  ServiceNamespace ->
  DescribeScalableTargets
mkDescribeScalableTargets pServiceNamespace_ =
  DescribeScalableTargets'
    { resourceIds = Lude.Nothing,
      scalableDimension = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      serviceNamespace = pServiceNamespace_
    }

-- | The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier. If you specify a scalable dimension, you must also specify a resource ID.
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
-- /Note:/ Consider using 'resourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstResourceIds :: Lens.Lens' DescribeScalableTargets (Lude.Maybe [Lude.Text])
dstResourceIds = Lens.lens (resourceIds :: DescribeScalableTargets -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceIds = a} :: DescribeScalableTargets)
{-# DEPRECATED dstResourceIds "Use generic-lens or generic-optics with 'resourceIds' instead." #-}

-- | The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property. If you specify a scalable dimension, you must also specify a resource ID.
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
dstScalableDimension :: Lens.Lens' DescribeScalableTargets (Lude.Maybe ScalableDimension)
dstScalableDimension = Lens.lens (scalableDimension :: DescribeScalableTargets -> Lude.Maybe ScalableDimension) (\s a -> s {scalableDimension = a} :: DescribeScalableTargets)
{-# DEPRECATED dstScalableDimension "Use generic-lens or generic-optics with 'scalableDimension' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstNextToken :: Lens.Lens' DescribeScalableTargets (Lude.Maybe Lude.Text)
dstNextToken = Lens.lens (nextToken :: DescribeScalableTargets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScalableTargets)
{-# DEPRECATED dstNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of scalable targets. This value can be between 1 and 50. The default value is 50.
--
-- If this parameter is used, the operation returns up to @MaxResults@ results at a time, along with a @NextToken@ value. To get the next set of results, include the @NextToken@ value in a subsequent call. If this parameter is not used, the operation returns up to 50 results and a @NextToken@ value, if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstMaxResults :: Lens.Lens' DescribeScalableTargets (Lude.Maybe Lude.Int)
dstMaxResults = Lens.lens (maxResults :: DescribeScalableTargets -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeScalableTargets)
{-# DEPRECATED dstMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The namespace of the AWS service that provides the resource. For a resource provided by your own application or service, use @custom-resource@ instead.
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstServiceNamespace :: Lens.Lens' DescribeScalableTargets ServiceNamespace
dstServiceNamespace = Lens.lens (serviceNamespace :: DescribeScalableTargets -> ServiceNamespace) (\s a -> s {serviceNamespace = a} :: DescribeScalableTargets)
{-# DEPRECATED dstServiceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead." #-}

instance Page.AWSPager DescribeScalableTargets where
  page rq rs
    | Page.stop (rs Lens.^. dstsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dstsrsScalableTargets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dstNextToken Lens..~ rs Lens.^. dstsrsNextToken

instance Lude.AWSRequest DescribeScalableTargets where
  type Rs DescribeScalableTargets = DescribeScalableTargetsResponse
  request = Req.postJSON applicationAutoScalingService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeScalableTargetsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ScalableTargets" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeScalableTargets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AnyScaleFrontendService.DescribeScalableTargets" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeScalableTargets where
  toJSON DescribeScalableTargets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceIds" Lude..=) Lude.<$> resourceIds,
            ("ScalableDimension" Lude..=) Lude.<$> scalableDimension,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ServiceNamespace" Lude..= serviceNamespace)
          ]
      )

instance Lude.ToPath DescribeScalableTargets where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeScalableTargets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeScalableTargetsResponse' smart constructor.
data DescribeScalableTargetsResponse = DescribeScalableTargetsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    scalableTargets ::
      Lude.Maybe [ScalableTarget],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScalableTargetsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token required to get the next set of results. This value is @null@ if there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'scalableTargets' - The scalable targets that match the request parameters.
mkDescribeScalableTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeScalableTargetsResponse
mkDescribeScalableTargetsResponse pResponseStatus_ =
  DescribeScalableTargetsResponse'
    { nextToken = Lude.Nothing,
      scalableTargets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstsrsNextToken :: Lens.Lens' DescribeScalableTargetsResponse (Lude.Maybe Lude.Text)
dstsrsNextToken = Lens.lens (nextToken :: DescribeScalableTargetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScalableTargetsResponse)
{-# DEPRECATED dstsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The scalable targets that match the request parameters.
--
-- /Note:/ Consider using 'scalableTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstsrsScalableTargets :: Lens.Lens' DescribeScalableTargetsResponse (Lude.Maybe [ScalableTarget])
dstsrsScalableTargets = Lens.lens (scalableTargets :: DescribeScalableTargetsResponse -> Lude.Maybe [ScalableTarget]) (\s a -> s {scalableTargets = a} :: DescribeScalableTargetsResponse)
{-# DEPRECATED dstsrsScalableTargets "Use generic-lens or generic-optics with 'scalableTargets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dstsrsResponseStatus :: Lens.Lens' DescribeScalableTargetsResponse Lude.Int
dstsrsResponseStatus = Lens.lens (responseStatus :: DescribeScalableTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScalableTargetsResponse)
{-# DEPRECATED dstsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
