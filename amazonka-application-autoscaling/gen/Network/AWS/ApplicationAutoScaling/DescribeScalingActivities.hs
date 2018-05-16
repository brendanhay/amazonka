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
-- Module      : Network.AWS.ApplicationAutoScaling.DescribeScalingActivities
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides descriptive information about the scaling activities in the specified namespace from the previous six weeks.
--
--
-- You can filter the results using the @ResourceId@ and @ScalableDimension@ parameters.
--
-- Scaling activities are triggered by CloudWatch alarms that are associated with scaling policies. To view the scaling policies for a service namespace, see 'DescribeScalingPolicies' . To create a scaling policy or update an existing one, see 'PutScalingPolicy' .
--
--
-- This operation returns paginated results.
module Network.AWS.ApplicationAutoScaling.DescribeScalingActivities
    (
    -- * Creating a Request
      describeScalingActivities
    , DescribeScalingActivities
    -- * Request Lenses
    , desScalableDimension
    , desResourceId
    , desNextToken
    , desMaxResults
    , desServiceNamespace

    -- * Destructuring the Response
    , describeScalingActivitiesResponse
    , DescribeScalingActivitiesResponse
    -- * Response Lenses
    , dsasrsScalingActivities
    , dsasrsNextToken
    , dsasrsResponseStatus
    ) where

import Network.AWS.ApplicationAutoScaling.Types
import Network.AWS.ApplicationAutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeScalingActivities' smart constructor.
data DescribeScalingActivities = DescribeScalingActivities'
  { _desScalableDimension :: !(Maybe ScalableDimension)
  , _desResourceId        :: !(Maybe Text)
  , _desNextToken         :: !(Maybe Text)
  , _desMaxResults        :: !(Maybe Int)
  , _desServiceNamespace  :: !ServiceNamespace
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScalingActivities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property. If you specify a scalable dimension, you must also specify a resource ID.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
--
-- * 'desResourceId' - The identifier of the resource associated with the scaling activity. This string consists of the resource type and unique identifier. If you specify a scalable dimension, you must also specify a resource ID.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
--
-- * 'desNextToken' - The token for the next set of results.
--
-- * 'desMaxResults' - The maximum number of scalable targets. This value can be between 1 and 50. The default value is 50. If this parameter is used, the operation returns up to @MaxResults@ results at a time, along with a @NextToken@ value. To get the next set of results, include the @NextToken@ value in a subsequent call. If this parameter is not used, the operation returns up to 50 results and a @NextToken@ value, if applicable.
--
-- * 'desServiceNamespace' - The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
describeScalingActivities
    :: ServiceNamespace -- ^ 'desServiceNamespace'
    -> DescribeScalingActivities
describeScalingActivities pServiceNamespace_ =
  DescribeScalingActivities'
    { _desScalableDimension = Nothing
    , _desResourceId = Nothing
    , _desNextToken = Nothing
    , _desMaxResults = Nothing
    , _desServiceNamespace = pServiceNamespace_
    }


-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property. If you specify a scalable dimension, you must also specify a resource ID.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
desScalableDimension :: Lens' DescribeScalingActivities (Maybe ScalableDimension)
desScalableDimension = lens _desScalableDimension (\ s a -> s{_desScalableDimension = a})

-- | The identifier of the resource associated with the scaling activity. This string consists of the resource type and unique identifier. If you specify a scalable dimension, you must also specify a resource ID.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
desResourceId :: Lens' DescribeScalingActivities (Maybe Text)
desResourceId = lens _desResourceId (\ s a -> s{_desResourceId = a})

-- | The token for the next set of results.
desNextToken :: Lens' DescribeScalingActivities (Maybe Text)
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a})

-- | The maximum number of scalable targets. This value can be between 1 and 50. The default value is 50. If this parameter is used, the operation returns up to @MaxResults@ results at a time, along with a @NextToken@ value. To get the next set of results, include the @NextToken@ value in a subsequent call. If this parameter is not used, the operation returns up to 50 results and a @NextToken@ value, if applicable.
desMaxResults :: Lens' DescribeScalingActivities (Maybe Int)
desMaxResults = lens _desMaxResults (\ s a -> s{_desMaxResults = a})

-- | The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
desServiceNamespace :: Lens' DescribeScalingActivities ServiceNamespace
desServiceNamespace = lens _desServiceNamespace (\ s a -> s{_desServiceNamespace = a})

instance AWSPager DescribeScalingActivities where
        page rq rs
          | stop (rs ^. dsasrsNextToken) = Nothing
          | stop (rs ^. dsasrsScalingActivities) = Nothing
          | otherwise =
            Just $ rq & desNextToken .~ rs ^. dsasrsNextToken

instance AWSRequest DescribeScalingActivities where
        type Rs DescribeScalingActivities =
             DescribeScalingActivitiesResponse
        request = postJSON applicationAutoScaling
        response
          = receiveJSON
              (\ s h x ->
                 DescribeScalingActivitiesResponse' <$>
                   (x .?> "ScalingActivities" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeScalingActivities where

instance NFData DescribeScalingActivities where

instance ToHeaders DescribeScalingActivities where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleFrontendService.DescribeScalingActivities"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeScalingActivities where
        toJSON DescribeScalingActivities'{..}
          = object
              (catMaybes
                 [("ScalableDimension" .=) <$> _desScalableDimension,
                  ("ResourceId" .=) <$> _desResourceId,
                  ("NextToken" .=) <$> _desNextToken,
                  ("MaxResults" .=) <$> _desMaxResults,
                  Just ("ServiceNamespace" .= _desServiceNamespace)])

instance ToPath DescribeScalingActivities where
        toPath = const "/"

instance ToQuery DescribeScalingActivities where
        toQuery = const mempty

-- | /See:/ 'describeScalingActivitiesResponse' smart constructor.
data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse'
  { _dsasrsScalingActivities :: !(Maybe [ScalingActivity])
  , _dsasrsNextToken         :: !(Maybe Text)
  , _dsasrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScalingActivitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsasrsScalingActivities' - A list of scaling activity objects.
--
-- * 'dsasrsNextToken' - The token required to get the next set of results. This value is @null@ if there are no more results to return.
--
-- * 'dsasrsResponseStatus' - -- | The response status code.
describeScalingActivitiesResponse
    :: Int -- ^ 'dsasrsResponseStatus'
    -> DescribeScalingActivitiesResponse
describeScalingActivitiesResponse pResponseStatus_ =
  DescribeScalingActivitiesResponse'
    { _dsasrsScalingActivities = Nothing
    , _dsasrsNextToken = Nothing
    , _dsasrsResponseStatus = pResponseStatus_
    }


-- | A list of scaling activity objects.
dsasrsScalingActivities :: Lens' DescribeScalingActivitiesResponse [ScalingActivity]
dsasrsScalingActivities = lens _dsasrsScalingActivities (\ s a -> s{_dsasrsScalingActivities = a}) . _Default . _Coerce

-- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
dsasrsNextToken :: Lens' DescribeScalingActivitiesResponse (Maybe Text)
dsasrsNextToken = lens _dsasrsNextToken (\ s a -> s{_dsasrsNextToken = a})

-- | -- | The response status code.
dsasrsResponseStatus :: Lens' DescribeScalingActivitiesResponse Int
dsasrsResponseStatus = lens _dsasrsResponseStatus (\ s a -> s{_dsasrsResponseStatus = a})

instance NFData DescribeScalingActivitiesResponse
         where
