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
-- Module      : Network.AWS.ApplicationAutoScaling.DescribeScalableTargets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the scalable targets in the specified namespace.
--
--
-- You can filter the results using the @ResourceIds@ and @ScalableDimension@ parameters.
--
-- To create a scalable target or update an existing one, see 'RegisterScalableTarget' . If you are no longer using a scalable target, you can deregister it using 'DeregisterScalableTarget' .
--
--
-- This operation returns paginated results.
module Network.AWS.ApplicationAutoScaling.DescribeScalableTargets
    (
    -- * Creating a Request
      describeScalableTargets
    , DescribeScalableTargets
    -- * Request Lenses
    , dstResourceIds
    , dstScalableDimension
    , dstNextToken
    , dstMaxResults
    , dstServiceNamespace

    -- * Destructuring the Response
    , describeScalableTargetsResponse
    , DescribeScalableTargetsResponse
    -- * Response Lenses
    , dstsrsNextToken
    , dstsrsScalableTargets
    , dstsrsResponseStatus
    ) where

import Network.AWS.ApplicationAutoScaling.Types
import Network.AWS.ApplicationAutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeScalableTargets' smart constructor.
data DescribeScalableTargets = DescribeScalableTargets'
  { _dstResourceIds       :: !(Maybe [Text])
  , _dstScalableDimension :: !(Maybe ScalableDimension)
  , _dstNextToken         :: !(Maybe Text)
  , _dstMaxResults        :: !(Maybe Int)
  , _dstServiceNamespace  :: !ServiceNamespace
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScalableTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dstResourceIds' - The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier. If you specify a scalable dimension, you must also specify a resource ID.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
--
-- * 'dstScalableDimension' - The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property. If you specify a scalable dimension, you must also specify a resource ID.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
--
-- * 'dstNextToken' - The token for the next set of results.
--
-- * 'dstMaxResults' - The maximum number of scalable targets. This value can be between 1 and 50. The default value is 50. If this parameter is used, the operation returns up to @MaxResults@ results at a time, along with a @NextToken@ value. To get the next set of results, include the @NextToken@ value in a subsequent call. If this parameter is not used, the operation returns up to 50 results and a @NextToken@ value, if applicable.
--
-- * 'dstServiceNamespace' - The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
describeScalableTargets
    :: ServiceNamespace -- ^ 'dstServiceNamespace'
    -> DescribeScalableTargets
describeScalableTargets pServiceNamespace_ =
  DescribeScalableTargets'
    { _dstResourceIds = Nothing
    , _dstScalableDimension = Nothing
    , _dstNextToken = Nothing
    , _dstMaxResults = Nothing
    , _dstServiceNamespace = pServiceNamespace_
    }


-- | The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier. If you specify a scalable dimension, you must also specify a resource ID.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
dstResourceIds :: Lens' DescribeScalableTargets [Text]
dstResourceIds = lens _dstResourceIds (\ s a -> s{_dstResourceIds = a}) . _Default . _Coerce

-- | The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property. If you specify a scalable dimension, you must also specify a resource ID.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
dstScalableDimension :: Lens' DescribeScalableTargets (Maybe ScalableDimension)
dstScalableDimension = lens _dstScalableDimension (\ s a -> s{_dstScalableDimension = a})

-- | The token for the next set of results.
dstNextToken :: Lens' DescribeScalableTargets (Maybe Text)
dstNextToken = lens _dstNextToken (\ s a -> s{_dstNextToken = a})

-- | The maximum number of scalable targets. This value can be between 1 and 50. The default value is 50. If this parameter is used, the operation returns up to @MaxResults@ results at a time, along with a @NextToken@ value. To get the next set of results, include the @NextToken@ value in a subsequent call. If this parameter is not used, the operation returns up to 50 results and a @NextToken@ value, if applicable.
dstMaxResults :: Lens' DescribeScalableTargets (Maybe Int)
dstMaxResults = lens _dstMaxResults (\ s a -> s{_dstMaxResults = a})

-- | The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
dstServiceNamespace :: Lens' DescribeScalableTargets ServiceNamespace
dstServiceNamespace = lens _dstServiceNamespace (\ s a -> s{_dstServiceNamespace = a})

instance AWSPager DescribeScalableTargets where
        page rq rs
          | stop (rs ^. dstsrsNextToken) = Nothing
          | stop (rs ^. dstsrsScalableTargets) = Nothing
          | otherwise =
            Just $ rq & dstNextToken .~ rs ^. dstsrsNextToken

instance AWSRequest DescribeScalableTargets where
        type Rs DescribeScalableTargets =
             DescribeScalableTargetsResponse
        request = postJSON applicationAutoScaling
        response
          = receiveJSON
              (\ s h x ->
                 DescribeScalableTargetsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "ScalableTargets" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeScalableTargets where

instance NFData DescribeScalableTargets where

instance ToHeaders DescribeScalableTargets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleFrontendService.DescribeScalableTargets" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeScalableTargets where
        toJSON DescribeScalableTargets'{..}
          = object
              (catMaybes
                 [("ResourceIds" .=) <$> _dstResourceIds,
                  ("ScalableDimension" .=) <$> _dstScalableDimension,
                  ("NextToken" .=) <$> _dstNextToken,
                  ("MaxResults" .=) <$> _dstMaxResults,
                  Just ("ServiceNamespace" .= _dstServiceNamespace)])

instance ToPath DescribeScalableTargets where
        toPath = const "/"

instance ToQuery DescribeScalableTargets where
        toQuery = const mempty

-- | /See:/ 'describeScalableTargetsResponse' smart constructor.
data DescribeScalableTargetsResponse = DescribeScalableTargetsResponse'
  { _dstsrsNextToken       :: !(Maybe Text)
  , _dstsrsScalableTargets :: !(Maybe [ScalableTarget])
  , _dstsrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScalableTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dstsrsNextToken' - The token required to get the next set of results. This value is @null@ if there are no more results to return.
--
-- * 'dstsrsScalableTargets' - The scalable targets that match the request parameters.
--
-- * 'dstsrsResponseStatus' - -- | The response status code.
describeScalableTargetsResponse
    :: Int -- ^ 'dstsrsResponseStatus'
    -> DescribeScalableTargetsResponse
describeScalableTargetsResponse pResponseStatus_ =
  DescribeScalableTargetsResponse'
    { _dstsrsNextToken = Nothing
    , _dstsrsScalableTargets = Nothing
    , _dstsrsResponseStatus = pResponseStatus_
    }


-- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
dstsrsNextToken :: Lens' DescribeScalableTargetsResponse (Maybe Text)
dstsrsNextToken = lens _dstsrsNextToken (\ s a -> s{_dstsrsNextToken = a})

-- | The scalable targets that match the request parameters.
dstsrsScalableTargets :: Lens' DescribeScalableTargetsResponse [ScalableTarget]
dstsrsScalableTargets = lens _dstsrsScalableTargets (\ s a -> s{_dstsrsScalableTargets = a}) . _Default . _Coerce

-- | -- | The response status code.
dstsrsResponseStatus :: Lens' DescribeScalableTargetsResponse Int
dstsrsResponseStatus = lens _dstsrsResponseStatus (\ s a -> s{_dstsrsResponseStatus = a})

instance NFData DescribeScalableTargetsResponse where
