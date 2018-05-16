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
-- Module      : Network.AWS.ApplicationAutoScaling.DescribeScalingPolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the scaling policies for the specified service namespace.
--
--
-- You can filter the results using the @ResourceId@ , @ScalableDimension@ , and @PolicyNames@ parameters.
--
-- To create a scaling policy or update an existing one, see 'PutScalingPolicy' . If you are no longer using a scaling policy, you can delete it using 'DeleteScalingPolicy' .
--
--
-- This operation returns paginated results.
module Network.AWS.ApplicationAutoScaling.DescribeScalingPolicies
    (
    -- * Creating a Request
      describeScalingPolicies
    , DescribeScalingPolicies
    -- * Request Lenses
    , dPolicyNames
    , dScalableDimension
    , dResourceId
    , dNextToken
    , dMaxResults
    , dServiceNamespace

    -- * Destructuring the Response
    , describeScalingPoliciesResponse
    , DescribeScalingPoliciesResponse
    -- * Response Lenses
    , drsNextToken
    , drsScalingPolicies
    , drsResponseStatus
    ) where

import Network.AWS.ApplicationAutoScaling.Types
import Network.AWS.ApplicationAutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeScalingPolicies' smart constructor.
data DescribeScalingPolicies = DescribeScalingPolicies'
  { _dPolicyNames       :: !(Maybe [Text])
  , _dScalableDimension :: !(Maybe ScalableDimension)
  , _dResourceId        :: !(Maybe Text)
  , _dNextToken         :: !(Maybe Text)
  , _dMaxResults        :: !(Maybe Int)
  , _dServiceNamespace  :: !ServiceNamespace
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScalingPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dPolicyNames' - The names of the scaling policies to describe.
--
-- * 'dScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property. If you specify a scalable dimension, you must also specify a resource ID.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
--
-- * 'dResourceId' - The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier. If you specify a scalable dimension, you must also specify a resource ID.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
--
-- * 'dNextToken' - The token for the next set of results.
--
-- * 'dMaxResults' - The maximum number of scalable targets. This value can be between 1 and 50. The default value is 50. If this parameter is used, the operation returns up to @MaxResults@ results at a time, along with a @NextToken@ value. To get the next set of results, include the @NextToken@ value in a subsequent call. If this parameter is not used, the operation returns up to 50 results and a @NextToken@ value, if applicable.
--
-- * 'dServiceNamespace' - The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
describeScalingPolicies
    :: ServiceNamespace -- ^ 'dServiceNamespace'
    -> DescribeScalingPolicies
describeScalingPolicies pServiceNamespace_ =
  DescribeScalingPolicies'
    { _dPolicyNames = Nothing
    , _dScalableDimension = Nothing
    , _dResourceId = Nothing
    , _dNextToken = Nothing
    , _dMaxResults = Nothing
    , _dServiceNamespace = pServiceNamespace_
    }


-- | The names of the scaling policies to describe.
dPolicyNames :: Lens' DescribeScalingPolicies [Text]
dPolicyNames = lens _dPolicyNames (\ s a -> s{_dPolicyNames = a}) . _Default . _Coerce

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property. If you specify a scalable dimension, you must also specify a resource ID.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
dScalableDimension :: Lens' DescribeScalingPolicies (Maybe ScalableDimension)
dScalableDimension = lens _dScalableDimension (\ s a -> s{_dScalableDimension = a})

-- | The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier. If you specify a scalable dimension, you must also specify a resource ID.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
dResourceId :: Lens' DescribeScalingPolicies (Maybe Text)
dResourceId = lens _dResourceId (\ s a -> s{_dResourceId = a})

-- | The token for the next set of results.
dNextToken :: Lens' DescribeScalingPolicies (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a})

-- | The maximum number of scalable targets. This value can be between 1 and 50. The default value is 50. If this parameter is used, the operation returns up to @MaxResults@ results at a time, along with a @NextToken@ value. To get the next set of results, include the @NextToken@ value in a subsequent call. If this parameter is not used, the operation returns up to 50 results and a @NextToken@ value, if applicable.
dMaxResults :: Lens' DescribeScalingPolicies (Maybe Int)
dMaxResults = lens _dMaxResults (\ s a -> s{_dMaxResults = a})

-- | The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
dServiceNamespace :: Lens' DescribeScalingPolicies ServiceNamespace
dServiceNamespace = lens _dServiceNamespace (\ s a -> s{_dServiceNamespace = a})

instance AWSPager DescribeScalingPolicies where
        page rq rs
          | stop (rs ^. drsNextToken) = Nothing
          | stop (rs ^. drsScalingPolicies) = Nothing
          | otherwise =
            Just $ rq & dNextToken .~ rs ^. drsNextToken

instance AWSRequest DescribeScalingPolicies where
        type Rs DescribeScalingPolicies =
             DescribeScalingPoliciesResponse
        request = postJSON applicationAutoScaling
        response
          = receiveJSON
              (\ s h x ->
                 DescribeScalingPoliciesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "ScalingPolicies" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeScalingPolicies where

instance NFData DescribeScalingPolicies where

instance ToHeaders DescribeScalingPolicies where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleFrontendService.DescribeScalingPolicies" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeScalingPolicies where
        toJSON DescribeScalingPolicies'{..}
          = object
              (catMaybes
                 [("PolicyNames" .=) <$> _dPolicyNames,
                  ("ScalableDimension" .=) <$> _dScalableDimension,
                  ("ResourceId" .=) <$> _dResourceId,
                  ("NextToken" .=) <$> _dNextToken,
                  ("MaxResults" .=) <$> _dMaxResults,
                  Just ("ServiceNamespace" .= _dServiceNamespace)])

instance ToPath DescribeScalingPolicies where
        toPath = const "/"

instance ToQuery DescribeScalingPolicies where
        toQuery = const mempty

-- | /See:/ 'describeScalingPoliciesResponse' smart constructor.
data DescribeScalingPoliciesResponse = DescribeScalingPoliciesResponse'
  { _drsNextToken       :: !(Maybe Text)
  , _drsScalingPolicies :: !(Maybe [ScalingPolicy])
  , _drsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScalingPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsNextToken' - The token required to get the next set of results. This value is @null@ if there are no more results to return.
--
-- * 'drsScalingPolicies' - Information about the scaling policies.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeScalingPoliciesResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeScalingPoliciesResponse
describeScalingPoliciesResponse pResponseStatus_ =
  DescribeScalingPoliciesResponse'
    { _drsNextToken = Nothing
    , _drsScalingPolicies = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
drsNextToken :: Lens' DescribeScalingPoliciesResponse (Maybe Text)
drsNextToken = lens _drsNextToken (\ s a -> s{_drsNextToken = a})

-- | Information about the scaling policies.
drsScalingPolicies :: Lens' DescribeScalingPoliciesResponse [ScalingPolicy]
drsScalingPolicies = lens _drsScalingPolicies (\ s a -> s{_drsScalingPolicies = a}) . _Default . _Coerce

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeScalingPoliciesResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeScalingPoliciesResponse where
