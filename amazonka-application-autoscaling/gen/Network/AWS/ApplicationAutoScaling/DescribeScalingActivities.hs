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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , dsaScalableDimension
    , dsaResourceId
    , dsaNextToken
    , dsaMaxResults
    , dsaServiceNamespace

    -- * Destructuring the Response
    , describeScalingActivitiesResponse
    , DescribeScalingActivitiesResponse
    -- * Response Lenses
    , dsarsScalingActivities
    , dsarsNextToken
    , dsarsResponseStatus
    ) where

import           Network.AWS.ApplicationAutoScaling.Types
import           Network.AWS.ApplicationAutoScaling.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeScalingActivities' smart constructor.
data DescribeScalingActivities = DescribeScalingActivities'
    { _dsaScalableDimension :: !(Maybe ScalableDimension)
    , _dsaResourceId        :: !(Maybe Text)
    , _dsaNextToken         :: !(Maybe Text)
    , _dsaMaxResults        :: !(Maybe Int)
    , _dsaServiceNamespace  :: !ServiceNamespace
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeScalingActivities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsaScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property. If you specify a scalable dimension, you must also specify a resource ID.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.
--
-- * 'dsaResourceId' - The identifier of the resource associated with the scaling activity. This string consists of the resource type and unique identifier. If you specify a scalable dimension, you must also specify a resource ID.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .
--
-- * 'dsaNextToken' - The token for the next set of results.
--
-- * 'dsaMaxResults' - The maximum number of scalable target results. This value can be between 1 and 50. The default value is 50. If this parameter is used, the operation returns up to @MaxResults@ results at a time, along with a @NextToken@ value. To get the next set of results, include the @NextToken@ value in a subsequent call. If this parameter is not used, the operation returns up to 50 results and a @NextToken@ value, if applicable.
--
-- * 'dsaServiceNamespace' - The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
describeScalingActivities
    :: ServiceNamespace -- ^ 'dsaServiceNamespace'
    -> DescribeScalingActivities
describeScalingActivities pServiceNamespace_ =
    DescribeScalingActivities'
    { _dsaScalableDimension = Nothing
    , _dsaResourceId = Nothing
    , _dsaNextToken = Nothing
    , _dsaMaxResults = Nothing
    , _dsaServiceNamespace = pServiceNamespace_
    }

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property. If you specify a scalable dimension, you must also specify a resource ID.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.
dsaScalableDimension :: Lens' DescribeScalingActivities (Maybe ScalableDimension)
dsaScalableDimension = lens _dsaScalableDimension (\ s a -> s{_dsaScalableDimension = a});

-- | The identifier of the resource associated with the scaling activity. This string consists of the resource type and unique identifier. If you specify a scalable dimension, you must also specify a resource ID.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .
dsaResourceId :: Lens' DescribeScalingActivities (Maybe Text)
dsaResourceId = lens _dsaResourceId (\ s a -> s{_dsaResourceId = a});

-- | The token for the next set of results.
dsaNextToken :: Lens' DescribeScalingActivities (Maybe Text)
dsaNextToken = lens _dsaNextToken (\ s a -> s{_dsaNextToken = a});

-- | The maximum number of scalable target results. This value can be between 1 and 50. The default value is 50. If this parameter is used, the operation returns up to @MaxResults@ results at a time, along with a @NextToken@ value. To get the next set of results, include the @NextToken@ value in a subsequent call. If this parameter is not used, the operation returns up to 50 results and a @NextToken@ value, if applicable.
dsaMaxResults :: Lens' DescribeScalingActivities (Maybe Int)
dsaMaxResults = lens _dsaMaxResults (\ s a -> s{_dsaMaxResults = a});

-- | The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
dsaServiceNamespace :: Lens' DescribeScalingActivities ServiceNamespace
dsaServiceNamespace = lens _dsaServiceNamespace (\ s a -> s{_dsaServiceNamespace = a});

instance AWSPager DescribeScalingActivities where
        page rq rs
          | stop (rs ^. dsarsNextToken) = Nothing
          | stop (rs ^. dsarsScalingActivities) = Nothing
          | otherwise =
            Just $ rq & dsaNextToken .~ rs ^. dsarsNextToken

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

instance Hashable DescribeScalingActivities

instance NFData DescribeScalingActivities

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
                 [("ScalableDimension" .=) <$> _dsaScalableDimension,
                  ("ResourceId" .=) <$> _dsaResourceId,
                  ("NextToken" .=) <$> _dsaNextToken,
                  ("MaxResults" .=) <$> _dsaMaxResults,
                  Just ("ServiceNamespace" .= _dsaServiceNamespace)])

instance ToPath DescribeScalingActivities where
        toPath = const "/"

instance ToQuery DescribeScalingActivities where
        toQuery = const mempty

-- | /See:/ 'describeScalingActivitiesResponse' smart constructor.
data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse'
    { _dsarsScalingActivities :: !(Maybe [ScalingActivity])
    , _dsarsNextToken         :: !(Maybe Text)
    , _dsarsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeScalingActivitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsarsScalingActivities' - A list of scaling activity objects.
--
-- * 'dsarsNextToken' - The token required to get the next set of results. This value is @null@ if there are no more results to return.
--
-- * 'dsarsResponseStatus' - -- | The response status code.
describeScalingActivitiesResponse
    :: Int -- ^ 'dsarsResponseStatus'
    -> DescribeScalingActivitiesResponse
describeScalingActivitiesResponse pResponseStatus_ =
    DescribeScalingActivitiesResponse'
    { _dsarsScalingActivities = Nothing
    , _dsarsNextToken = Nothing
    , _dsarsResponseStatus = pResponseStatus_
    }

-- | A list of scaling activity objects.
dsarsScalingActivities :: Lens' DescribeScalingActivitiesResponse [ScalingActivity]
dsarsScalingActivities = lens _dsarsScalingActivities (\ s a -> s{_dsarsScalingActivities = a}) . _Default . _Coerce;

-- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
dsarsNextToken :: Lens' DescribeScalingActivitiesResponse (Maybe Text)
dsarsNextToken = lens _dsarsNextToken (\ s a -> s{_dsarsNextToken = a});

-- | -- | The response status code.
dsarsResponseStatus :: Lens' DescribeScalingActivitiesResponse Int
dsarsResponseStatus = lens _dsarsResponseStatus (\ s a -> s{_dsarsResponseStatus = a});

instance NFData DescribeScalingActivitiesResponse
