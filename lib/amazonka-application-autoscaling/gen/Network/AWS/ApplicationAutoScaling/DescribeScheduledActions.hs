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
-- Module      : Network.AWS.ApplicationAutoScaling.DescribeScheduledActions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the scheduled actions for the specified service namespace.
--
--
-- You can filter the results using the @ResourceId@ , @ScalableDimension@ , and @ScheduledActionNames@ parameters.
--
-- To create a scheduled action or update an existing one, see 'PutScheduledAction' . If you are no longer using a scheduled action, you can delete it using 'DeleteScheduledAction' .
--
module Network.AWS.ApplicationAutoScaling.DescribeScheduledActions
    (
    -- * Creating a Request
      describeScheduledActions
    , DescribeScheduledActions
    -- * Request Lenses
    , dsasScalableDimension
    , dsasResourceId
    , dsasNextToken
    , dsasScheduledActionNames
    , dsasMaxResults
    , dsasServiceNamespace

    -- * Destructuring the Response
    , describeScheduledActionsResponse
    , DescribeScheduledActionsResponse
    -- * Response Lenses
    , dsarsNextToken
    , dsarsScheduledActions
    , dsarsResponseStatus
    ) where

import Network.AWS.ApplicationAutoScaling.Types
import Network.AWS.ApplicationAutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeScheduledActions' smart constructor.
data DescribeScheduledActions = DescribeScheduledActions'
  { _dsasScalableDimension    :: !(Maybe ScalableDimension)
  , _dsasResourceId           :: !(Maybe Text)
  , _dsasNextToken            :: !(Maybe Text)
  , _dsasScheduledActionNames :: !(Maybe [Text])
  , _dsasMaxResults           :: !(Maybe Int)
  , _dsasServiceNamespace     :: !ServiceNamespace
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScheduledActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsasScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property. If you specify a scalable dimension, you must also specify a resource ID.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
--
-- * 'dsasResourceId' - The identifier of the resource associated with the scheduled action. This string consists of the resource type and unique identifier. If you specify a scalable dimension, you must also specify a resource ID.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
--
-- * 'dsasNextToken' - The token for the next set of results.
--
-- * 'dsasScheduledActionNames' - The names of the scheduled actions to describe.
--
-- * 'dsasMaxResults' - The maximum number of scheduled action results. This value can be between 1 and 50. The default value is 50. If this parameter is used, the operation returns up to @MaxResults@ results at a time, along with a @NextToken@ value. To get the next set of results, include the @NextToken@ value in a subsequent call. If this parameter is not used, the operation returns up to 50 results and a @NextToken@ value, if applicable.
--
-- * 'dsasServiceNamespace' - The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
describeScheduledActions
    :: ServiceNamespace -- ^ 'dsasServiceNamespace'
    -> DescribeScheduledActions
describeScheduledActions pServiceNamespace_ =
  DescribeScheduledActions'
    { _dsasScalableDimension = Nothing
    , _dsasResourceId = Nothing
    , _dsasNextToken = Nothing
    , _dsasScheduledActionNames = Nothing
    , _dsasMaxResults = Nothing
    , _dsasServiceNamespace = pServiceNamespace_
    }


-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property. If you specify a scalable dimension, you must also specify a resource ID.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
dsasScalableDimension :: Lens' DescribeScheduledActions (Maybe ScalableDimension)
dsasScalableDimension = lens _dsasScalableDimension (\ s a -> s{_dsasScalableDimension = a})

-- | The identifier of the resource associated with the scheduled action. This string consists of the resource type and unique identifier. If you specify a scalable dimension, you must also specify a resource ID.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
dsasResourceId :: Lens' DescribeScheduledActions (Maybe Text)
dsasResourceId = lens _dsasResourceId (\ s a -> s{_dsasResourceId = a})

-- | The token for the next set of results.
dsasNextToken :: Lens' DescribeScheduledActions (Maybe Text)
dsasNextToken = lens _dsasNextToken (\ s a -> s{_dsasNextToken = a})

-- | The names of the scheduled actions to describe.
dsasScheduledActionNames :: Lens' DescribeScheduledActions [Text]
dsasScheduledActionNames = lens _dsasScheduledActionNames (\ s a -> s{_dsasScheduledActionNames = a}) . _Default . _Coerce

-- | The maximum number of scheduled action results. This value can be between 1 and 50. The default value is 50. If this parameter is used, the operation returns up to @MaxResults@ results at a time, along with a @NextToken@ value. To get the next set of results, include the @NextToken@ value in a subsequent call. If this parameter is not used, the operation returns up to 50 results and a @NextToken@ value, if applicable.
dsasMaxResults :: Lens' DescribeScheduledActions (Maybe Int)
dsasMaxResults = lens _dsasMaxResults (\ s a -> s{_dsasMaxResults = a})

-- | The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
dsasServiceNamespace :: Lens' DescribeScheduledActions ServiceNamespace
dsasServiceNamespace = lens _dsasServiceNamespace (\ s a -> s{_dsasServiceNamespace = a})

instance AWSRequest DescribeScheduledActions where
        type Rs DescribeScheduledActions =
             DescribeScheduledActionsResponse
        request = postJSON applicationAutoScaling
        response
          = receiveJSON
              (\ s h x ->
                 DescribeScheduledActionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "ScheduledActions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeScheduledActions where

instance NFData DescribeScheduledActions where

instance ToHeaders DescribeScheduledActions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleFrontendService.DescribeScheduledActions"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeScheduledActions where
        toJSON DescribeScheduledActions'{..}
          = object
              (catMaybes
                 [("ScalableDimension" .=) <$> _dsasScalableDimension,
                  ("ResourceId" .=) <$> _dsasResourceId,
                  ("NextToken" .=) <$> _dsasNextToken,
                  ("ScheduledActionNames" .=) <$>
                    _dsasScheduledActionNames,
                  ("MaxResults" .=) <$> _dsasMaxResults,
                  Just ("ServiceNamespace" .= _dsasServiceNamespace)])

instance ToPath DescribeScheduledActions where
        toPath = const "/"

instance ToQuery DescribeScheduledActions where
        toQuery = const mempty

-- | /See:/ 'describeScheduledActionsResponse' smart constructor.
data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse'
  { _dsarsNextToken        :: !(Maybe Text)
  , _dsarsScheduledActions :: !(Maybe [ScheduledAction])
  , _dsarsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScheduledActionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsarsNextToken' - The token required to get the next set of results. This value is @null@ if there are no more results to return.
--
-- * 'dsarsScheduledActions' - Information about the scheduled actions.
--
-- * 'dsarsResponseStatus' - -- | The response status code.
describeScheduledActionsResponse
    :: Int -- ^ 'dsarsResponseStatus'
    -> DescribeScheduledActionsResponse
describeScheduledActionsResponse pResponseStatus_ =
  DescribeScheduledActionsResponse'
    { _dsarsNextToken = Nothing
    , _dsarsScheduledActions = Nothing
    , _dsarsResponseStatus = pResponseStatus_
    }


-- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
dsarsNextToken :: Lens' DescribeScheduledActionsResponse (Maybe Text)
dsarsNextToken = lens _dsarsNextToken (\ s a -> s{_dsarsNextToken = a})

-- | Information about the scheduled actions.
dsarsScheduledActions :: Lens' DescribeScheduledActionsResponse [ScheduledAction]
dsarsScheduledActions = lens _dsarsScheduledActions (\ s a -> s{_dsarsScheduledActions = a}) . _Default . _Coerce

-- | -- | The response status code.
dsarsResponseStatus :: Lens' DescribeScheduledActionsResponse Int
dsarsResponseStatus = lens _dsarsResponseStatus (\ s a -> s{_dsarsResponseStatus = a})

instance NFData DescribeScheduledActionsResponse
         where
