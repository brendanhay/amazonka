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
-- Module      : Network.AWS.ApplicationAutoScaling.DeleteScheduledAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Application Auto Scaling scheduled action.
--
--
module Network.AWS.ApplicationAutoScaling.DeleteScheduledAction
    (
    -- * Creating a Request
      deleteScheduledAction
    , DeleteScheduledAction
    -- * Request Lenses
    , dsaServiceNamespace
    , dsaScheduledActionName
    , dsaResourceId
    , dsaScalableDimension

    -- * Destructuring the Response
    , deleteScheduledActionResponse
    , DeleteScheduledActionResponse
    -- * Response Lenses
    , delrsResponseStatus
    ) where

import Network.AWS.ApplicationAutoScaling.Types
import Network.AWS.ApplicationAutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteScheduledAction' smart constructor.
data DeleteScheduledAction = DeleteScheduledAction'
  { _dsaServiceNamespace    :: !ServiceNamespace
  , _dsaScheduledActionName :: !Text
  , _dsaResourceId          :: !Text
  , _dsaScalableDimension   :: !ScalableDimension
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteScheduledAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsaServiceNamespace' - The namespace of the AWS service that provides the resource or @custom-resource@ for a resource provided by your own application or service. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
--
-- * 'dsaScheduledActionName' - The name of the scheduled action.
--
-- * 'dsaResourceId' - The identifier of the resource associated with the scheduled action. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .
--
-- * 'dsaScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.
deleteScheduledAction
    :: ServiceNamespace -- ^ 'dsaServiceNamespace'
    -> Text -- ^ 'dsaScheduledActionName'
    -> Text -- ^ 'dsaResourceId'
    -> ScalableDimension -- ^ 'dsaScalableDimension'
    -> DeleteScheduledAction
deleteScheduledAction pServiceNamespace_ pScheduledActionName_ pResourceId_ pScalableDimension_ =
  DeleteScheduledAction'
    { _dsaServiceNamespace = pServiceNamespace_
    , _dsaScheduledActionName = pScheduledActionName_
    , _dsaResourceId = pResourceId_
    , _dsaScalableDimension = pScalableDimension_
    }


-- | The namespace of the AWS service that provides the resource or @custom-resource@ for a resource provided by your own application or service. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
dsaServiceNamespace :: Lens' DeleteScheduledAction ServiceNamespace
dsaServiceNamespace = lens _dsaServiceNamespace (\ s a -> s{_dsaServiceNamespace = a})

-- | The name of the scheduled action.
dsaScheduledActionName :: Lens' DeleteScheduledAction Text
dsaScheduledActionName = lens _dsaScheduledActionName (\ s a -> s{_dsaScheduledActionName = a})

-- | The identifier of the resource associated with the scheduled action. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .
dsaResourceId :: Lens' DeleteScheduledAction Text
dsaResourceId = lens _dsaResourceId (\ s a -> s{_dsaResourceId = a})

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.
dsaScalableDimension :: Lens' DeleteScheduledAction ScalableDimension
dsaScalableDimension = lens _dsaScalableDimension (\ s a -> s{_dsaScalableDimension = a})

instance AWSRequest DeleteScheduledAction where
        type Rs DeleteScheduledAction =
             DeleteScheduledActionResponse
        request = postJSON applicationAutoScaling
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteScheduledActionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteScheduledAction where

instance NFData DeleteScheduledAction where

instance ToHeaders DeleteScheduledAction where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleFrontendService.DeleteScheduledAction" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteScheduledAction where
        toJSON DeleteScheduledAction'{..}
          = object
              (catMaybes
                 [Just ("ServiceNamespace" .= _dsaServiceNamespace),
                  Just
                    ("ScheduledActionName" .= _dsaScheduledActionName),
                  Just ("ResourceId" .= _dsaResourceId),
                  Just ("ScalableDimension" .= _dsaScalableDimension)])

instance ToPath DeleteScheduledAction where
        toPath = const "/"

instance ToQuery DeleteScheduledAction where
        toQuery = const mempty

-- | /See:/ 'deleteScheduledActionResponse' smart constructor.
newtype DeleteScheduledActionResponse = DeleteScheduledActionResponse'
  { _delrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteScheduledActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteScheduledActionResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteScheduledActionResponse
deleteScheduledActionResponse pResponseStatus_ =
  DeleteScheduledActionResponse' {_delrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteScheduledActionResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeleteScheduledActionResponse where
