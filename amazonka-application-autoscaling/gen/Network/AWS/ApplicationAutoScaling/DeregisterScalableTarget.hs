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
-- Module      : Network.AWS.ApplicationAutoScaling.DeregisterScalableTarget
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters a scalable target.
--
--
-- Deregistering a scalable target deletes the scaling policies that are associated with it.
--
-- To create a scalable target or update an existing one, see 'RegisterScalableTarget' .
--
module Network.AWS.ApplicationAutoScaling.DeregisterScalableTarget
    (
    -- * Creating a Request
      deregisterScalableTarget
    , DeregisterScalableTarget
    -- * Request Lenses
    , derServiceNamespace
    , derResourceId
    , derScalableDimension

    -- * Destructuring the Response
    , deregisterScalableTargetResponse
    , DeregisterScalableTargetResponse
    -- * Response Lenses
    , dstrsResponseStatus
    ) where

import Network.AWS.ApplicationAutoScaling.Types
import Network.AWS.ApplicationAutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterScalableTarget' smart constructor.
data DeregisterScalableTarget = DeregisterScalableTarget'
  { _derServiceNamespace  :: !ServiceNamespace
  , _derResourceId        :: !Text
  , _derScalableDimension :: !ScalableDimension
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterScalableTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'derServiceNamespace' - The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
--
-- * 'derResourceId' - The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
--
-- * 'derScalableDimension' - The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
deregisterScalableTarget
    :: ServiceNamespace -- ^ 'derServiceNamespace'
    -> Text -- ^ 'derResourceId'
    -> ScalableDimension -- ^ 'derScalableDimension'
    -> DeregisterScalableTarget
deregisterScalableTarget pServiceNamespace_ pResourceId_ pScalableDimension_ =
  DeregisterScalableTarget'
    { _derServiceNamespace = pServiceNamespace_
    , _derResourceId = pResourceId_
    , _derScalableDimension = pScalableDimension_
    }


-- | The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
derServiceNamespace :: Lens' DeregisterScalableTarget ServiceNamespace
derServiceNamespace = lens _derServiceNamespace (\ s a -> s{_derServiceNamespace = a})

-- | The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
derResourceId :: Lens' DeregisterScalableTarget Text
derResourceId = lens _derResourceId (\ s a -> s{_derResourceId = a})

-- | The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
derScalableDimension :: Lens' DeregisterScalableTarget ScalableDimension
derScalableDimension = lens _derScalableDimension (\ s a -> s{_derScalableDimension = a})

instance AWSRequest DeregisterScalableTarget where
        type Rs DeregisterScalableTarget =
             DeregisterScalableTargetResponse
        request = postJSON applicationAutoScaling
        response
          = receiveEmpty
              (\ s h x ->
                 DeregisterScalableTargetResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeregisterScalableTarget where

instance NFData DeregisterScalableTarget where

instance ToHeaders DeregisterScalableTarget where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleFrontendService.DeregisterScalableTarget"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterScalableTarget where
        toJSON DeregisterScalableTarget'{..}
          = object
              (catMaybes
                 [Just ("ServiceNamespace" .= _derServiceNamespace),
                  Just ("ResourceId" .= _derResourceId),
                  Just ("ScalableDimension" .= _derScalableDimension)])

instance ToPath DeregisterScalableTarget where
        toPath = const "/"

instance ToQuery DeregisterScalableTarget where
        toQuery = const mempty

-- | /See:/ 'deregisterScalableTargetResponse' smart constructor.
newtype DeregisterScalableTargetResponse = DeregisterScalableTargetResponse'
  { _dstrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterScalableTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dstrsResponseStatus' - -- | The response status code.
deregisterScalableTargetResponse
    :: Int -- ^ 'dstrsResponseStatus'
    -> DeregisterScalableTargetResponse
deregisterScalableTargetResponse pResponseStatus_ =
  DeregisterScalableTargetResponse' {_dstrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dstrsResponseStatus :: Lens' DeregisterScalableTargetResponse Int
dstrsResponseStatus = lens _dstrsResponseStatus (\ s a -> s{_dstrsResponseStatus = a})

instance NFData DeregisterScalableTargetResponse
         where
