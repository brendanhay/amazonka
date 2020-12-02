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
-- Module      : Network.AWS.ApplicationAutoScaling.DeleteScalingPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Application Auto Scaling scaling policy.
--
--
-- Deleting a policy deletes the underlying alarm action, but does not delete the CloudWatch alarm associated with the scaling policy, even if it no longer has an associated action.
--
-- To create a scaling policy or update an existing one, see 'PutScalingPolicy' .
--
module Network.AWS.ApplicationAutoScaling.DeleteScalingPolicy
    (
    -- * Creating a Request
      deleteScalingPolicy
    , DeleteScalingPolicy
    -- * Request Lenses
    , dspPolicyName
    , dspServiceNamespace
    , dspResourceId
    , dspScalableDimension

    -- * Destructuring the Response
    , deleteScalingPolicyResponse
    , DeleteScalingPolicyResponse
    -- * Response Lenses
    , dsprsResponseStatus
    ) where

import Network.AWS.ApplicationAutoScaling.Types
import Network.AWS.ApplicationAutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteScalingPolicy' smart constructor.
data DeleteScalingPolicy = DeleteScalingPolicy'
  { _dspPolicyName        :: !Text
  , _dspServiceNamespace  :: !ServiceNamespace
  , _dspResourceId        :: !Text
  , _dspScalableDimension :: !ScalableDimension
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dspPolicyName' - The name of the scaling policy.
--
-- * 'dspServiceNamespace' - The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
--
-- * 'dspResourceId' - The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
--
-- * 'dspScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
deleteScalingPolicy
    :: Text -- ^ 'dspPolicyName'
    -> ServiceNamespace -- ^ 'dspServiceNamespace'
    -> Text -- ^ 'dspResourceId'
    -> ScalableDimension -- ^ 'dspScalableDimension'
    -> DeleteScalingPolicy
deleteScalingPolicy pPolicyName_ pServiceNamespace_ pResourceId_ pScalableDimension_ =
  DeleteScalingPolicy'
    { _dspPolicyName = pPolicyName_
    , _dspServiceNamespace = pServiceNamespace_
    , _dspResourceId = pResourceId_
    , _dspScalableDimension = pScalableDimension_
    }


-- | The name of the scaling policy.
dspPolicyName :: Lens' DeleteScalingPolicy Text
dspPolicyName = lens _dspPolicyName (\ s a -> s{_dspPolicyName = a})

-- | The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
dspServiceNamespace :: Lens' DeleteScalingPolicy ServiceNamespace
dspServiceNamespace = lens _dspServiceNamespace (\ s a -> s{_dspServiceNamespace = a})

-- | The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
dspResourceId :: Lens' DeleteScalingPolicy Text
dspResourceId = lens _dspResourceId (\ s a -> s{_dspResourceId = a})

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
dspScalableDimension :: Lens' DeleteScalingPolicy ScalableDimension
dspScalableDimension = lens _dspScalableDimension (\ s a -> s{_dspScalableDimension = a})

instance AWSRequest DeleteScalingPolicy where
        type Rs DeleteScalingPolicy =
             DeleteScalingPolicyResponse
        request = postJSON applicationAutoScaling
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteScalingPolicyResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteScalingPolicy where

instance NFData DeleteScalingPolicy where

instance ToHeaders DeleteScalingPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleFrontendService.DeleteScalingPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteScalingPolicy where
        toJSON DeleteScalingPolicy'{..}
          = object
              (catMaybes
                 [Just ("PolicyName" .= _dspPolicyName),
                  Just ("ServiceNamespace" .= _dspServiceNamespace),
                  Just ("ResourceId" .= _dspResourceId),
                  Just ("ScalableDimension" .= _dspScalableDimension)])

instance ToPath DeleteScalingPolicy where
        toPath = const "/"

instance ToQuery DeleteScalingPolicy where
        toQuery = const mempty

-- | /See:/ 'deleteScalingPolicyResponse' smart constructor.
newtype DeleteScalingPolicyResponse = DeleteScalingPolicyResponse'
  { _dsprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteScalingPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsprsResponseStatus' - -- | The response status code.
deleteScalingPolicyResponse
    :: Int -- ^ 'dsprsResponseStatus'
    -> DeleteScalingPolicyResponse
deleteScalingPolicyResponse pResponseStatus_ =
  DeleteScalingPolicyResponse' {_dsprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dsprsResponseStatus :: Lens' DeleteScalingPolicyResponse Int
dsprsResponseStatus = lens _dsprsResponseStatus (\ s a -> s{_dsprsResponseStatus = a})

instance NFData DeleteScalingPolicyResponse where
