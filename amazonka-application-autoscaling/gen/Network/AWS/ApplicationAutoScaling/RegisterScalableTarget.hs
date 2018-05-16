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
-- Module      : Network.AWS.ApplicationAutoScaling.RegisterScalableTarget
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers or updates a scalable target. A scalable target is a resource that Application Auto Scaling can scale out or scale in. After you have registered a scalable target, you can use this operation to update the minimum and maximum values for its scalable dimension.
--
--
-- After you register a scalable target, you can create and apply scaling policies using 'PutScalingPolicy' . You can view the scaling policies for a service namespace using 'DescribeScalableTargets' . If you no longer need a scalable target, you can deregister it using 'DeregisterScalableTarget' .
--
module Network.AWS.ApplicationAutoScaling.RegisterScalableTarget
    (
    -- * Creating a Request
      registerScalableTarget
    , RegisterScalableTarget
    -- * Request Lenses
    , rstMaxCapacity
    , rstMinCapacity
    , rstRoleARN
    , rstServiceNamespace
    , rstResourceId
    , rstScalableDimension

    -- * Destructuring the Response
    , registerScalableTargetResponse
    , RegisterScalableTargetResponse
    -- * Response Lenses
    , rstrsResponseStatus
    ) where

import Network.AWS.ApplicationAutoScaling.Types
import Network.AWS.ApplicationAutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerScalableTarget' smart constructor.
data RegisterScalableTarget = RegisterScalableTarget'
  { _rstMaxCapacity       :: !(Maybe Int)
  , _rstMinCapacity       :: !(Maybe Int)
  , _rstRoleARN           :: !(Maybe Text)
  , _rstServiceNamespace  :: !ServiceNamespace
  , _rstResourceId        :: !Text
  , _rstScalableDimension :: !ScalableDimension
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterScalableTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rstMaxCapacity' - The maximum value to scale to in response to a scale out event. This parameter is required if you are registering a scalable target.
--
-- * 'rstMinCapacity' - The minimum value to scale to in response to a scale in event. This parameter is required if you are registering a scalable target.
--
-- * 'rstRoleARN' - Application Auto Scaling creates a service-linked role that grants it permissions to modify the scalable target on your behalf. For more information, see <http://docs.aws.amazon.com/ApplicationAutoScaling/latest/APIReference/application-autoscaling-service-linked-roles.html Service-Linked Roles for Application Auto Scaling> . For resources that are not supported using a service-linked role, this parameter is required and must specify the ARN of an IAM role that allows Application Auto Scaling to modify the scalable target on your behalf.
--
-- * 'rstServiceNamespace' - The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
--
-- * 'rstResourceId' - The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
--
-- * 'rstScalableDimension' - The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
registerScalableTarget
    :: ServiceNamespace -- ^ 'rstServiceNamespace'
    -> Text -- ^ 'rstResourceId'
    -> ScalableDimension -- ^ 'rstScalableDimension'
    -> RegisterScalableTarget
registerScalableTarget pServiceNamespace_ pResourceId_ pScalableDimension_ =
  RegisterScalableTarget'
    { _rstMaxCapacity = Nothing
    , _rstMinCapacity = Nothing
    , _rstRoleARN = Nothing
    , _rstServiceNamespace = pServiceNamespace_
    , _rstResourceId = pResourceId_
    , _rstScalableDimension = pScalableDimension_
    }


-- | The maximum value to scale to in response to a scale out event. This parameter is required if you are registering a scalable target.
rstMaxCapacity :: Lens' RegisterScalableTarget (Maybe Int)
rstMaxCapacity = lens _rstMaxCapacity (\ s a -> s{_rstMaxCapacity = a})

-- | The minimum value to scale to in response to a scale in event. This parameter is required if you are registering a scalable target.
rstMinCapacity :: Lens' RegisterScalableTarget (Maybe Int)
rstMinCapacity = lens _rstMinCapacity (\ s a -> s{_rstMinCapacity = a})

-- | Application Auto Scaling creates a service-linked role that grants it permissions to modify the scalable target on your behalf. For more information, see <http://docs.aws.amazon.com/ApplicationAutoScaling/latest/APIReference/application-autoscaling-service-linked-roles.html Service-Linked Roles for Application Auto Scaling> . For resources that are not supported using a service-linked role, this parameter is required and must specify the ARN of an IAM role that allows Application Auto Scaling to modify the scalable target on your behalf.
rstRoleARN :: Lens' RegisterScalableTarget (Maybe Text)
rstRoleARN = lens _rstRoleARN (\ s a -> s{_rstRoleARN = a})

-- | The namespace of the AWS service. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /Amazon Web Services General Reference/ .
rstServiceNamespace :: Lens' RegisterScalableTarget ServiceNamespace
rstServiceNamespace = lens _rstServiceNamespace (\ s a -> s{_rstServiceNamespace = a})

-- | The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variants - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
rstResourceId :: Lens' RegisterScalableTarget Text
rstResourceId = lens _rstResourceId (\ s a -> s{_rstResourceId = a})

-- | The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
rstScalableDimension :: Lens' RegisterScalableTarget ScalableDimension
rstScalableDimension = lens _rstScalableDimension (\ s a -> s{_rstScalableDimension = a})

instance AWSRequest RegisterScalableTarget where
        type Rs RegisterScalableTarget =
             RegisterScalableTargetResponse
        request = postJSON applicationAutoScaling
        response
          = receiveEmpty
              (\ s h x ->
                 RegisterScalableTargetResponse' <$>
                   (pure (fromEnum s)))

instance Hashable RegisterScalableTarget where

instance NFData RegisterScalableTarget where

instance ToHeaders RegisterScalableTarget where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleFrontendService.RegisterScalableTarget" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterScalableTarget where
        toJSON RegisterScalableTarget'{..}
          = object
              (catMaybes
                 [("MaxCapacity" .=) <$> _rstMaxCapacity,
                  ("MinCapacity" .=) <$> _rstMinCapacity,
                  ("RoleARN" .=) <$> _rstRoleARN,
                  Just ("ServiceNamespace" .= _rstServiceNamespace),
                  Just ("ResourceId" .= _rstResourceId),
                  Just ("ScalableDimension" .= _rstScalableDimension)])

instance ToPath RegisterScalableTarget where
        toPath = const "/"

instance ToQuery RegisterScalableTarget where
        toQuery = const mempty

-- | /See:/ 'registerScalableTargetResponse' smart constructor.
newtype RegisterScalableTargetResponse = RegisterScalableTargetResponse'
  { _rstrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterScalableTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rstrsResponseStatus' - -- | The response status code.
registerScalableTargetResponse
    :: Int -- ^ 'rstrsResponseStatus'
    -> RegisterScalableTargetResponse
registerScalableTargetResponse pResponseStatus_ =
  RegisterScalableTargetResponse' {_rstrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rstrsResponseStatus :: Lens' RegisterScalableTargetResponse Int
rstrsResponseStatus = lens _rstrsResponseStatus (\ s a -> s{_rstrsResponseStatus = a})

instance NFData RegisterScalableTargetResponse where
