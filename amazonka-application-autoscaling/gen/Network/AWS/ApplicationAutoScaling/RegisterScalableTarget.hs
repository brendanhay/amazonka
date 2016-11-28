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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers or updates a scalable target. A scalable target is a resource that can be scaled out or in with Application Auto Scaling. After you have registered a scalable target, you can use this operation to update the minimum and maximum values for your scalable dimension.
--
--
-- After you register a scalable target with Application Auto Scaling, you can create and apply scaling policies to it with 'PutScalingPolicy' . You can view the existing scaling policies for a service namespace with 'DescribeScalableTargets' . If you are no longer using a scalable target, you can deregister it with 'DeregisterScalableTarget' .
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

import           Network.AWS.ApplicationAutoScaling.Types
import           Network.AWS.ApplicationAutoScaling.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'registerScalableTarget' smart constructor.
data RegisterScalableTarget = RegisterScalableTarget'
    { _rstMaxCapacity       :: !(Maybe Int)
    , _rstMinCapacity       :: !(Maybe Int)
    , _rstRoleARN           :: !(Maybe Text)
    , _rstServiceNamespace  :: !ServiceNamespace
    , _rstResourceId        :: !Text
    , _rstScalableDimension :: !ScalableDimension
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterScalableTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rstMaxCapacity' - The maximum value for this scalable target to scale out to in response to scaling activities. This parameter is required if you are registering a new scalable target, and it is optional if you are updating an existing one.
--
-- * 'rstMinCapacity' - The minimum value for this scalable target to scale in to in response to scaling activities. This parameter is required if you are registering a new scalable target, and it is optional if you are updating an existing one.
--
-- * 'rstRoleARN' - The ARN of the IAM role that allows Application Auto Scaling to modify your scalable target on your behalf. This parameter is required if you are registering a new scalable target, and it is optional if you are updating an existing one.
--
-- * 'rstServiceNamespace' - The namespace for the AWS service that the scalable target is associated with. For Amazon ECS services, the namespace value is @ecs@ . For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the Amazon Web Services General Reference.
--
-- * 'rstResourceId' - The resource type and unique identifier string for the resource to associate with the scalable target. For Amazon ECS services, the resource type is @services@ , and the identifier is the cluster name and service name; for example, @service/default/sample-webapp@ . For Amazon EC2 Spot fleet requests, the resource type is @spot-fleet-request@ , and the identifier is the Spot fleet request ID; for example, @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .
--
-- * 'rstScalableDimension' - The scalable dimension associated with the scalable target. The scalable dimension contains the service namespace, resource type, and scaling property, such as @ecs:service:DesiredCount@ for the desired task count of an Amazon ECS service, or @ec2:spot-fleet-request:TargetCapacity@ for the target capacity of an Amazon EC2 Spot fleet request.
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

-- | The maximum value for this scalable target to scale out to in response to scaling activities. This parameter is required if you are registering a new scalable target, and it is optional if you are updating an existing one.
rstMaxCapacity :: Lens' RegisterScalableTarget (Maybe Int)
rstMaxCapacity = lens _rstMaxCapacity (\ s a -> s{_rstMaxCapacity = a});

-- | The minimum value for this scalable target to scale in to in response to scaling activities. This parameter is required if you are registering a new scalable target, and it is optional if you are updating an existing one.
rstMinCapacity :: Lens' RegisterScalableTarget (Maybe Int)
rstMinCapacity = lens _rstMinCapacity (\ s a -> s{_rstMinCapacity = a});

-- | The ARN of the IAM role that allows Application Auto Scaling to modify your scalable target on your behalf. This parameter is required if you are registering a new scalable target, and it is optional if you are updating an existing one.
rstRoleARN :: Lens' RegisterScalableTarget (Maybe Text)
rstRoleARN = lens _rstRoleARN (\ s a -> s{_rstRoleARN = a});

-- | The namespace for the AWS service that the scalable target is associated with. For Amazon ECS services, the namespace value is @ecs@ . For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the Amazon Web Services General Reference.
rstServiceNamespace :: Lens' RegisterScalableTarget ServiceNamespace
rstServiceNamespace = lens _rstServiceNamespace (\ s a -> s{_rstServiceNamespace = a});

-- | The resource type and unique identifier string for the resource to associate with the scalable target. For Amazon ECS services, the resource type is @services@ , and the identifier is the cluster name and service name; for example, @service/default/sample-webapp@ . For Amazon EC2 Spot fleet requests, the resource type is @spot-fleet-request@ , and the identifier is the Spot fleet request ID; for example, @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .
rstResourceId :: Lens' RegisterScalableTarget Text
rstResourceId = lens _rstResourceId (\ s a -> s{_rstResourceId = a});

-- | The scalable dimension associated with the scalable target. The scalable dimension contains the service namespace, resource type, and scaling property, such as @ecs:service:DesiredCount@ for the desired task count of an Amazon ECS service, or @ec2:spot-fleet-request:TargetCapacity@ for the target capacity of an Amazon EC2 Spot fleet request.
rstScalableDimension :: Lens' RegisterScalableTarget ScalableDimension
rstScalableDimension = lens _rstScalableDimension (\ s a -> s{_rstScalableDimension = a});

instance AWSRequest RegisterScalableTarget where
        type Rs RegisterScalableTarget =
             RegisterScalableTargetResponse
        request = postJSON applicationAutoScaling
        response
          = receiveEmpty
              (\ s h x ->
                 RegisterScalableTargetResponse' <$>
                   (pure (fromEnum s)))

instance Hashable RegisterScalableTarget

instance NFData RegisterScalableTarget

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterScalableTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rstrsResponseStatus' - -- | The response status code.
registerScalableTargetResponse
    :: Int -- ^ 'rstrsResponseStatus'
    -> RegisterScalableTargetResponse
registerScalableTargetResponse pResponseStatus_ =
    RegisterScalableTargetResponse'
    { _rstrsResponseStatus = pResponseStatus_
    }

-- | -- | The response status code.
rstrsResponseStatus :: Lens' RegisterScalableTargetResponse Int
rstrsResponseStatus = lens _rstrsResponseStatus (\ s a -> s{_rstrsResponseStatus = a});

instance NFData RegisterScalableTargetResponse
