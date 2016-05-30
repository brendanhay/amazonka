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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters a scalable target that was previously registered. If you are
-- no longer using a scalable target, you can delete it with this
-- operation. When you deregister a scalable target, all of the scaling
-- policies that are associated with that scalable target are deleted.
--
-- To create a new scalable target or update an existing one, see
-- < RegisterScalableTarget>.
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

import           Network.AWS.ApplicationAutoScaling.Types
import           Network.AWS.ApplicationAutoScaling.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deregisterScalableTarget' smart constructor.
data DeregisterScalableTarget = DeregisterScalableTarget'
    { _derServiceNamespace  :: !ServiceNamespace
    , _derResourceId        :: !Text
    , _derScalableDimension :: !ScalableDimension
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeregisterScalableTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'derServiceNamespace'
--
-- * 'derResourceId'
--
-- * 'derScalableDimension'
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

-- | The namespace for the AWS service that the scalable target is associated
-- with. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces>
-- in the Amazon Web Services General Reference.
derServiceNamespace :: Lens' DeregisterScalableTarget ServiceNamespace
derServiceNamespace = lens _derServiceNamespace (\ s a -> s{_derServiceNamespace = a});

-- | The unique identifier string for the resource associated with the
-- scalable target. For Amazon ECS services, this value is the resource
-- type, followed by the cluster name and service name, such as
-- 'service\/default\/sample-webapp'.
derResourceId :: Lens' DeregisterScalableTarget Text
derResourceId = lens _derResourceId (\ s a -> s{_derResourceId = a});

-- | The scalable dimension associated with the scalable target. The scalable
-- dimension contains the service namespace, resource type, and scaling
-- property, such as 'ecs:service:DesiredCount' for the desired task count
-- of an Amazon ECS service.
derScalableDimension :: Lens' DeregisterScalableTarget ScalableDimension
derScalableDimension = lens _derScalableDimension (\ s a -> s{_derScalableDimension = a});

instance AWSRequest DeregisterScalableTarget where
        type Rs DeregisterScalableTarget =
             DeregisterScalableTargetResponse
        request = postJSON applicationAutoScaling
        response
          = receiveEmpty
              (\ s h x ->
                 DeregisterScalableTargetResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeregisterScalableTarget

instance NFData DeregisterScalableTarget

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeregisterScalableTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dstrsResponseStatus'
deregisterScalableTargetResponse
    :: Int -- ^ 'dstrsResponseStatus'
    -> DeregisterScalableTargetResponse
deregisterScalableTargetResponse pResponseStatus_ =
    DeregisterScalableTargetResponse'
    { _dstrsResponseStatus = pResponseStatus_
    }

-- | The response status code.
dstrsResponseStatus :: Lens' DeregisterScalableTargetResponse Int
dstrsResponseStatus = lens _dstrsResponseStatus (\ s a -> s{_dstrsResponseStatus = a});

instance NFData DeregisterScalableTargetResponse
