{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.AttachInstancesToLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more Lightsail instances to a load balancer.
--
-- After some time, the instances are attached to the load balancer and the health check status is available.
-- The @attach instances to load balancer@ operation supports tag-based access control via resource tags applied to the resource identified by @load balancer name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.AttachInstancesToLoadBalancer
  ( -- * Creating a request
    AttachInstancesToLoadBalancer (..),
    mkAttachInstancesToLoadBalancer,

    -- ** Request lenses
    aitlbLoadBalancerName,
    aitlbInstanceNames,

    -- * Destructuring the response
    AttachInstancesToLoadBalancerResponse (..),
    mkAttachInstancesToLoadBalancerResponse,

    -- ** Response lenses
    aitlbrsOperations,
    aitlbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachInstancesToLoadBalancer' smart constructor.
data AttachInstancesToLoadBalancer = AttachInstancesToLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Lude.Text,
    -- | An array of strings representing the instance name(s) you want to attach to your load balancer.
    --
    -- An instance must be @running@ before you can attach it to your load balancer.
    -- There are no additional limits on the number of instances you can attach to your load balancer, aside from the limit of Lightsail instances you can create in your account (20).
    instanceNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachInstancesToLoadBalancer' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'instanceNames' - An array of strings representing the instance name(s) you want to attach to your load balancer.
--
-- An instance must be @running@ before you can attach it to your load balancer.
-- There are no additional limits on the number of instances you can attach to your load balancer, aside from the limit of Lightsail instances you can create in your account (20).
mkAttachInstancesToLoadBalancer ::
  -- | 'loadBalancerName'
  Lude.Text ->
  AttachInstancesToLoadBalancer
mkAttachInstancesToLoadBalancer pLoadBalancerName_ =
  AttachInstancesToLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_,
      instanceNames = Lude.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aitlbLoadBalancerName :: Lens.Lens' AttachInstancesToLoadBalancer Lude.Text
aitlbLoadBalancerName = Lens.lens (loadBalancerName :: AttachInstancesToLoadBalancer -> Lude.Text) (\s a -> s {loadBalancerName = a} :: AttachInstancesToLoadBalancer)
{-# DEPRECATED aitlbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | An array of strings representing the instance name(s) you want to attach to your load balancer.
--
-- An instance must be @running@ before you can attach it to your load balancer.
-- There are no additional limits on the number of instances you can attach to your load balancer, aside from the limit of Lightsail instances you can create in your account (20).
--
-- /Note:/ Consider using 'instanceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aitlbInstanceNames :: Lens.Lens' AttachInstancesToLoadBalancer [Lude.Text]
aitlbInstanceNames = Lens.lens (instanceNames :: AttachInstancesToLoadBalancer -> [Lude.Text]) (\s a -> s {instanceNames = a} :: AttachInstancesToLoadBalancer)
{-# DEPRECATED aitlbInstanceNames "Use generic-lens or generic-optics with 'instanceNames' instead." #-}

instance Lude.AWSRequest AttachInstancesToLoadBalancer where
  type
    Rs AttachInstancesToLoadBalancer =
      AttachInstancesToLoadBalancerResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          AttachInstancesToLoadBalancerResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachInstancesToLoadBalancer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.AttachInstancesToLoadBalancer" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AttachInstancesToLoadBalancer where
  toJSON AttachInstancesToLoadBalancer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("loadBalancerName" Lude..= loadBalancerName),
            Lude.Just ("instanceNames" Lude..= instanceNames)
          ]
      )

instance Lude.ToPath AttachInstancesToLoadBalancer where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachInstancesToLoadBalancer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAttachInstancesToLoadBalancerResponse' smart constructor.
data AttachInstancesToLoadBalancerResponse = AttachInstancesToLoadBalancerResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachInstancesToLoadBalancerResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkAttachInstancesToLoadBalancerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachInstancesToLoadBalancerResponse
mkAttachInstancesToLoadBalancerResponse pResponseStatus_ =
  AttachInstancesToLoadBalancerResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aitlbrsOperations :: Lens.Lens' AttachInstancesToLoadBalancerResponse (Lude.Maybe [Operation])
aitlbrsOperations = Lens.lens (operations :: AttachInstancesToLoadBalancerResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: AttachInstancesToLoadBalancerResponse)
{-# DEPRECATED aitlbrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aitlbrsResponseStatus :: Lens.Lens' AttachInstancesToLoadBalancerResponse Lude.Int
aitlbrsResponseStatus = Lens.lens (responseStatus :: AttachInstancesToLoadBalancerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachInstancesToLoadBalancerResponse)
{-# DEPRECATED aitlbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
