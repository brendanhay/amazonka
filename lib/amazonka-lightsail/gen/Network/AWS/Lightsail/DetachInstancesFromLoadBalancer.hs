{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DetachInstancesFromLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches the specified instances from a Lightsail load balancer.
--
-- This operation waits until the instances are no longer needed before they are detached from the load balancer.
-- The @detach instances from load balancer@ operation supports tag-based access control via resource tags applied to the resource identified by @load balancer name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DetachInstancesFromLoadBalancer
  ( -- * Creating a request
    DetachInstancesFromLoadBalancer (..),
    mkDetachInstancesFromLoadBalancer,

    -- ** Request lenses
    diflbLoadBalancerName,
    diflbInstanceNames,

    -- * Destructuring the response
    DetachInstancesFromLoadBalancerResponse (..),
    mkDetachInstancesFromLoadBalancerResponse,

    -- ** Response lenses
    diflbrsOperations,
    diflbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachInstancesFromLoadBalancer' smart constructor.
data DetachInstancesFromLoadBalancer = DetachInstancesFromLoadBalancer'
  { loadBalancerName ::
      Lude.Text,
    instanceNames ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachInstancesFromLoadBalancer' with the minimum fields required to make a request.
--
-- * 'instanceNames' - An array of strings containing the names of the instances you want to detach from the load balancer.
-- * 'loadBalancerName' - The name of the Lightsail load balancer.
mkDetachInstancesFromLoadBalancer ::
  -- | 'loadBalancerName'
  Lude.Text ->
  DetachInstancesFromLoadBalancer
mkDetachInstancesFromLoadBalancer pLoadBalancerName_ =
  DetachInstancesFromLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_,
      instanceNames = Lude.mempty
    }

-- | The name of the Lightsail load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbLoadBalancerName :: Lens.Lens' DetachInstancesFromLoadBalancer Lude.Text
diflbLoadBalancerName = Lens.lens (loadBalancerName :: DetachInstancesFromLoadBalancer -> Lude.Text) (\s a -> s {loadBalancerName = a} :: DetachInstancesFromLoadBalancer)
{-# DEPRECATED diflbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | An array of strings containing the names of the instances you want to detach from the load balancer.
--
-- /Note:/ Consider using 'instanceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbInstanceNames :: Lens.Lens' DetachInstancesFromLoadBalancer [Lude.Text]
diflbInstanceNames = Lens.lens (instanceNames :: DetachInstancesFromLoadBalancer -> [Lude.Text]) (\s a -> s {instanceNames = a} :: DetachInstancesFromLoadBalancer)
{-# DEPRECATED diflbInstanceNames "Use generic-lens or generic-optics with 'instanceNames' instead." #-}

instance Lude.AWSRequest DetachInstancesFromLoadBalancer where
  type
    Rs DetachInstancesFromLoadBalancer =
      DetachInstancesFromLoadBalancerResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetachInstancesFromLoadBalancerResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetachInstancesFromLoadBalancer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.DetachInstancesFromLoadBalancer" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetachInstancesFromLoadBalancer where
  toJSON DetachInstancesFromLoadBalancer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("loadBalancerName" Lude..= loadBalancerName),
            Lude.Just ("instanceNames" Lude..= instanceNames)
          ]
      )

instance Lude.ToPath DetachInstancesFromLoadBalancer where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachInstancesFromLoadBalancer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetachInstancesFromLoadBalancerResponse' smart constructor.
data DetachInstancesFromLoadBalancerResponse = DetachInstancesFromLoadBalancerResponse'
  { operations ::
      Lude.Maybe
        [Operation],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachInstancesFromLoadBalancerResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDetachInstancesFromLoadBalancerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetachInstancesFromLoadBalancerResponse
mkDetachInstancesFromLoadBalancerResponse pResponseStatus_ =
  DetachInstancesFromLoadBalancerResponse'
    { operations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbrsOperations :: Lens.Lens' DetachInstancesFromLoadBalancerResponse (Lude.Maybe [Operation])
diflbrsOperations = Lens.lens (operations :: DetachInstancesFromLoadBalancerResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DetachInstancesFromLoadBalancerResponse)
{-# DEPRECATED diflbrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbrsResponseStatus :: Lens.Lens' DetachInstancesFromLoadBalancerResponse Lude.Int
diflbrsResponseStatus = Lens.lens (responseStatus :: DetachInstancesFromLoadBalancerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetachInstancesFromLoadBalancerResponse)
{-# DEPRECATED diflbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
