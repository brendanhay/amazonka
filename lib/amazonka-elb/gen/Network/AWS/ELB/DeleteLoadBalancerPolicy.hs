{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DeleteLoadBalancerPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified policy from the specified load balancer. This policy must not be enabled for any listeners.
module Network.AWS.ELB.DeleteLoadBalancerPolicy
  ( -- * Creating a request
    DeleteLoadBalancerPolicy (..),
    mkDeleteLoadBalancerPolicy,

    -- ** Request lenses
    dPolicyName,
    dLoadBalancerName,

    -- * Destructuring the response
    DeleteLoadBalancerPolicyResponse (..),
    mkDeleteLoadBalancerPolicyResponse,

    -- ** Response lenses
    dlbprsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DeleteLoadBalancerPolicy.
--
-- /See:/ 'mkDeleteLoadBalancerPolicy' smart constructor.
data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy'
  { -- | The name of the policy.
    policyName :: Lude.Text,
    -- | The name of the load balancer.
    loadBalancerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLoadBalancerPolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the policy.
-- * 'loadBalancerName' - The name of the load balancer.
mkDeleteLoadBalancerPolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'loadBalancerName'
  Lude.Text ->
  DeleteLoadBalancerPolicy
mkDeleteLoadBalancerPolicy pPolicyName_ pLoadBalancerName_ =
  DeleteLoadBalancerPolicy'
    { policyName = pPolicyName_,
      loadBalancerName = pLoadBalancerName_
    }

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPolicyName :: Lens.Lens' DeleteLoadBalancerPolicy Lude.Text
dPolicyName = Lens.lens (policyName :: DeleteLoadBalancerPolicy -> Lude.Text) (\s a -> s {policyName = a} :: DeleteLoadBalancerPolicy)
{-# DEPRECATED dPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLoadBalancerName :: Lens.Lens' DeleteLoadBalancerPolicy Lude.Text
dLoadBalancerName = Lens.lens (loadBalancerName :: DeleteLoadBalancerPolicy -> Lude.Text) (\s a -> s {loadBalancerName = a} :: DeleteLoadBalancerPolicy)
{-# DEPRECATED dLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

instance Lude.AWSRequest DeleteLoadBalancerPolicy where
  type Rs DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicyResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "DeleteLoadBalancerPolicyResult"
      ( \s h x ->
          DeleteLoadBalancerPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLoadBalancerPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteLoadBalancerPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLoadBalancerPolicy where
  toQuery DeleteLoadBalancerPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteLoadBalancerPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "PolicyName" Lude.=: policyName,
        "LoadBalancerName" Lude.=: loadBalancerName
      ]

-- | Contains the output of DeleteLoadBalancerPolicy.
--
-- /See:/ 'mkDeleteLoadBalancerPolicyResponse' smart constructor.
newtype DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLoadBalancerPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteLoadBalancerPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLoadBalancerPolicyResponse
mkDeleteLoadBalancerPolicyResponse pResponseStatus_ =
  DeleteLoadBalancerPolicyResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbprsResponseStatus :: Lens.Lens' DeleteLoadBalancerPolicyResponse Lude.Int
dlbprsResponseStatus = Lens.lens (responseStatus :: DeleteLoadBalancerPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLoadBalancerPolicyResponse)
{-# DEPRECATED dlbprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
