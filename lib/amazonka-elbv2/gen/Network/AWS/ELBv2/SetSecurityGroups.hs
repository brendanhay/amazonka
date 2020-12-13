{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.SetSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified security groups with the specified Application Load Balancer. The specified security groups override the previously associated security groups.
--
-- You can't specify a security group for a Network Load Balancer or Gateway Load Balancer.
module Network.AWS.ELBv2.SetSecurityGroups
  ( -- * Creating a request
    SetSecurityGroups (..),
    mkSetSecurityGroups,

    -- ** Request lenses
    ssgSecurityGroups,
    ssgLoadBalancerARN,

    -- * Destructuring the response
    SetSecurityGroupsResponse (..),
    mkSetSecurityGroupsResponse,

    -- ** Response lenses
    ssgrsSecurityGroupIds,
    ssgrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetSecurityGroups' smart constructor.
data SetSecurityGroups = SetSecurityGroups'
  { -- | The IDs of the security groups.
    securityGroups :: [Lude.Text],
    -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetSecurityGroups' with the minimum fields required to make a request.
--
-- * 'securityGroups' - The IDs of the security groups.
-- * 'loadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
mkSetSecurityGroups ::
  -- | 'loadBalancerARN'
  Lude.Text ->
  SetSecurityGroups
mkSetSecurityGroups pLoadBalancerARN_ =
  SetSecurityGroups'
    { securityGroups = Lude.mempty,
      loadBalancerARN = pLoadBalancerARN_
    }

-- | The IDs of the security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgSecurityGroups :: Lens.Lens' SetSecurityGroups [Lude.Text]
ssgSecurityGroups = Lens.lens (securityGroups :: SetSecurityGroups -> [Lude.Text]) (\s a -> s {securityGroups = a} :: SetSecurityGroups)
{-# DEPRECATED ssgSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgLoadBalancerARN :: Lens.Lens' SetSecurityGroups Lude.Text
ssgLoadBalancerARN = Lens.lens (loadBalancerARN :: SetSecurityGroups -> Lude.Text) (\s a -> s {loadBalancerARN = a} :: SetSecurityGroups)
{-# DEPRECATED ssgLoadBalancerARN "Use generic-lens or generic-optics with 'loadBalancerARN' instead." #-}

instance Lude.AWSRequest SetSecurityGroups where
  type Rs SetSecurityGroups = SetSecurityGroupsResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "SetSecurityGroupsResult"
      ( \s h x ->
          SetSecurityGroupsResponse'
            Lude.<$> ( x Lude..@? "SecurityGroupIds" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetSecurityGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetSecurityGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery SetSecurityGroups where
  toQuery SetSecurityGroups' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetSecurityGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "SecurityGroups" Lude.=: Lude.toQueryList "member" securityGroups,
        "LoadBalancerArn" Lude.=: loadBalancerARN
      ]

-- | /See:/ 'mkSetSecurityGroupsResponse' smart constructor.
data SetSecurityGroupsResponse = SetSecurityGroupsResponse'
  { -- | The IDs of the security groups associated with the load balancer.
    securityGroupIds :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetSecurityGroupsResponse' with the minimum fields required to make a request.
--
-- * 'securityGroupIds' - The IDs of the security groups associated with the load balancer.
-- * 'responseStatus' - The response status code.
mkSetSecurityGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetSecurityGroupsResponse
mkSetSecurityGroupsResponse pResponseStatus_ =
  SetSecurityGroupsResponse'
    { securityGroupIds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The IDs of the security groups associated with the load balancer.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgrsSecurityGroupIds :: Lens.Lens' SetSecurityGroupsResponse (Lude.Maybe [Lude.Text])
ssgrsSecurityGroupIds = Lens.lens (securityGroupIds :: SetSecurityGroupsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: SetSecurityGroupsResponse)
{-# DEPRECATED ssgrsSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgrsResponseStatus :: Lens.Lens' SetSecurityGroupsResponse Lude.Int
ssgrsResponseStatus = Lens.lens (responseStatus :: SetSecurityGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetSecurityGroupsResponse)
{-# DEPRECATED ssgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
