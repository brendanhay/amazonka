{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RevokeClientVPNIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an ingress authorization rule from a Client VPN endpoint.
module Network.AWS.EC2.RevokeClientVPNIngress
  ( -- * Creating a request
    RevokeClientVPNIngress (..),
    mkRevokeClientVPNIngress,

    -- ** Request lenses
    rcviTargetNetworkCidr,
    rcviAccessGroupId,
    rcviClientVPNEndpointId,
    rcviRevokeAllGroups,
    rcviDryRun,

    -- * Destructuring the response
    RevokeClientVPNIngressResponse (..),
    mkRevokeClientVPNIngressResponse,

    -- ** Response lenses
    rcvirsStatus,
    rcvirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRevokeClientVPNIngress' smart constructor.
data RevokeClientVPNIngress = RevokeClientVPNIngress'
  { -- | The IPv4 address range, in CIDR notation, of the network for which access is being removed.
    targetNetworkCidr :: Lude.Text,
    -- | The ID of the Active Directory group for which to revoke access.
    accessGroupId :: Lude.Maybe Lude.Text,
    -- | The ID of the Client VPN endpoint with which the authorization rule is associated.
    clientVPNEndpointId :: Lude.Text,
    -- | Indicates whether access should be revoked for all clients.
    revokeAllGroups :: Lude.Maybe Lude.Bool,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeClientVPNIngress' with the minimum fields required to make a request.
--
-- * 'targetNetworkCidr' - The IPv4 address range, in CIDR notation, of the network for which access is being removed.
-- * 'accessGroupId' - The ID of the Active Directory group for which to revoke access.
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint with which the authorization rule is associated.
-- * 'revokeAllGroups' - Indicates whether access should be revoked for all clients.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkRevokeClientVPNIngress ::
  -- | 'targetNetworkCidr'
  Lude.Text ->
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  RevokeClientVPNIngress
mkRevokeClientVPNIngress pTargetNetworkCidr_ pClientVPNEndpointId_ =
  RevokeClientVPNIngress'
    { targetNetworkCidr = pTargetNetworkCidr_,
      accessGroupId = Lude.Nothing,
      clientVPNEndpointId = pClientVPNEndpointId_,
      revokeAllGroups = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The IPv4 address range, in CIDR notation, of the network for which access is being removed.
--
-- /Note:/ Consider using 'targetNetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcviTargetNetworkCidr :: Lens.Lens' RevokeClientVPNIngress Lude.Text
rcviTargetNetworkCidr = Lens.lens (targetNetworkCidr :: RevokeClientVPNIngress -> Lude.Text) (\s a -> s {targetNetworkCidr = a} :: RevokeClientVPNIngress)
{-# DEPRECATED rcviTargetNetworkCidr "Use generic-lens or generic-optics with 'targetNetworkCidr' instead." #-}

-- | The ID of the Active Directory group for which to revoke access.
--
-- /Note:/ Consider using 'accessGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcviAccessGroupId :: Lens.Lens' RevokeClientVPNIngress (Lude.Maybe Lude.Text)
rcviAccessGroupId = Lens.lens (accessGroupId :: RevokeClientVPNIngress -> Lude.Maybe Lude.Text) (\s a -> s {accessGroupId = a} :: RevokeClientVPNIngress)
{-# DEPRECATED rcviAccessGroupId "Use generic-lens or generic-optics with 'accessGroupId' instead." #-}

-- | The ID of the Client VPN endpoint with which the authorization rule is associated.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcviClientVPNEndpointId :: Lens.Lens' RevokeClientVPNIngress Lude.Text
rcviClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: RevokeClientVPNIngress -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: RevokeClientVPNIngress)
{-# DEPRECATED rcviClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | Indicates whether access should be revoked for all clients.
--
-- /Note:/ Consider using 'revokeAllGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcviRevokeAllGroups :: Lens.Lens' RevokeClientVPNIngress (Lude.Maybe Lude.Bool)
rcviRevokeAllGroups = Lens.lens (revokeAllGroups :: RevokeClientVPNIngress -> Lude.Maybe Lude.Bool) (\s a -> s {revokeAllGroups = a} :: RevokeClientVPNIngress)
{-# DEPRECATED rcviRevokeAllGroups "Use generic-lens or generic-optics with 'revokeAllGroups' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcviDryRun :: Lens.Lens' RevokeClientVPNIngress (Lude.Maybe Lude.Bool)
rcviDryRun = Lens.lens (dryRun :: RevokeClientVPNIngress -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RevokeClientVPNIngress)
{-# DEPRECATED rcviDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest RevokeClientVPNIngress where
  type Rs RevokeClientVPNIngress = RevokeClientVPNIngressResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          RevokeClientVPNIngressResponse'
            Lude.<$> (x Lude..@? "status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RevokeClientVPNIngress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RevokeClientVPNIngress where
  toPath = Lude.const "/"

instance Lude.ToQuery RevokeClientVPNIngress where
  toQuery RevokeClientVPNIngress' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RevokeClientVpnIngress" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TargetNetworkCidr" Lude.=: targetNetworkCidr,
        "AccessGroupId" Lude.=: accessGroupId,
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId,
        "RevokeAllGroups" Lude.=: revokeAllGroups,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkRevokeClientVPNIngressResponse' smart constructor.
data RevokeClientVPNIngressResponse = RevokeClientVPNIngressResponse'
  { -- | The current state of the authorization rule.
    status :: Lude.Maybe ClientVPNAuthorizationRuleStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeClientVPNIngressResponse' with the minimum fields required to make a request.
--
-- * 'status' - The current state of the authorization rule.
-- * 'responseStatus' - The response status code.
mkRevokeClientVPNIngressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RevokeClientVPNIngressResponse
mkRevokeClientVPNIngressResponse pResponseStatus_ =
  RevokeClientVPNIngressResponse'
    { status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current state of the authorization rule.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcvirsStatus :: Lens.Lens' RevokeClientVPNIngressResponse (Lude.Maybe ClientVPNAuthorizationRuleStatus)
rcvirsStatus = Lens.lens (status :: RevokeClientVPNIngressResponse -> Lude.Maybe ClientVPNAuthorizationRuleStatus) (\s a -> s {status = a} :: RevokeClientVPNIngressResponse)
{-# DEPRECATED rcvirsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcvirsResponseStatus :: Lens.Lens' RevokeClientVPNIngressResponse Lude.Int
rcvirsResponseStatus = Lens.lens (responseStatus :: RevokeClientVPNIngressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RevokeClientVPNIngressResponse)
{-# DEPRECATED rcvirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
