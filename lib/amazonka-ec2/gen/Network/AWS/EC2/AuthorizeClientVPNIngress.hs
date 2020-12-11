{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AuthorizeClientVPNIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an ingress authorization rule to a Client VPN endpoint. Ingress authorization rules act as firewall rules that grant access to networks. You must configure ingress authorization rules to enable clients to access resources in AWS or on-premises networks.
module Network.AWS.EC2.AuthorizeClientVPNIngress
  ( -- * Creating a request
    AuthorizeClientVPNIngress (..),
    mkAuthorizeClientVPNIngress,

    -- ** Request lenses
    acviClientToken,
    acviAccessGroupId,
    acviAuthorizeAllGroups,
    acviDescription,
    acviDryRun,
    acviClientVPNEndpointId,
    acviTargetNetworkCidr,

    -- * Destructuring the response
    AuthorizeClientVPNIngressResponse (..),
    mkAuthorizeClientVPNIngressResponse,

    -- ** Response lenses
    acvirsStatus,
    acvirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAuthorizeClientVPNIngress' smart constructor.
data AuthorizeClientVPNIngress = AuthorizeClientVPNIngress'
  { clientToken ::
      Lude.Maybe Lude.Text,
    accessGroupId :: Lude.Maybe Lude.Text,
    authorizeAllGroups ::
      Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    clientVPNEndpointId :: Lude.Text,
    targetNetworkCidr :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizeClientVPNIngress' with the minimum fields required to make a request.
--
-- * 'accessGroupId' - The ID of the group to grant access to, for example, the Active Directory group or identity provider (IdP) group. Required if @AuthorizeAllGroups@ is @false@ or not specified.
-- * 'authorizeAllGroups' - Indicates whether to grant access to all clients. Specify @true@ to grant all clients who successfully establish a VPN connection access to the network. Must be set to @true@ if @AccessGroupId@ is not specified.
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint.
-- * 'description' - A brief description of the authorization rule.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'targetNetworkCidr' - The IPv4 address range, in CIDR notation, of the network for which access is being authorized.
mkAuthorizeClientVPNIngress ::
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  -- | 'targetNetworkCidr'
  Lude.Text ->
  AuthorizeClientVPNIngress
mkAuthorizeClientVPNIngress
  pClientVPNEndpointId_
  pTargetNetworkCidr_ =
    AuthorizeClientVPNIngress'
      { clientToken = Lude.Nothing,
        accessGroupId = Lude.Nothing,
        authorizeAllGroups = Lude.Nothing,
        description = Lude.Nothing,
        dryRun = Lude.Nothing,
        clientVPNEndpointId = pClientVPNEndpointId_,
        targetNetworkCidr = pTargetNetworkCidr_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acviClientToken :: Lens.Lens' AuthorizeClientVPNIngress (Lude.Maybe Lude.Text)
acviClientToken = Lens.lens (clientToken :: AuthorizeClientVPNIngress -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: AuthorizeClientVPNIngress)
{-# DEPRECATED acviClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ID of the group to grant access to, for example, the Active Directory group or identity provider (IdP) group. Required if @AuthorizeAllGroups@ is @false@ or not specified.
--
-- /Note:/ Consider using 'accessGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acviAccessGroupId :: Lens.Lens' AuthorizeClientVPNIngress (Lude.Maybe Lude.Text)
acviAccessGroupId = Lens.lens (accessGroupId :: AuthorizeClientVPNIngress -> Lude.Maybe Lude.Text) (\s a -> s {accessGroupId = a} :: AuthorizeClientVPNIngress)
{-# DEPRECATED acviAccessGroupId "Use generic-lens or generic-optics with 'accessGroupId' instead." #-}

-- | Indicates whether to grant access to all clients. Specify @true@ to grant all clients who successfully establish a VPN connection access to the network. Must be set to @true@ if @AccessGroupId@ is not specified.
--
-- /Note:/ Consider using 'authorizeAllGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acviAuthorizeAllGroups :: Lens.Lens' AuthorizeClientVPNIngress (Lude.Maybe Lude.Bool)
acviAuthorizeAllGroups = Lens.lens (authorizeAllGroups :: AuthorizeClientVPNIngress -> Lude.Maybe Lude.Bool) (\s a -> s {authorizeAllGroups = a} :: AuthorizeClientVPNIngress)
{-# DEPRECATED acviAuthorizeAllGroups "Use generic-lens or generic-optics with 'authorizeAllGroups' instead." #-}

-- | A brief description of the authorization rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acviDescription :: Lens.Lens' AuthorizeClientVPNIngress (Lude.Maybe Lude.Text)
acviDescription = Lens.lens (description :: AuthorizeClientVPNIngress -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: AuthorizeClientVPNIngress)
{-# DEPRECATED acviDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acviDryRun :: Lens.Lens' AuthorizeClientVPNIngress (Lude.Maybe Lude.Bool)
acviDryRun = Lens.lens (dryRun :: AuthorizeClientVPNIngress -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AuthorizeClientVPNIngress)
{-# DEPRECATED acviDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acviClientVPNEndpointId :: Lens.Lens' AuthorizeClientVPNIngress Lude.Text
acviClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: AuthorizeClientVPNIngress -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: AuthorizeClientVPNIngress)
{-# DEPRECATED acviClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | The IPv4 address range, in CIDR notation, of the network for which access is being authorized.
--
-- /Note:/ Consider using 'targetNetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acviTargetNetworkCidr :: Lens.Lens' AuthorizeClientVPNIngress Lude.Text
acviTargetNetworkCidr = Lens.lens (targetNetworkCidr :: AuthorizeClientVPNIngress -> Lude.Text) (\s a -> s {targetNetworkCidr = a} :: AuthorizeClientVPNIngress)
{-# DEPRECATED acviTargetNetworkCidr "Use generic-lens or generic-optics with 'targetNetworkCidr' instead." #-}

instance Lude.AWSRequest AuthorizeClientVPNIngress where
  type
    Rs AuthorizeClientVPNIngress =
      AuthorizeClientVPNIngressResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AuthorizeClientVPNIngressResponse'
            Lude.<$> (x Lude..@? "status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AuthorizeClientVPNIngress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AuthorizeClientVPNIngress where
  toPath = Lude.const "/"

instance Lude.ToQuery AuthorizeClientVPNIngress where
  toQuery AuthorizeClientVPNIngress' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AuthorizeClientVpnIngress" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        "AccessGroupId" Lude.=: accessGroupId,
        "AuthorizeAllGroups" Lude.=: authorizeAllGroups,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId,
        "TargetNetworkCidr" Lude.=: targetNetworkCidr
      ]

-- | /See:/ 'mkAuthorizeClientVPNIngressResponse' smart constructor.
data AuthorizeClientVPNIngressResponse = AuthorizeClientVPNIngressResponse'
  { status ::
      Lude.Maybe
        ClientVPNAuthorizationRuleStatus,
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

-- | Creates a value of 'AuthorizeClientVPNIngressResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'status' - The current state of the authorization rule.
mkAuthorizeClientVPNIngressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AuthorizeClientVPNIngressResponse
mkAuthorizeClientVPNIngressResponse pResponseStatus_ =
  AuthorizeClientVPNIngressResponse'
    { status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current state of the authorization rule.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvirsStatus :: Lens.Lens' AuthorizeClientVPNIngressResponse (Lude.Maybe ClientVPNAuthorizationRuleStatus)
acvirsStatus = Lens.lens (status :: AuthorizeClientVPNIngressResponse -> Lude.Maybe ClientVPNAuthorizationRuleStatus) (\s a -> s {status = a} :: AuthorizeClientVPNIngressResponse)
{-# DEPRECATED acvirsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvirsResponseStatus :: Lens.Lens' AuthorizeClientVPNIngressResponse Lude.Int
acvirsResponseStatus = Lens.lens (responseStatus :: AuthorizeClientVPNIngressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AuthorizeClientVPNIngressResponse)
{-# DEPRECATED acvirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
