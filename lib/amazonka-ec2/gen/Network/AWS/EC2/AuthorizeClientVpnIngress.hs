{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AuthorizeClientVpnIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an ingress authorization rule to a Client VPN endpoint. Ingress authorization rules act as firewall rules that grant access to networks. You must configure ingress authorization rules to enable clients to access resources in AWS or on-premises networks.
module Network.AWS.EC2.AuthorizeClientVpnIngress
    (
    -- * Creating a request
      AuthorizeClientVpnIngress (..)
    , mkAuthorizeClientVpnIngress
    -- ** Request lenses
    , acviClientVpnEndpointId
    , acviTargetNetworkCidr
    , acviAccessGroupId
    , acviAuthorizeAllGroups
    , acviClientToken
    , acviDescription
    , acviDryRun

    -- * Destructuring the response
    , AuthorizeClientVpnIngressResponse (..)
    , mkAuthorizeClientVpnIngressResponse
    -- ** Response lenses
    , acvirrsStatus
    , acvirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAuthorizeClientVpnIngress' smart constructor.
data AuthorizeClientVpnIngress = AuthorizeClientVpnIngress'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint.
  , targetNetworkCidr :: Core.Text
    -- ^ The IPv4 address range, in CIDR notation, of the network for which access is being authorized.
  , accessGroupId :: Core.Maybe Core.Text
    -- ^ The ID of the group to grant access to, for example, the Active Directory group or identity provider (IdP) group. Required if @AuthorizeAllGroups@ is @false@ or not specified.
  , authorizeAllGroups :: Core.Maybe Core.Bool
    -- ^ Indicates whether to grant access to all clients. Specify @true@ to grant all clients who successfully establish a VPN connection access to the network. Must be set to @true@ if @AccessGroupId@ is not specified.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , description :: Core.Maybe Core.Text
    -- ^ A brief description of the authorization rule.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeClientVpnIngress' value with any optional fields omitted.
mkAuthorizeClientVpnIngress
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> Core.Text -- ^ 'targetNetworkCidr'
    -> AuthorizeClientVpnIngress
mkAuthorizeClientVpnIngress clientVpnEndpointId targetNetworkCidr
  = AuthorizeClientVpnIngress'{clientVpnEndpointId,
                               targetNetworkCidr, accessGroupId = Core.Nothing,
                               authorizeAllGroups = Core.Nothing, clientToken = Core.Nothing,
                               description = Core.Nothing, dryRun = Core.Nothing}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acviClientVpnEndpointId :: Lens.Lens' AuthorizeClientVpnIngress Types.ClientVpnEndpointId
acviClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE acviClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | The IPv4 address range, in CIDR notation, of the network for which access is being authorized.
--
-- /Note:/ Consider using 'targetNetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acviTargetNetworkCidr :: Lens.Lens' AuthorizeClientVpnIngress Core.Text
acviTargetNetworkCidr = Lens.field @"targetNetworkCidr"
{-# INLINEABLE acviTargetNetworkCidr #-}
{-# DEPRECATED targetNetworkCidr "Use generic-lens or generic-optics with 'targetNetworkCidr' instead"  #-}

-- | The ID of the group to grant access to, for example, the Active Directory group or identity provider (IdP) group. Required if @AuthorizeAllGroups@ is @false@ or not specified.
--
-- /Note:/ Consider using 'accessGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acviAccessGroupId :: Lens.Lens' AuthorizeClientVpnIngress (Core.Maybe Core.Text)
acviAccessGroupId = Lens.field @"accessGroupId"
{-# INLINEABLE acviAccessGroupId #-}
{-# DEPRECATED accessGroupId "Use generic-lens or generic-optics with 'accessGroupId' instead"  #-}

-- | Indicates whether to grant access to all clients. Specify @true@ to grant all clients who successfully establish a VPN connection access to the network. Must be set to @true@ if @AccessGroupId@ is not specified.
--
-- /Note:/ Consider using 'authorizeAllGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acviAuthorizeAllGroups :: Lens.Lens' AuthorizeClientVpnIngress (Core.Maybe Core.Bool)
acviAuthorizeAllGroups = Lens.field @"authorizeAllGroups"
{-# INLINEABLE acviAuthorizeAllGroups #-}
{-# DEPRECATED authorizeAllGroups "Use generic-lens or generic-optics with 'authorizeAllGroups' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acviClientToken :: Lens.Lens' AuthorizeClientVpnIngress (Core.Maybe Core.Text)
acviClientToken = Lens.field @"clientToken"
{-# INLINEABLE acviClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | A brief description of the authorization rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acviDescription :: Lens.Lens' AuthorizeClientVpnIngress (Core.Maybe Core.Text)
acviDescription = Lens.field @"description"
{-# INLINEABLE acviDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acviDryRun :: Lens.Lens' AuthorizeClientVpnIngress (Core.Maybe Core.Bool)
acviDryRun = Lens.field @"dryRun"
{-# INLINEABLE acviDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery AuthorizeClientVpnIngress where
        toQuery AuthorizeClientVpnIngress{..}
          = Core.toQueryPair "Action"
              ("AuthorizeClientVpnIngress" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<> Core.toQueryPair "TargetNetworkCidr" targetNetworkCidr
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AccessGroupId")
                accessGroupId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AuthorizeAllGroups")
                authorizeAllGroups
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders AuthorizeClientVpnIngress where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AuthorizeClientVpnIngress where
        type Rs AuthorizeClientVpnIngress =
             AuthorizeClientVpnIngressResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 AuthorizeClientVpnIngressResponse' Core.<$>
                   (x Core..@? "status") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAuthorizeClientVpnIngressResponse' smart constructor.
data AuthorizeClientVpnIngressResponse = AuthorizeClientVpnIngressResponse'
  { status :: Core.Maybe Types.ClientVpnAuthorizationRuleStatus
    -- ^ The current state of the authorization rule.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeClientVpnIngressResponse' value with any optional fields omitted.
mkAuthorizeClientVpnIngressResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AuthorizeClientVpnIngressResponse
mkAuthorizeClientVpnIngressResponse responseStatus
  = AuthorizeClientVpnIngressResponse'{status = Core.Nothing,
                                       responseStatus}

-- | The current state of the authorization rule.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvirrsStatus :: Lens.Lens' AuthorizeClientVpnIngressResponse (Core.Maybe Types.ClientVpnAuthorizationRuleStatus)
acvirrsStatus = Lens.field @"status"
{-# INLINEABLE acvirrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvirrsResponseStatus :: Lens.Lens' AuthorizeClientVpnIngressResponse Core.Int
acvirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE acvirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
