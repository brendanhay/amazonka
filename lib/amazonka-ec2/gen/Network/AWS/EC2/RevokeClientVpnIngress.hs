{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RevokeClientVpnIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an ingress authorization rule from a Client VPN endpoint. 
module Network.AWS.EC2.RevokeClientVpnIngress
    (
    -- * Creating a request
      RevokeClientVpnIngress (..)
    , mkRevokeClientVpnIngress
    -- ** Request lenses
    , rcviClientVpnEndpointId
    , rcviTargetNetworkCidr
    , rcviAccessGroupId
    , rcviDryRun
    , rcviRevokeAllGroups

    -- * Destructuring the response
    , RevokeClientVpnIngressResponse (..)
    , mkRevokeClientVpnIngressResponse
    -- ** Response lenses
    , rcvirrsStatus
    , rcvirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRevokeClientVpnIngress' smart constructor.
data RevokeClientVpnIngress = RevokeClientVpnIngress'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint with which the authorization rule is associated.
  , targetNetworkCidr :: Core.Text
    -- ^ The IPv4 address range, in CIDR notation, of the network for which access is being removed.
  , accessGroupId :: Core.Maybe Core.Text
    -- ^ The ID of the Active Directory group for which to revoke access. 
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , revokeAllGroups :: Core.Maybe Core.Bool
    -- ^ Indicates whether access should be revoked for all clients.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeClientVpnIngress' value with any optional fields omitted.
mkRevokeClientVpnIngress
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> Core.Text -- ^ 'targetNetworkCidr'
    -> RevokeClientVpnIngress
mkRevokeClientVpnIngress clientVpnEndpointId targetNetworkCidr
  = RevokeClientVpnIngress'{clientVpnEndpointId, targetNetworkCidr,
                            accessGroupId = Core.Nothing, dryRun = Core.Nothing,
                            revokeAllGroups = Core.Nothing}

-- | The ID of the Client VPN endpoint with which the authorization rule is associated.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcviClientVpnEndpointId :: Lens.Lens' RevokeClientVpnIngress Types.ClientVpnEndpointId
rcviClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE rcviClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | The IPv4 address range, in CIDR notation, of the network for which access is being removed.
--
-- /Note:/ Consider using 'targetNetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcviTargetNetworkCidr :: Lens.Lens' RevokeClientVpnIngress Core.Text
rcviTargetNetworkCidr = Lens.field @"targetNetworkCidr"
{-# INLINEABLE rcviTargetNetworkCidr #-}
{-# DEPRECATED targetNetworkCidr "Use generic-lens or generic-optics with 'targetNetworkCidr' instead"  #-}

-- | The ID of the Active Directory group for which to revoke access. 
--
-- /Note:/ Consider using 'accessGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcviAccessGroupId :: Lens.Lens' RevokeClientVpnIngress (Core.Maybe Core.Text)
rcviAccessGroupId = Lens.field @"accessGroupId"
{-# INLINEABLE rcviAccessGroupId #-}
{-# DEPRECATED accessGroupId "Use generic-lens or generic-optics with 'accessGroupId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcviDryRun :: Lens.Lens' RevokeClientVpnIngress (Core.Maybe Core.Bool)
rcviDryRun = Lens.field @"dryRun"
{-# INLINEABLE rcviDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Indicates whether access should be revoked for all clients.
--
-- /Note:/ Consider using 'revokeAllGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcviRevokeAllGroups :: Lens.Lens' RevokeClientVpnIngress (Core.Maybe Core.Bool)
rcviRevokeAllGroups = Lens.field @"revokeAllGroups"
{-# INLINEABLE rcviRevokeAllGroups #-}
{-# DEPRECATED revokeAllGroups "Use generic-lens or generic-optics with 'revokeAllGroups' instead"  #-}

instance Core.ToQuery RevokeClientVpnIngress where
        toQuery RevokeClientVpnIngress{..}
          = Core.toQueryPair "Action" ("RevokeClientVpnIngress" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<> Core.toQueryPair "TargetNetworkCidr" targetNetworkCidr
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AccessGroupId")
                accessGroupId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RevokeAllGroups")
                revokeAllGroups

instance Core.ToHeaders RevokeClientVpnIngress where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RevokeClientVpnIngress where
        type Rs RevokeClientVpnIngress = RevokeClientVpnIngressResponse
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
                 RevokeClientVpnIngressResponse' Core.<$>
                   (x Core..@? "status") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRevokeClientVpnIngressResponse' smart constructor.
data RevokeClientVpnIngressResponse = RevokeClientVpnIngressResponse'
  { status :: Core.Maybe Types.ClientVpnAuthorizationRuleStatus
    -- ^ The current state of the authorization rule.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeClientVpnIngressResponse' value with any optional fields omitted.
mkRevokeClientVpnIngressResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RevokeClientVpnIngressResponse
mkRevokeClientVpnIngressResponse responseStatus
  = RevokeClientVpnIngressResponse'{status = Core.Nothing,
                                    responseStatus}

-- | The current state of the authorization rule.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcvirrsStatus :: Lens.Lens' RevokeClientVpnIngressResponse (Core.Maybe Types.ClientVpnAuthorizationRuleStatus)
rcvirrsStatus = Lens.field @"status"
{-# INLINEABLE rcvirrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcvirrsResponseStatus :: Lens.Lens' RevokeClientVpnIngressResponse Core.Int
rcvirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rcvirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
