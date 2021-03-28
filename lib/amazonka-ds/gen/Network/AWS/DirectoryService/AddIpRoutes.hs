{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.AddIpRoutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If the DNS server for your on-premises domain uses a publicly addressable IP address, you must add a CIDR address block to correctly route traffic to and from your Microsoft AD on Amazon Web Services. /AddIpRoutes/ adds this address block. You can also use /AddIpRoutes/ to facilitate routing traffic that uses public IP ranges from your Microsoft AD on AWS to a peer VPC. 
--
-- Before you call /AddIpRoutes/ , ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the /AddIpRoutes/ operation, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference> .
module Network.AWS.DirectoryService.AddIpRoutes
    (
    -- * Creating a request
      AddIpRoutes (..)
    , mkAddIpRoutes
    -- ** Request lenses
    , airDirectoryId
    , airIpRoutes
    , airUpdateSecurityGroupForDirectoryControllers

    -- * Destructuring the response
    , AddIpRoutesResponse (..)
    , mkAddIpRoutesResponse
    -- ** Response lenses
    , airrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddIpRoutes' smart constructor.
data AddIpRoutes = AddIpRoutes'
  { directoryId :: Types.DirectoryId
    -- ^ Identifier (ID) of the directory to which to add the address block.
  , ipRoutes :: [Types.IpRoute]
    -- ^ IP address blocks, using CIDR format, of the traffic to route. This is often the IP address block of the DNS server used for your on-premises domain.
  , updateSecurityGroupForDirectoryControllers :: Core.Maybe Core.Bool
    -- ^ If set to true, updates the inbound and outbound rules of the security group that has the description: "AWS created security group for /directory ID/ directory controllers." Following are the new rules: 
--
-- Inbound:
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 88, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 123, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 138, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 389, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 464, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 445, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 88, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 135, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 445, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 464, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 636, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 1024-65535, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 3268-33269, Source: 0.0.0.0/0
--
--
--     * Type: DNS (UDP), Protocol: UDP, Range: 53, Source: 0.0.0.0/0
--
--
--     * Type: DNS (TCP), Protocol: TCP, Range: 53, Source: 0.0.0.0/0
--
--
--     * Type: LDAP, Protocol: TCP, Range: 389, Source: 0.0.0.0/0
--
--
--     * Type: All ICMP, Protocol: All, Range: N/A, Source: 0.0.0.0/0
--
--
--
-- Outbound:
--
--     * Type: All traffic, Protocol: All, Range: All, Destination: 0.0.0.0/0
--
--
-- These security rules impact an internal network interface that is not exposed publicly.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddIpRoutes' value with any optional fields omitted.
mkAddIpRoutes
    :: Types.DirectoryId -- ^ 'directoryId'
    -> AddIpRoutes
mkAddIpRoutes directoryId
  = AddIpRoutes'{directoryId, ipRoutes = Core.mempty,
                 updateSecurityGroupForDirectoryControllers = Core.Nothing}

-- | Identifier (ID) of the directory to which to add the address block.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airDirectoryId :: Lens.Lens' AddIpRoutes Types.DirectoryId
airDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE airDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | IP address blocks, using CIDR format, of the traffic to route. This is often the IP address block of the DNS server used for your on-premises domain.
--
-- /Note:/ Consider using 'ipRoutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airIpRoutes :: Lens.Lens' AddIpRoutes [Types.IpRoute]
airIpRoutes = Lens.field @"ipRoutes"
{-# INLINEABLE airIpRoutes #-}
{-# DEPRECATED ipRoutes "Use generic-lens or generic-optics with 'ipRoutes' instead"  #-}

-- | If set to true, updates the inbound and outbound rules of the security group that has the description: "AWS created security group for /directory ID/ directory controllers." Following are the new rules: 
--
-- Inbound:
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 88, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 123, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 138, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 389, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 464, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 445, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 88, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 135, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 445, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 464, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 636, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 1024-65535, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 3268-33269, Source: 0.0.0.0/0
--
--
--     * Type: DNS (UDP), Protocol: UDP, Range: 53, Source: 0.0.0.0/0
--
--
--     * Type: DNS (TCP), Protocol: TCP, Range: 53, Source: 0.0.0.0/0
--
--
--     * Type: LDAP, Protocol: TCP, Range: 389, Source: 0.0.0.0/0
--
--
--     * Type: All ICMP, Protocol: All, Range: N/A, Source: 0.0.0.0/0
--
--
--
-- Outbound:
--
--     * Type: All traffic, Protocol: All, Range: All, Destination: 0.0.0.0/0
--
--
-- These security rules impact an internal network interface that is not exposed publicly.
--
-- /Note:/ Consider using 'updateSecurityGroupForDirectoryControllers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airUpdateSecurityGroupForDirectoryControllers :: Lens.Lens' AddIpRoutes (Core.Maybe Core.Bool)
airUpdateSecurityGroupForDirectoryControllers = Lens.field @"updateSecurityGroupForDirectoryControllers"
{-# INLINEABLE airUpdateSecurityGroupForDirectoryControllers #-}
{-# DEPRECATED updateSecurityGroupForDirectoryControllers "Use generic-lens or generic-optics with 'updateSecurityGroupForDirectoryControllers' instead"  #-}

instance Core.ToQuery AddIpRoutes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddIpRoutes where
        toHeaders AddIpRoutes{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.AddIpRoutes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddIpRoutes where
        toJSON AddIpRoutes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("IpRoutes" Core..= ipRoutes),
                  ("UpdateSecurityGroupForDirectoryControllers" Core..=) Core.<$>
                    updateSecurityGroupForDirectoryControllers])

instance Core.AWSRequest AddIpRoutes where
        type Rs AddIpRoutes = AddIpRoutesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AddIpRoutesResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddIpRoutesResponse' smart constructor.
newtype AddIpRoutesResponse = AddIpRoutesResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddIpRoutesResponse' value with any optional fields omitted.
mkAddIpRoutesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddIpRoutesResponse
mkAddIpRoutesResponse responseStatus
  = AddIpRoutesResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airrrsResponseStatus :: Lens.Lens' AddIpRoutesResponse Core.Int
airrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE airrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
