{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateCustomAvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom Availability Zone (AZ).
--
-- A custom AZ is an on-premises AZ that is integrated with a VMware vSphere cluster.
-- For more information about RDS on VMware, see the <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html /RDS on VMware User Guide./ > 
module Network.AWS.RDS.CreateCustomAvailabilityZone
    (
    -- * Creating a request
      CreateCustomAvailabilityZone (..)
    , mkCreateCustomAvailabilityZone
    -- ** Request lenses
    , ccazCustomAvailabilityZoneName
    , ccazExistingVpnId
    , ccazNewVpnTunnelName
    , ccazVpnTunnelOriginatorIP

    -- * Destructuring the response
    , CreateCustomAvailabilityZoneResponse (..)
    , mkCreateCustomAvailabilityZoneResponse
    -- ** Response lenses
    , ccazrrsCustomAvailabilityZone
    , ccazrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateCustomAvailabilityZone' smart constructor.
data CreateCustomAvailabilityZone = CreateCustomAvailabilityZone'
  { customAvailabilityZoneName :: Core.Text
    -- ^ The name of the custom Availability Zone (AZ).
  , existingVpnId :: Core.Maybe Core.Text
    -- ^ The ID of an existing virtual private network (VPN) between the Amazon RDS website and the VMware vSphere cluster.
  , newVpnTunnelName :: Core.Maybe Core.Text
    -- ^ The name of a new VPN tunnel between the Amazon RDS website and the VMware vSphere cluster.
--
-- Specify this parameter only if @ExistingVpnId@ isn't specified.
  , vpnTunnelOriginatorIP :: Core.Maybe Core.Text
    -- ^ The IP address of network traffic from your on-premises data center. A custom AZ receives the network traffic.
--
-- Specify this parameter only if @ExistingVpnId@ isn't specified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomAvailabilityZone' value with any optional fields omitted.
mkCreateCustomAvailabilityZone
    :: Core.Text -- ^ 'customAvailabilityZoneName'
    -> CreateCustomAvailabilityZone
mkCreateCustomAvailabilityZone customAvailabilityZoneName
  = CreateCustomAvailabilityZone'{customAvailabilityZoneName,
                                  existingVpnId = Core.Nothing, newVpnTunnelName = Core.Nothing,
                                  vpnTunnelOriginatorIP = Core.Nothing}

-- | The name of the custom Availability Zone (AZ).
--
-- /Note:/ Consider using 'customAvailabilityZoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazCustomAvailabilityZoneName :: Lens.Lens' CreateCustomAvailabilityZone Core.Text
ccazCustomAvailabilityZoneName = Lens.field @"customAvailabilityZoneName"
{-# INLINEABLE ccazCustomAvailabilityZoneName #-}
{-# DEPRECATED customAvailabilityZoneName "Use generic-lens or generic-optics with 'customAvailabilityZoneName' instead"  #-}

-- | The ID of an existing virtual private network (VPN) between the Amazon RDS website and the VMware vSphere cluster.
--
-- /Note:/ Consider using 'existingVpnId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazExistingVpnId :: Lens.Lens' CreateCustomAvailabilityZone (Core.Maybe Core.Text)
ccazExistingVpnId = Lens.field @"existingVpnId"
{-# INLINEABLE ccazExistingVpnId #-}
{-# DEPRECATED existingVpnId "Use generic-lens or generic-optics with 'existingVpnId' instead"  #-}

-- | The name of a new VPN tunnel between the Amazon RDS website and the VMware vSphere cluster.
--
-- Specify this parameter only if @ExistingVpnId@ isn't specified.
--
-- /Note:/ Consider using 'newVpnTunnelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazNewVpnTunnelName :: Lens.Lens' CreateCustomAvailabilityZone (Core.Maybe Core.Text)
ccazNewVpnTunnelName = Lens.field @"newVpnTunnelName"
{-# INLINEABLE ccazNewVpnTunnelName #-}
{-# DEPRECATED newVpnTunnelName "Use generic-lens or generic-optics with 'newVpnTunnelName' instead"  #-}

-- | The IP address of network traffic from your on-premises data center. A custom AZ receives the network traffic.
--
-- Specify this parameter only if @ExistingVpnId@ isn't specified.
--
-- /Note:/ Consider using 'vpnTunnelOriginatorIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazVpnTunnelOriginatorIP :: Lens.Lens' CreateCustomAvailabilityZone (Core.Maybe Core.Text)
ccazVpnTunnelOriginatorIP = Lens.field @"vpnTunnelOriginatorIP"
{-# INLINEABLE ccazVpnTunnelOriginatorIP #-}
{-# DEPRECATED vpnTunnelOriginatorIP "Use generic-lens or generic-optics with 'vpnTunnelOriginatorIP' instead"  #-}

instance Core.ToQuery CreateCustomAvailabilityZone where
        toQuery CreateCustomAvailabilityZone{..}
          = Core.toQueryPair "Action"
              ("CreateCustomAvailabilityZone" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "CustomAvailabilityZoneName"
                customAvailabilityZoneName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ExistingVpnId")
                existingVpnId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NewVpnTunnelName")
                newVpnTunnelName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VpnTunnelOriginatorIP")
                vpnTunnelOriginatorIP

instance Core.ToHeaders CreateCustomAvailabilityZone where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateCustomAvailabilityZone where
        type Rs CreateCustomAvailabilityZone =
             CreateCustomAvailabilityZoneResponse
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
          = Response.receiveXMLWrapper "CreateCustomAvailabilityZoneResult"
              (\ s h x ->
                 CreateCustomAvailabilityZoneResponse' Core.<$>
                   (x Core..@? "CustomAvailabilityZone") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCustomAvailabilityZoneResponse' smart constructor.
data CreateCustomAvailabilityZoneResponse = CreateCustomAvailabilityZoneResponse'
  { customAvailabilityZone :: Core.Maybe Types.CustomAvailabilityZone
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomAvailabilityZoneResponse' value with any optional fields omitted.
mkCreateCustomAvailabilityZoneResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCustomAvailabilityZoneResponse
mkCreateCustomAvailabilityZoneResponse responseStatus
  = CreateCustomAvailabilityZoneResponse'{customAvailabilityZone =
                                            Core.Nothing,
                                          responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'customAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazrrsCustomAvailabilityZone :: Lens.Lens' CreateCustomAvailabilityZoneResponse (Core.Maybe Types.CustomAvailabilityZone)
ccazrrsCustomAvailabilityZone = Lens.field @"customAvailabilityZone"
{-# INLINEABLE ccazrrsCustomAvailabilityZone #-}
{-# DEPRECATED customAvailabilityZone "Use generic-lens or generic-optics with 'customAvailabilityZone' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazrrsResponseStatus :: Lens.Lens' CreateCustomAvailabilityZoneResponse Core.Int
ccazrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccazrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
