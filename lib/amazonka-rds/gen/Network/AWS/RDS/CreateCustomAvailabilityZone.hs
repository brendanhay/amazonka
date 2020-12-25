{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateCustomAvailabilityZone (..),
    mkCreateCustomAvailabilityZone,

    -- ** Request lenses
    ccazCustomAvailabilityZoneName,
    ccazExistingVpnId,
    ccazNewVpnTunnelName,
    ccazVpnTunnelOriginatorIP,

    -- * Destructuring the response
    CreateCustomAvailabilityZoneResponse (..),
    mkCreateCustomAvailabilityZoneResponse,

    -- ** Response lenses
    ccazrrsCustomAvailabilityZone,
    ccazrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateCustomAvailabilityZone' smart constructor.
data CreateCustomAvailabilityZone = CreateCustomAvailabilityZone'
  { -- | The name of the custom Availability Zone (AZ).
    customAvailabilityZoneName :: Types.String,
    -- | The ID of an existing virtual private network (VPN) between the Amazon RDS website and the VMware vSphere cluster.
    existingVpnId :: Core.Maybe Types.String,
    -- | The name of a new VPN tunnel between the Amazon RDS website and the VMware vSphere cluster.
    --
    -- Specify this parameter only if @ExistingVpnId@ isn't specified.
    newVpnTunnelName :: Core.Maybe Types.String,
    -- | The IP address of network traffic from your on-premises data center. A custom AZ receives the network traffic.
    --
    -- Specify this parameter only if @ExistingVpnId@ isn't specified.
    vpnTunnelOriginatorIP :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomAvailabilityZone' value with any optional fields omitted.
mkCreateCustomAvailabilityZone ::
  -- | 'customAvailabilityZoneName'
  Types.String ->
  CreateCustomAvailabilityZone
mkCreateCustomAvailabilityZone customAvailabilityZoneName =
  CreateCustomAvailabilityZone'
    { customAvailabilityZoneName,
      existingVpnId = Core.Nothing,
      newVpnTunnelName = Core.Nothing,
      vpnTunnelOriginatorIP = Core.Nothing
    }

-- | The name of the custom Availability Zone (AZ).
--
-- /Note:/ Consider using 'customAvailabilityZoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazCustomAvailabilityZoneName :: Lens.Lens' CreateCustomAvailabilityZone Types.String
ccazCustomAvailabilityZoneName = Lens.field @"customAvailabilityZoneName"
{-# DEPRECATED ccazCustomAvailabilityZoneName "Use generic-lens or generic-optics with 'customAvailabilityZoneName' instead." #-}

-- | The ID of an existing virtual private network (VPN) between the Amazon RDS website and the VMware vSphere cluster.
--
-- /Note:/ Consider using 'existingVpnId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazExistingVpnId :: Lens.Lens' CreateCustomAvailabilityZone (Core.Maybe Types.String)
ccazExistingVpnId = Lens.field @"existingVpnId"
{-# DEPRECATED ccazExistingVpnId "Use generic-lens or generic-optics with 'existingVpnId' instead." #-}

-- | The name of a new VPN tunnel between the Amazon RDS website and the VMware vSphere cluster.
--
-- Specify this parameter only if @ExistingVpnId@ isn't specified.
--
-- /Note:/ Consider using 'newVpnTunnelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazNewVpnTunnelName :: Lens.Lens' CreateCustomAvailabilityZone (Core.Maybe Types.String)
ccazNewVpnTunnelName = Lens.field @"newVpnTunnelName"
{-# DEPRECATED ccazNewVpnTunnelName "Use generic-lens or generic-optics with 'newVpnTunnelName' instead." #-}

-- | The IP address of network traffic from your on-premises data center. A custom AZ receives the network traffic.
--
-- Specify this parameter only if @ExistingVpnId@ isn't specified.
--
-- /Note:/ Consider using 'vpnTunnelOriginatorIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazVpnTunnelOriginatorIP :: Lens.Lens' CreateCustomAvailabilityZone (Core.Maybe Types.String)
ccazVpnTunnelOriginatorIP = Lens.field @"vpnTunnelOriginatorIP"
{-# DEPRECATED ccazVpnTunnelOriginatorIP "Use generic-lens or generic-optics with 'vpnTunnelOriginatorIP' instead." #-}

instance Core.AWSRequest CreateCustomAvailabilityZone where
  type
    Rs CreateCustomAvailabilityZone =
      CreateCustomAvailabilityZoneResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateCustomAvailabilityZone")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "CustomAvailabilityZoneName"
                            customAvailabilityZoneName
                        )
                Core.<> (Core.toQueryValue "ExistingVpnId" Core.<$> existingVpnId)
                Core.<> (Core.toQueryValue "NewVpnTunnelName" Core.<$> newVpnTunnelName)
                Core.<> ( Core.toQueryValue "VpnTunnelOriginatorIP"
                            Core.<$> vpnTunnelOriginatorIP
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateCustomAvailabilityZoneResult"
      ( \s h x ->
          CreateCustomAvailabilityZoneResponse'
            Core.<$> (x Core..@? "CustomAvailabilityZone")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateCustomAvailabilityZoneResponse' smart constructor.
data CreateCustomAvailabilityZoneResponse = CreateCustomAvailabilityZoneResponse'
  { customAvailabilityZone :: Core.Maybe Types.CustomAvailabilityZone,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomAvailabilityZoneResponse' value with any optional fields omitted.
mkCreateCustomAvailabilityZoneResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateCustomAvailabilityZoneResponse
mkCreateCustomAvailabilityZoneResponse responseStatus =
  CreateCustomAvailabilityZoneResponse'
    { customAvailabilityZone =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'customAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazrrsCustomAvailabilityZone :: Lens.Lens' CreateCustomAvailabilityZoneResponse (Core.Maybe Types.CustomAvailabilityZone)
ccazrrsCustomAvailabilityZone = Lens.field @"customAvailabilityZone"
{-# DEPRECATED ccazrrsCustomAvailabilityZone "Use generic-lens or generic-optics with 'customAvailabilityZone' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccazrrsResponseStatus :: Lens.Lens' CreateCustomAvailabilityZoneResponse Core.Int
ccazrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccazrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
