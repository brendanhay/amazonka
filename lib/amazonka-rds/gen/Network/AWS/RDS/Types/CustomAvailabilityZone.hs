{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.CustomAvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.CustomAvailabilityZone
  ( CustomAvailabilityZone (..),

    -- * Smart constructor
    mkCustomAvailabilityZone,

    -- * Lenses
    cazCustomAvailabilityZoneId,
    cazCustomAvailabilityZoneName,
    cazCustomAvailabilityZoneStatus,
    cazVpnDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types
import qualified Network.AWS.RDS.Types.VpnDetails as Types

-- | A custom Availability Zone (AZ) is an on-premises AZ that is integrated with a VMware vSphere cluster.
--
-- For more information about RDS on VMware, see the <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html /RDS on VMware User Guide./ >
--
-- /See:/ 'mkCustomAvailabilityZone' smart constructor.
data CustomAvailabilityZone = CustomAvailabilityZone'
  { -- | The identifier of the custom AZ.
    --
    -- Amazon RDS generates a unique identifier when a custom AZ is created.
    customAvailabilityZoneId :: Core.Maybe Types.String,
    -- | The name of the custom AZ.
    customAvailabilityZoneName :: Core.Maybe Types.String,
    -- | The status of the custom AZ.
    customAvailabilityZoneStatus :: Core.Maybe Types.String,
    -- | Information about the virtual private network (VPN) between the VMware vSphere cluster and the AWS website.
    vpnDetails :: Core.Maybe Types.VpnDetails
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomAvailabilityZone' value with any optional fields omitted.
mkCustomAvailabilityZone ::
  CustomAvailabilityZone
mkCustomAvailabilityZone =
  CustomAvailabilityZone'
    { customAvailabilityZoneId = Core.Nothing,
      customAvailabilityZoneName = Core.Nothing,
      customAvailabilityZoneStatus = Core.Nothing,
      vpnDetails = Core.Nothing
    }

-- | The identifier of the custom AZ.
--
-- Amazon RDS generates a unique identifier when a custom AZ is created.
--
-- /Note:/ Consider using 'customAvailabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cazCustomAvailabilityZoneId :: Lens.Lens' CustomAvailabilityZone (Core.Maybe Types.String)
cazCustomAvailabilityZoneId = Lens.field @"customAvailabilityZoneId"
{-# DEPRECATED cazCustomAvailabilityZoneId "Use generic-lens or generic-optics with 'customAvailabilityZoneId' instead." #-}

-- | The name of the custom AZ.
--
-- /Note:/ Consider using 'customAvailabilityZoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cazCustomAvailabilityZoneName :: Lens.Lens' CustomAvailabilityZone (Core.Maybe Types.String)
cazCustomAvailabilityZoneName = Lens.field @"customAvailabilityZoneName"
{-# DEPRECATED cazCustomAvailabilityZoneName "Use generic-lens or generic-optics with 'customAvailabilityZoneName' instead." #-}

-- | The status of the custom AZ.
--
-- /Note:/ Consider using 'customAvailabilityZoneStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cazCustomAvailabilityZoneStatus :: Lens.Lens' CustomAvailabilityZone (Core.Maybe Types.String)
cazCustomAvailabilityZoneStatus = Lens.field @"customAvailabilityZoneStatus"
{-# DEPRECATED cazCustomAvailabilityZoneStatus "Use generic-lens or generic-optics with 'customAvailabilityZoneStatus' instead." #-}

-- | Information about the virtual private network (VPN) between the VMware vSphere cluster and the AWS website.
--
-- /Note:/ Consider using 'vpnDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cazVpnDetails :: Lens.Lens' CustomAvailabilityZone (Core.Maybe Types.VpnDetails)
cazVpnDetails = Lens.field @"vpnDetails"
{-# DEPRECATED cazVpnDetails "Use generic-lens or generic-optics with 'vpnDetails' instead." #-}

instance Core.FromXML CustomAvailabilityZone where
  parseXML x =
    CustomAvailabilityZone'
      Core.<$> (x Core..@? "CustomAvailabilityZoneId")
      Core.<*> (x Core..@? "CustomAvailabilityZoneName")
      Core.<*> (x Core..@? "CustomAvailabilityZoneStatus")
      Core.<*> (x Core..@? "VpnDetails")
