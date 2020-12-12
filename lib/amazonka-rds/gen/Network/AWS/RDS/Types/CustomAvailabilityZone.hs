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
    cazVPNDetails,
    cazCustomAvailabilityZoneName,
    cazCustomAvailabilityZoneId,
    cazCustomAvailabilityZoneStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.VPNDetails

-- | A custom Availability Zone (AZ) is an on-premises AZ that is integrated with a VMware vSphere cluster.
--
-- For more information about RDS on VMware, see the <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html /RDS on VMware User Guide./ >
--
-- /See:/ 'mkCustomAvailabilityZone' smart constructor.
data CustomAvailabilityZone = CustomAvailabilityZone'
  { vpnDetails ::
      Lude.Maybe VPNDetails,
    customAvailabilityZoneName ::
      Lude.Maybe Lude.Text,
    customAvailabilityZoneId ::
      Lude.Maybe Lude.Text,
    customAvailabilityZoneStatus ::
      Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomAvailabilityZone' with the minimum fields required to make a request.
--
-- * 'customAvailabilityZoneId' - The identifier of the custom AZ.
--
-- Amazon RDS generates a unique identifier when a custom AZ is created.
-- * 'customAvailabilityZoneName' - The name of the custom AZ.
-- * 'customAvailabilityZoneStatus' - The status of the custom AZ.
-- * 'vpnDetails' - Information about the virtual private network (VPN) between the VMware vSphere cluster and the AWS website.
mkCustomAvailabilityZone ::
  CustomAvailabilityZone
mkCustomAvailabilityZone =
  CustomAvailabilityZone'
    { vpnDetails = Lude.Nothing,
      customAvailabilityZoneName = Lude.Nothing,
      customAvailabilityZoneId = Lude.Nothing,
      customAvailabilityZoneStatus = Lude.Nothing
    }

-- | Information about the virtual private network (VPN) between the VMware vSphere cluster and the AWS website.
--
-- /Note:/ Consider using 'vpnDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cazVPNDetails :: Lens.Lens' CustomAvailabilityZone (Lude.Maybe VPNDetails)
cazVPNDetails = Lens.lens (vpnDetails :: CustomAvailabilityZone -> Lude.Maybe VPNDetails) (\s a -> s {vpnDetails = a} :: CustomAvailabilityZone)
{-# DEPRECATED cazVPNDetails "Use generic-lens or generic-optics with 'vpnDetails' instead." #-}

-- | The name of the custom AZ.
--
-- /Note:/ Consider using 'customAvailabilityZoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cazCustomAvailabilityZoneName :: Lens.Lens' CustomAvailabilityZone (Lude.Maybe Lude.Text)
cazCustomAvailabilityZoneName = Lens.lens (customAvailabilityZoneName :: CustomAvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {customAvailabilityZoneName = a} :: CustomAvailabilityZone)
{-# DEPRECATED cazCustomAvailabilityZoneName "Use generic-lens or generic-optics with 'customAvailabilityZoneName' instead." #-}

-- | The identifier of the custom AZ.
--
-- Amazon RDS generates a unique identifier when a custom AZ is created.
--
-- /Note:/ Consider using 'customAvailabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cazCustomAvailabilityZoneId :: Lens.Lens' CustomAvailabilityZone (Lude.Maybe Lude.Text)
cazCustomAvailabilityZoneId = Lens.lens (customAvailabilityZoneId :: CustomAvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {customAvailabilityZoneId = a} :: CustomAvailabilityZone)
{-# DEPRECATED cazCustomAvailabilityZoneId "Use generic-lens or generic-optics with 'customAvailabilityZoneId' instead." #-}

-- | The status of the custom AZ.
--
-- /Note:/ Consider using 'customAvailabilityZoneStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cazCustomAvailabilityZoneStatus :: Lens.Lens' CustomAvailabilityZone (Lude.Maybe Lude.Text)
cazCustomAvailabilityZoneStatus = Lens.lens (customAvailabilityZoneStatus :: CustomAvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {customAvailabilityZoneStatus = a} :: CustomAvailabilityZone)
{-# DEPRECATED cazCustomAvailabilityZoneStatus "Use generic-lens or generic-optics with 'customAvailabilityZoneStatus' instead." #-}

instance Lude.FromXML CustomAvailabilityZone where
  parseXML x =
    CustomAvailabilityZone'
      Lude.<$> (x Lude..@? "VpnDetails")
      Lude.<*> (x Lude..@? "CustomAvailabilityZoneName")
      Lude.<*> (x Lude..@? "CustomAvailabilityZoneId")
      Lude.<*> (x Lude..@? "CustomAvailabilityZoneStatus")
