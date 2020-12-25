{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AvailabilityZone
  ( AvailabilityZone (..),

    -- * Smart constructor
    mkAvailabilityZone,

    -- * Lenses
    azGroupName,
    azMessages,
    azNetworkBorderGroup,
    azOptInStatus,
    azParentZoneId,
    azParentZoneName,
    azRegionName,
    azState,
    azZoneId,
    azZoneName,
    azZoneType,
  )
where

import qualified Network.AWS.EC2.Types.AvailabilityZoneMessage as Types
import qualified Network.AWS.EC2.Types.AvailabilityZoneOptInStatus as Types
import qualified Network.AWS.EC2.Types.AvailabilityZoneState as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes Availability Zones, Local Zones, and Wavelength Zones.
--
-- /See:/ 'mkAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { -- | For Availability Zones, this parameter has the same value as the Region name.
    --
    -- For Local Zones, the name of the associated group, for example @us-west-2-lax-1@ .
    -- For Wavelength Zones, the name of the associated group, for example @us-east-1-wl1-bos-wlz-1@ .
    groupName :: Core.Maybe Types.String,
    -- | Any messages about the Availability Zone, Local Zone, or Wavelength Zone.
    messages :: Core.Maybe [Types.AvailabilityZoneMessage],
    -- | The name of the network border group.
    networkBorderGroup :: Core.Maybe Types.String,
    -- | For Availability Zones, this parameter always has the value of @opt-in-not-required@ .
    --
    -- For Local Zones and Wavelength Zones, this parameter is the opt-in status. The possible values are @opted-in@ , and @not-opted-in@ .
    optInStatus :: Core.Maybe Types.AvailabilityZoneOptInStatus,
    -- | The ID of the zone that handles some of the Local Zone or Wavelength Zone control plane operations, such as API calls.
    parentZoneId :: Core.Maybe Types.String,
    -- | The name of the zone that handles some of the Local Zone or Wavelength Zone control plane operations, such as API calls.
    parentZoneName :: Core.Maybe Types.String,
    -- | The name of the Region.
    regionName :: Core.Maybe Types.String,
    -- | The state of the Availability Zone, Local Zone, or Wavelength Zone.
    state :: Core.Maybe Types.AvailabilityZoneState,
    -- | The ID of the Availability Zone, Local Zone, or Wavelength Zone.
    zoneId :: Core.Maybe Types.String,
    -- | The name of the Availability Zone, Local Zone, or Wavelength Zone.
    zoneName :: Core.Maybe Types.String,
    -- | The type of zone. The valid values are @availability-zone@ , @local-zone@ , and @wavelength-zone@ .
    zoneType :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AvailabilityZone' value with any optional fields omitted.
mkAvailabilityZone ::
  AvailabilityZone
mkAvailabilityZone =
  AvailabilityZone'
    { groupName = Core.Nothing,
      messages = Core.Nothing,
      networkBorderGroup = Core.Nothing,
      optInStatus = Core.Nothing,
      parentZoneId = Core.Nothing,
      parentZoneName = Core.Nothing,
      regionName = Core.Nothing,
      state = Core.Nothing,
      zoneId = Core.Nothing,
      zoneName = Core.Nothing,
      zoneType = Core.Nothing
    }

-- | For Availability Zones, this parameter has the same value as the Region name.
--
-- For Local Zones, the name of the associated group, for example @us-west-2-lax-1@ .
-- For Wavelength Zones, the name of the associated group, for example @us-east-1-wl1-bos-wlz-1@ .
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azGroupName :: Lens.Lens' AvailabilityZone (Core.Maybe Types.String)
azGroupName = Lens.field @"groupName"
{-# DEPRECATED azGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | Any messages about the Availability Zone, Local Zone, or Wavelength Zone.
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azMessages :: Lens.Lens' AvailabilityZone (Core.Maybe [Types.AvailabilityZoneMessage])
azMessages = Lens.field @"messages"
{-# DEPRECATED azMessages "Use generic-lens or generic-optics with 'messages' instead." #-}

-- | The name of the network border group.
--
-- /Note:/ Consider using 'networkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azNetworkBorderGroup :: Lens.Lens' AvailabilityZone (Core.Maybe Types.String)
azNetworkBorderGroup = Lens.field @"networkBorderGroup"
{-# DEPRECATED azNetworkBorderGroup "Use generic-lens or generic-optics with 'networkBorderGroup' instead." #-}

-- | For Availability Zones, this parameter always has the value of @opt-in-not-required@ .
--
-- For Local Zones and Wavelength Zones, this parameter is the opt-in status. The possible values are @opted-in@ , and @not-opted-in@ .
--
-- /Note:/ Consider using 'optInStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azOptInStatus :: Lens.Lens' AvailabilityZone (Core.Maybe Types.AvailabilityZoneOptInStatus)
azOptInStatus = Lens.field @"optInStatus"
{-# DEPRECATED azOptInStatus "Use generic-lens or generic-optics with 'optInStatus' instead." #-}

-- | The ID of the zone that handles some of the Local Zone or Wavelength Zone control plane operations, such as API calls.
--
-- /Note:/ Consider using 'parentZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azParentZoneId :: Lens.Lens' AvailabilityZone (Core.Maybe Types.String)
azParentZoneId = Lens.field @"parentZoneId"
{-# DEPRECATED azParentZoneId "Use generic-lens or generic-optics with 'parentZoneId' instead." #-}

-- | The name of the zone that handles some of the Local Zone or Wavelength Zone control plane operations, such as API calls.
--
-- /Note:/ Consider using 'parentZoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azParentZoneName :: Lens.Lens' AvailabilityZone (Core.Maybe Types.String)
azParentZoneName = Lens.field @"parentZoneName"
{-# DEPRECATED azParentZoneName "Use generic-lens or generic-optics with 'parentZoneName' instead." #-}

-- | The name of the Region.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azRegionName :: Lens.Lens' AvailabilityZone (Core.Maybe Types.String)
azRegionName = Lens.field @"regionName"
{-# DEPRECATED azRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | The state of the Availability Zone, Local Zone, or Wavelength Zone.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azState :: Lens.Lens' AvailabilityZone (Core.Maybe Types.AvailabilityZoneState)
azState = Lens.field @"state"
{-# DEPRECATED azState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the Availability Zone, Local Zone, or Wavelength Zone.
--
-- /Note:/ Consider using 'zoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azZoneId :: Lens.Lens' AvailabilityZone (Core.Maybe Types.String)
azZoneId = Lens.field @"zoneId"
{-# DEPRECATED azZoneId "Use generic-lens or generic-optics with 'zoneId' instead." #-}

-- | The name of the Availability Zone, Local Zone, or Wavelength Zone.
--
-- /Note:/ Consider using 'zoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azZoneName :: Lens.Lens' AvailabilityZone (Core.Maybe Types.String)
azZoneName = Lens.field @"zoneName"
{-# DEPRECATED azZoneName "Use generic-lens or generic-optics with 'zoneName' instead." #-}

-- | The type of zone. The valid values are @availability-zone@ , @local-zone@ , and @wavelength-zone@ .
--
-- /Note:/ Consider using 'zoneType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azZoneType :: Lens.Lens' AvailabilityZone (Core.Maybe Types.String)
azZoneType = Lens.field @"zoneType"
{-# DEPRECATED azZoneType "Use generic-lens or generic-optics with 'zoneType' instead." #-}

instance Core.FromXML AvailabilityZone where
  parseXML x =
    AvailabilityZone'
      Core.<$> (x Core..@? "groupName")
      Core.<*> (x Core..@? "messageSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "networkBorderGroup")
      Core.<*> (x Core..@? "optInStatus")
      Core.<*> (x Core..@? "parentZoneId")
      Core.<*> (x Core..@? "parentZoneName")
      Core.<*> (x Core..@? "regionName")
      Core.<*> (x Core..@? "zoneState")
      Core.<*> (x Core..@? "zoneId")
      Core.<*> (x Core..@? "zoneName")
      Core.<*> (x Core..@? "zoneType")
