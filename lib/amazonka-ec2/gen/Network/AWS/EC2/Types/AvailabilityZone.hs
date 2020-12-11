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
    azState,
    azParentZoneId,
    azRegionName,
    azParentZoneName,
    azNetworkBorderGroup,
    azZoneId,
    azZoneName,
    azOptInStatus,
    azMessages,
    azGroupName,
    azZoneType,
  )
where

import Network.AWS.EC2.Types.AvailabilityZoneMessage
import Network.AWS.EC2.Types.AvailabilityZoneOptInStatus
import Network.AWS.EC2.Types.AvailabilityZoneState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes Availability Zones, Local Zones, and Wavelength Zones.
--
-- /See:/ 'mkAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { state ::
      Lude.Maybe AvailabilityZoneState,
    parentZoneId :: Lude.Maybe Lude.Text,
    regionName :: Lude.Maybe Lude.Text,
    parentZoneName :: Lude.Maybe Lude.Text,
    networkBorderGroup :: Lude.Maybe Lude.Text,
    zoneId :: Lude.Maybe Lude.Text,
    zoneName :: Lude.Maybe Lude.Text,
    optInStatus :: Lude.Maybe AvailabilityZoneOptInStatus,
    messages :: Lude.Maybe [AvailabilityZoneMessage],
    groupName :: Lude.Maybe Lude.Text,
    zoneType :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- * 'groupName' - For Availability Zones, this parameter has the same value as the Region name.
--
-- For Local Zones, the name of the associated group, for example @us-west-2-lax-1@ .
-- For Wavelength Zones, the name of the associated group, for example @us-east-1-wl1-bos-wlz-1@ .
-- * 'messages' - Any messages about the Availability Zone, Local Zone, or Wavelength Zone.
-- * 'networkBorderGroup' - The name of the network border group.
-- * 'optInStatus' - For Availability Zones, this parameter always has the value of @opt-in-not-required@ .
--
-- For Local Zones and Wavelength Zones, this parameter is the opt-in status. The possible values are @opted-in@ , and @not-opted-in@ .
-- * 'parentZoneId' - The ID of the zone that handles some of the Local Zone or Wavelength Zone control plane operations, such as API calls.
-- * 'parentZoneName' - The name of the zone that handles some of the Local Zone or Wavelength Zone control plane operations, such as API calls.
-- * 'regionName' - The name of the Region.
-- * 'state' - The state of the Availability Zone, Local Zone, or Wavelength Zone.
-- * 'zoneId' - The ID of the Availability Zone, Local Zone, or Wavelength Zone.
-- * 'zoneName' - The name of the Availability Zone, Local Zone, or Wavelength Zone.
-- * 'zoneType' - The type of zone. The valid values are @availability-zone@ , @local-zone@ , and @wavelength-zone@ .
mkAvailabilityZone ::
  AvailabilityZone
mkAvailabilityZone =
  AvailabilityZone'
    { state = Lude.Nothing,
      parentZoneId = Lude.Nothing,
      regionName = Lude.Nothing,
      parentZoneName = Lude.Nothing,
      networkBorderGroup = Lude.Nothing,
      zoneId = Lude.Nothing,
      zoneName = Lude.Nothing,
      optInStatus = Lude.Nothing,
      messages = Lude.Nothing,
      groupName = Lude.Nothing,
      zoneType = Lude.Nothing
    }

-- | The state of the Availability Zone, Local Zone, or Wavelength Zone.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azState :: Lens.Lens' AvailabilityZone (Lude.Maybe AvailabilityZoneState)
azState = Lens.lens (state :: AvailabilityZone -> Lude.Maybe AvailabilityZoneState) (\s a -> s {state = a} :: AvailabilityZone)
{-# DEPRECATED azState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the zone that handles some of the Local Zone or Wavelength Zone control plane operations, such as API calls.
--
-- /Note:/ Consider using 'parentZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azParentZoneId :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azParentZoneId = Lens.lens (parentZoneId :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {parentZoneId = a} :: AvailabilityZone)
{-# DEPRECATED azParentZoneId "Use generic-lens or generic-optics with 'parentZoneId' instead." #-}

-- | The name of the Region.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azRegionName :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azRegionName = Lens.lens (regionName :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {regionName = a} :: AvailabilityZone)
{-# DEPRECATED azRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | The name of the zone that handles some of the Local Zone or Wavelength Zone control plane operations, such as API calls.
--
-- /Note:/ Consider using 'parentZoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azParentZoneName :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azParentZoneName = Lens.lens (parentZoneName :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {parentZoneName = a} :: AvailabilityZone)
{-# DEPRECATED azParentZoneName "Use generic-lens or generic-optics with 'parentZoneName' instead." #-}

-- | The name of the network border group.
--
-- /Note:/ Consider using 'networkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azNetworkBorderGroup :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azNetworkBorderGroup = Lens.lens (networkBorderGroup :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {networkBorderGroup = a} :: AvailabilityZone)
{-# DEPRECATED azNetworkBorderGroup "Use generic-lens or generic-optics with 'networkBorderGroup' instead." #-}

-- | The ID of the Availability Zone, Local Zone, or Wavelength Zone.
--
-- /Note:/ Consider using 'zoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azZoneId :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azZoneId = Lens.lens (zoneId :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {zoneId = a} :: AvailabilityZone)
{-# DEPRECATED azZoneId "Use generic-lens or generic-optics with 'zoneId' instead." #-}

-- | The name of the Availability Zone, Local Zone, or Wavelength Zone.
--
-- /Note:/ Consider using 'zoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azZoneName :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azZoneName = Lens.lens (zoneName :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {zoneName = a} :: AvailabilityZone)
{-# DEPRECATED azZoneName "Use generic-lens or generic-optics with 'zoneName' instead." #-}

-- | For Availability Zones, this parameter always has the value of @opt-in-not-required@ .
--
-- For Local Zones and Wavelength Zones, this parameter is the opt-in status. The possible values are @opted-in@ , and @not-opted-in@ .
--
-- /Note:/ Consider using 'optInStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azOptInStatus :: Lens.Lens' AvailabilityZone (Lude.Maybe AvailabilityZoneOptInStatus)
azOptInStatus = Lens.lens (optInStatus :: AvailabilityZone -> Lude.Maybe AvailabilityZoneOptInStatus) (\s a -> s {optInStatus = a} :: AvailabilityZone)
{-# DEPRECATED azOptInStatus "Use generic-lens or generic-optics with 'optInStatus' instead." #-}

-- | Any messages about the Availability Zone, Local Zone, or Wavelength Zone.
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azMessages :: Lens.Lens' AvailabilityZone (Lude.Maybe [AvailabilityZoneMessage])
azMessages = Lens.lens (messages :: AvailabilityZone -> Lude.Maybe [AvailabilityZoneMessage]) (\s a -> s {messages = a} :: AvailabilityZone)
{-# DEPRECATED azMessages "Use generic-lens or generic-optics with 'messages' instead." #-}

-- | For Availability Zones, this parameter has the same value as the Region name.
--
-- For Local Zones, the name of the associated group, for example @us-west-2-lax-1@ .
-- For Wavelength Zones, the name of the associated group, for example @us-east-1-wl1-bos-wlz-1@ .
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azGroupName :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azGroupName = Lens.lens (groupName :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: AvailabilityZone)
{-# DEPRECATED azGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The type of zone. The valid values are @availability-zone@ , @local-zone@ , and @wavelength-zone@ .
--
-- /Note:/ Consider using 'zoneType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azZoneType :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azZoneType = Lens.lens (zoneType :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {zoneType = a} :: AvailabilityZone)
{-# DEPRECATED azZoneType "Use generic-lens or generic-optics with 'zoneType' instead." #-}

instance Lude.FromXML AvailabilityZone where
  parseXML x =
    AvailabilityZone'
      Lude.<$> (x Lude..@? "zoneState")
      Lude.<*> (x Lude..@? "parentZoneId")
      Lude.<*> (x Lude..@? "regionName")
      Lude.<*> (x Lude..@? "parentZoneName")
      Lude.<*> (x Lude..@? "networkBorderGroup")
      Lude.<*> (x Lude..@? "zoneId")
      Lude.<*> (x Lude..@? "zoneName")
      Lude.<*> (x Lude..@? "optInStatus")
      Lude.<*> ( x Lude..@? "messageSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "groupName")
      Lude.<*> (x Lude..@? "zoneType")
