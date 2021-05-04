{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AvailabilityZone
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AvailabilityZone where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AvailabilityZoneMessage
import Network.AWS.EC2.Types.AvailabilityZoneOptInStatus
import Network.AWS.EC2.Types.AvailabilityZoneState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes Availability Zones, Local Zones, and Wavelength Zones.
--
-- /See:/ 'newAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { -- | The name of the Region.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the zone that handles some of the Local Zone or Wavelength
    -- Zone control plane operations, such as API calls.
    parentZoneId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Availability Zone, Local Zone, or Wavelength Zone.
    zoneName :: Prelude.Maybe Prelude.Text,
    -- | The type of zone. The valid values are @availability-zone@,
    -- @local-zone@, and @wavelength-zone@.
    zoneType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Availability Zone, Local Zone, or Wavelength Zone.
    zoneId :: Prelude.Maybe Prelude.Text,
    -- | For Availability Zones, this parameter has the same value as the Region
    -- name.
    --
    -- For Local Zones, the name of the associated group, for example
    -- @us-west-2-lax-1@.
    --
    -- For Wavelength Zones, the name of the associated group, for example
    -- @us-east-1-wl1-bos-wlz-1@.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | For Availability Zones, this parameter always has the value of
    -- @opt-in-not-required@.
    --
    -- For Local Zones and Wavelength Zones, this parameter is the opt-in
    -- status. The possible values are @opted-in@, and @not-opted-in@.
    optInStatus :: Prelude.Maybe AvailabilityZoneOptInStatus,
    -- | The state of the Availability Zone, Local Zone, or Wavelength Zone.
    state :: Prelude.Maybe AvailabilityZoneState,
    -- | Any messages about the Availability Zone, Local Zone, or Wavelength
    -- Zone.
    messages :: Prelude.Maybe [AvailabilityZoneMessage],
    -- | The name of the network border group.
    networkBorderGroup :: Prelude.Maybe Prelude.Text,
    -- | The name of the zone that handles some of the Local Zone or Wavelength
    -- Zone control plane operations, such as API calls.
    parentZoneName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AvailabilityZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'availabilityZone_regionName' - The name of the Region.
--
-- 'parentZoneId', 'availabilityZone_parentZoneId' - The ID of the zone that handles some of the Local Zone or Wavelength
-- Zone control plane operations, such as API calls.
--
-- 'zoneName', 'availabilityZone_zoneName' - The name of the Availability Zone, Local Zone, or Wavelength Zone.
--
-- 'zoneType', 'availabilityZone_zoneType' - The type of zone. The valid values are @availability-zone@,
-- @local-zone@, and @wavelength-zone@.
--
-- 'zoneId', 'availabilityZone_zoneId' - The ID of the Availability Zone, Local Zone, or Wavelength Zone.
--
-- 'groupName', 'availabilityZone_groupName' - For Availability Zones, this parameter has the same value as the Region
-- name.
--
-- For Local Zones, the name of the associated group, for example
-- @us-west-2-lax-1@.
--
-- For Wavelength Zones, the name of the associated group, for example
-- @us-east-1-wl1-bos-wlz-1@.
--
-- 'optInStatus', 'availabilityZone_optInStatus' - For Availability Zones, this parameter always has the value of
-- @opt-in-not-required@.
--
-- For Local Zones and Wavelength Zones, this parameter is the opt-in
-- status. The possible values are @opted-in@, and @not-opted-in@.
--
-- 'state', 'availabilityZone_state' - The state of the Availability Zone, Local Zone, or Wavelength Zone.
--
-- 'messages', 'availabilityZone_messages' - Any messages about the Availability Zone, Local Zone, or Wavelength
-- Zone.
--
-- 'networkBorderGroup', 'availabilityZone_networkBorderGroup' - The name of the network border group.
--
-- 'parentZoneName', 'availabilityZone_parentZoneName' - The name of the zone that handles some of the Local Zone or Wavelength
-- Zone control plane operations, such as API calls.
newAvailabilityZone ::
  AvailabilityZone
newAvailabilityZone =
  AvailabilityZone'
    { regionName = Prelude.Nothing,
      parentZoneId = Prelude.Nothing,
      zoneName = Prelude.Nothing,
      zoneType = Prelude.Nothing,
      zoneId = Prelude.Nothing,
      groupName = Prelude.Nothing,
      optInStatus = Prelude.Nothing,
      state = Prelude.Nothing,
      messages = Prelude.Nothing,
      networkBorderGroup = Prelude.Nothing,
      parentZoneName = Prelude.Nothing
    }

-- | The name of the Region.
availabilityZone_regionName :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_regionName = Lens.lens (\AvailabilityZone' {regionName} -> regionName) (\s@AvailabilityZone' {} a -> s {regionName = a} :: AvailabilityZone)

-- | The ID of the zone that handles some of the Local Zone or Wavelength
-- Zone control plane operations, such as API calls.
availabilityZone_parentZoneId :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_parentZoneId = Lens.lens (\AvailabilityZone' {parentZoneId} -> parentZoneId) (\s@AvailabilityZone' {} a -> s {parentZoneId = a} :: AvailabilityZone)

-- | The name of the Availability Zone, Local Zone, or Wavelength Zone.
availabilityZone_zoneName :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_zoneName = Lens.lens (\AvailabilityZone' {zoneName} -> zoneName) (\s@AvailabilityZone' {} a -> s {zoneName = a} :: AvailabilityZone)

-- | The type of zone. The valid values are @availability-zone@,
-- @local-zone@, and @wavelength-zone@.
availabilityZone_zoneType :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_zoneType = Lens.lens (\AvailabilityZone' {zoneType} -> zoneType) (\s@AvailabilityZone' {} a -> s {zoneType = a} :: AvailabilityZone)

-- | The ID of the Availability Zone, Local Zone, or Wavelength Zone.
availabilityZone_zoneId :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_zoneId = Lens.lens (\AvailabilityZone' {zoneId} -> zoneId) (\s@AvailabilityZone' {} a -> s {zoneId = a} :: AvailabilityZone)

-- | For Availability Zones, this parameter has the same value as the Region
-- name.
--
-- For Local Zones, the name of the associated group, for example
-- @us-west-2-lax-1@.
--
-- For Wavelength Zones, the name of the associated group, for example
-- @us-east-1-wl1-bos-wlz-1@.
availabilityZone_groupName :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_groupName = Lens.lens (\AvailabilityZone' {groupName} -> groupName) (\s@AvailabilityZone' {} a -> s {groupName = a} :: AvailabilityZone)

-- | For Availability Zones, this parameter always has the value of
-- @opt-in-not-required@.
--
-- For Local Zones and Wavelength Zones, this parameter is the opt-in
-- status. The possible values are @opted-in@, and @not-opted-in@.
availabilityZone_optInStatus :: Lens.Lens' AvailabilityZone (Prelude.Maybe AvailabilityZoneOptInStatus)
availabilityZone_optInStatus = Lens.lens (\AvailabilityZone' {optInStatus} -> optInStatus) (\s@AvailabilityZone' {} a -> s {optInStatus = a} :: AvailabilityZone)

-- | The state of the Availability Zone, Local Zone, or Wavelength Zone.
availabilityZone_state :: Lens.Lens' AvailabilityZone (Prelude.Maybe AvailabilityZoneState)
availabilityZone_state = Lens.lens (\AvailabilityZone' {state} -> state) (\s@AvailabilityZone' {} a -> s {state = a} :: AvailabilityZone)

-- | Any messages about the Availability Zone, Local Zone, or Wavelength
-- Zone.
availabilityZone_messages :: Lens.Lens' AvailabilityZone (Prelude.Maybe [AvailabilityZoneMessage])
availabilityZone_messages = Lens.lens (\AvailabilityZone' {messages} -> messages) (\s@AvailabilityZone' {} a -> s {messages = a} :: AvailabilityZone) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the network border group.
availabilityZone_networkBorderGroup :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_networkBorderGroup = Lens.lens (\AvailabilityZone' {networkBorderGroup} -> networkBorderGroup) (\s@AvailabilityZone' {} a -> s {networkBorderGroup = a} :: AvailabilityZone)

-- | The name of the zone that handles some of the Local Zone or Wavelength
-- Zone control plane operations, such as API calls.
availabilityZone_parentZoneName :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_parentZoneName = Lens.lens (\AvailabilityZone' {parentZoneName} -> parentZoneName) (\s@AvailabilityZone' {} a -> s {parentZoneName = a} :: AvailabilityZone)

instance Prelude.FromXML AvailabilityZone where
  parseXML x =
    AvailabilityZone'
      Prelude.<$> (x Prelude..@? "regionName")
      Prelude.<*> (x Prelude..@? "parentZoneId")
      Prelude.<*> (x Prelude..@? "zoneName")
      Prelude.<*> (x Prelude..@? "zoneType")
      Prelude.<*> (x Prelude..@? "zoneId")
      Prelude.<*> (x Prelude..@? "groupName")
      Prelude.<*> (x Prelude..@? "optInStatus")
      Prelude.<*> (x Prelude..@? "zoneState")
      Prelude.<*> ( x Prelude..@? "messageSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "networkBorderGroup")
      Prelude.<*> (x Prelude..@? "parentZoneName")

instance Prelude.Hashable AvailabilityZone

instance Prelude.NFData AvailabilityZone
