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
-- Module      : Network.AWS.Lightsail.Types.RegionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RegionInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AvailabilityZone
import Network.AWS.Lightsail.Types.RegionName

-- | Describes the AWS Region.
--
-- /See:/ 'newRegionInfo' smart constructor.
data RegionInfo = RegionInfo'
  { -- | The Availability Zones. Follows the format @us-east-2a@
    -- (case-sensitive).
    availabilityZones :: Core.Maybe [AvailabilityZone],
    -- | The continent code (e.g., @NA@, meaning North America).
    continentCode :: Core.Maybe Core.Text,
    -- | The Availability Zones for databases. Follows the format @us-east-2a@
    -- (case-sensitive).
    relationalDatabaseAvailabilityZones :: Core.Maybe [AvailabilityZone],
    -- | The region name (e.g., @us-east-2@).
    name :: Core.Maybe RegionName,
    -- | The description of the AWS Region (e.g.,
    -- @This region is recommended to serve users in the eastern United States and eastern Canada@).
    description :: Core.Maybe Core.Text,
    -- | The display name (e.g., @Ohio@).
    displayName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'regionInfo_availabilityZones' - The Availability Zones. Follows the format @us-east-2a@
-- (case-sensitive).
--
-- 'continentCode', 'regionInfo_continentCode' - The continent code (e.g., @NA@, meaning North America).
--
-- 'relationalDatabaseAvailabilityZones', 'regionInfo_relationalDatabaseAvailabilityZones' - The Availability Zones for databases. Follows the format @us-east-2a@
-- (case-sensitive).
--
-- 'name', 'regionInfo_name' - The region name (e.g., @us-east-2@).
--
-- 'description', 'regionInfo_description' - The description of the AWS Region (e.g.,
-- @This region is recommended to serve users in the eastern United States and eastern Canada@).
--
-- 'displayName', 'regionInfo_displayName' - The display name (e.g., @Ohio@).
newRegionInfo ::
  RegionInfo
newRegionInfo =
  RegionInfo'
    { availabilityZones = Core.Nothing,
      continentCode = Core.Nothing,
      relationalDatabaseAvailabilityZones = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      displayName = Core.Nothing
    }

-- | The Availability Zones. Follows the format @us-east-2a@
-- (case-sensitive).
regionInfo_availabilityZones :: Lens.Lens' RegionInfo (Core.Maybe [AvailabilityZone])
regionInfo_availabilityZones = Lens.lens (\RegionInfo' {availabilityZones} -> availabilityZones) (\s@RegionInfo' {} a -> s {availabilityZones = a} :: RegionInfo) Core.. Lens.mapping Lens._Coerce

-- | The continent code (e.g., @NA@, meaning North America).
regionInfo_continentCode :: Lens.Lens' RegionInfo (Core.Maybe Core.Text)
regionInfo_continentCode = Lens.lens (\RegionInfo' {continentCode} -> continentCode) (\s@RegionInfo' {} a -> s {continentCode = a} :: RegionInfo)

-- | The Availability Zones for databases. Follows the format @us-east-2a@
-- (case-sensitive).
regionInfo_relationalDatabaseAvailabilityZones :: Lens.Lens' RegionInfo (Core.Maybe [AvailabilityZone])
regionInfo_relationalDatabaseAvailabilityZones = Lens.lens (\RegionInfo' {relationalDatabaseAvailabilityZones} -> relationalDatabaseAvailabilityZones) (\s@RegionInfo' {} a -> s {relationalDatabaseAvailabilityZones = a} :: RegionInfo) Core.. Lens.mapping Lens._Coerce

-- | The region name (e.g., @us-east-2@).
regionInfo_name :: Lens.Lens' RegionInfo (Core.Maybe RegionName)
regionInfo_name = Lens.lens (\RegionInfo' {name} -> name) (\s@RegionInfo' {} a -> s {name = a} :: RegionInfo)

-- | The description of the AWS Region (e.g.,
-- @This region is recommended to serve users in the eastern United States and eastern Canada@).
regionInfo_description :: Lens.Lens' RegionInfo (Core.Maybe Core.Text)
regionInfo_description = Lens.lens (\RegionInfo' {description} -> description) (\s@RegionInfo' {} a -> s {description = a} :: RegionInfo)

-- | The display name (e.g., @Ohio@).
regionInfo_displayName :: Lens.Lens' RegionInfo (Core.Maybe Core.Text)
regionInfo_displayName = Lens.lens (\RegionInfo' {displayName} -> displayName) (\s@RegionInfo' {} a -> s {displayName = a} :: RegionInfo)

instance Core.FromJSON RegionInfo where
  parseJSON =
    Core.withObject
      "RegionInfo"
      ( \x ->
          RegionInfo'
            Core.<$> (x Core..:? "availabilityZones" Core..!= Core.mempty)
            Core.<*> (x Core..:? "continentCode")
            Core.<*> ( x Core..:? "relationalDatabaseAvailabilityZones"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "displayName")
      )

instance Core.Hashable RegionInfo

instance Core.NFData RegionInfo
