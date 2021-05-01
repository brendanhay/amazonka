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
-- Module      : Network.AWS.Lightsail.Types.RegionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RegionInfo where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AvailabilityZone
import Network.AWS.Lightsail.Types.RegionName
import qualified Network.AWS.Prelude as Prelude

-- | Describes the AWS Region.
--
-- /See:/ 'newRegionInfo' smart constructor.
data RegionInfo = RegionInfo'
  { -- | The Availability Zones. Follows the format @us-east-2a@
    -- (case-sensitive).
    availabilityZones :: Prelude.Maybe [AvailabilityZone],
    -- | The continent code (e.g., @NA@, meaning North America).
    continentCode :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zones for databases. Follows the format @us-east-2a@
    -- (case-sensitive).
    relationalDatabaseAvailabilityZones :: Prelude.Maybe [AvailabilityZone],
    -- | The region name (e.g., @us-east-2@).
    name :: Prelude.Maybe RegionName,
    -- | The description of the AWS Region (e.g.,
    -- @This region is recommended to serve users in the eastern United States and eastern Canada@).
    description :: Prelude.Maybe Prelude.Text,
    -- | The display name (e.g., @Ohio@).
    displayName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { availabilityZones = Prelude.Nothing,
      continentCode = Prelude.Nothing,
      relationalDatabaseAvailabilityZones =
        Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing
    }

-- | The Availability Zones. Follows the format @us-east-2a@
-- (case-sensitive).
regionInfo_availabilityZones :: Lens.Lens' RegionInfo (Prelude.Maybe [AvailabilityZone])
regionInfo_availabilityZones = Lens.lens (\RegionInfo' {availabilityZones} -> availabilityZones) (\s@RegionInfo' {} a -> s {availabilityZones = a} :: RegionInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The continent code (e.g., @NA@, meaning North America).
regionInfo_continentCode :: Lens.Lens' RegionInfo (Prelude.Maybe Prelude.Text)
regionInfo_continentCode = Lens.lens (\RegionInfo' {continentCode} -> continentCode) (\s@RegionInfo' {} a -> s {continentCode = a} :: RegionInfo)

-- | The Availability Zones for databases. Follows the format @us-east-2a@
-- (case-sensitive).
regionInfo_relationalDatabaseAvailabilityZones :: Lens.Lens' RegionInfo (Prelude.Maybe [AvailabilityZone])
regionInfo_relationalDatabaseAvailabilityZones = Lens.lens (\RegionInfo' {relationalDatabaseAvailabilityZones} -> relationalDatabaseAvailabilityZones) (\s@RegionInfo' {} a -> s {relationalDatabaseAvailabilityZones = a} :: RegionInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The region name (e.g., @us-east-2@).
regionInfo_name :: Lens.Lens' RegionInfo (Prelude.Maybe RegionName)
regionInfo_name = Lens.lens (\RegionInfo' {name} -> name) (\s@RegionInfo' {} a -> s {name = a} :: RegionInfo)

-- | The description of the AWS Region (e.g.,
-- @This region is recommended to serve users in the eastern United States and eastern Canada@).
regionInfo_description :: Lens.Lens' RegionInfo (Prelude.Maybe Prelude.Text)
regionInfo_description = Lens.lens (\RegionInfo' {description} -> description) (\s@RegionInfo' {} a -> s {description = a} :: RegionInfo)

-- | The display name (e.g., @Ohio@).
regionInfo_displayName :: Lens.Lens' RegionInfo (Prelude.Maybe Prelude.Text)
regionInfo_displayName = Lens.lens (\RegionInfo' {displayName} -> displayName) (\s@RegionInfo' {} a -> s {displayName = a} :: RegionInfo)

instance Prelude.FromJSON RegionInfo where
  parseJSON =
    Prelude.withObject
      "RegionInfo"
      ( \x ->
          RegionInfo'
            Prelude.<$> ( x Prelude..:? "availabilityZones"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "continentCode")
            Prelude.<*> ( x Prelude..:? "relationalDatabaseAvailabilityZones"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "displayName")
      )

instance Prelude.Hashable RegionInfo

instance Prelude.NFData RegionInfo
