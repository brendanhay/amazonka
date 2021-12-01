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
-- Module      : Amazonka.Lightsail.Types.RegionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.RegionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Lightsail.Types.AvailabilityZone
import Amazonka.Lightsail.Types.RegionName
import qualified Amazonka.Prelude as Prelude

-- | Describes the AWS Region.
--
-- /See:/ 'newRegionInfo' smart constructor.
data RegionInfo = RegionInfo'
  { -- | The Availability Zones. Follows the format @us-east-2a@
    -- (case-sensitive).
    availabilityZones :: Prelude.Maybe [AvailabilityZone],
    -- | The region name (e.g., @us-east-2@).
    name :: Prelude.Maybe RegionName,
    -- | The Availability Zones for databases. Follows the format @us-east-2a@
    -- (case-sensitive).
    relationalDatabaseAvailabilityZones :: Prelude.Maybe [AvailabilityZone],
    -- | The display name (e.g., @Ohio@).
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The continent code (e.g., @NA@, meaning North America).
    continentCode :: Prelude.Maybe Prelude.Text,
    -- | The description of the AWS Region (e.g.,
    -- @This region is recommended to serve users in the eastern United States and eastern Canada@).
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'name', 'regionInfo_name' - The region name (e.g., @us-east-2@).
--
-- 'relationalDatabaseAvailabilityZones', 'regionInfo_relationalDatabaseAvailabilityZones' - The Availability Zones for databases. Follows the format @us-east-2a@
-- (case-sensitive).
--
-- 'displayName', 'regionInfo_displayName' - The display name (e.g., @Ohio@).
--
-- 'continentCode', 'regionInfo_continentCode' - The continent code (e.g., @NA@, meaning North America).
--
-- 'description', 'regionInfo_description' - The description of the AWS Region (e.g.,
-- @This region is recommended to serve users in the eastern United States and eastern Canada@).
newRegionInfo ::
  RegionInfo
newRegionInfo =
  RegionInfo'
    { availabilityZones = Prelude.Nothing,
      name = Prelude.Nothing,
      relationalDatabaseAvailabilityZones =
        Prelude.Nothing,
      displayName = Prelude.Nothing,
      continentCode = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The Availability Zones. Follows the format @us-east-2a@
-- (case-sensitive).
regionInfo_availabilityZones :: Lens.Lens' RegionInfo (Prelude.Maybe [AvailabilityZone])
regionInfo_availabilityZones = Lens.lens (\RegionInfo' {availabilityZones} -> availabilityZones) (\s@RegionInfo' {} a -> s {availabilityZones = a} :: RegionInfo) Prelude.. Lens.mapping Lens.coerced

-- | The region name (e.g., @us-east-2@).
regionInfo_name :: Lens.Lens' RegionInfo (Prelude.Maybe RegionName)
regionInfo_name = Lens.lens (\RegionInfo' {name} -> name) (\s@RegionInfo' {} a -> s {name = a} :: RegionInfo)

-- | The Availability Zones for databases. Follows the format @us-east-2a@
-- (case-sensitive).
regionInfo_relationalDatabaseAvailabilityZones :: Lens.Lens' RegionInfo (Prelude.Maybe [AvailabilityZone])
regionInfo_relationalDatabaseAvailabilityZones = Lens.lens (\RegionInfo' {relationalDatabaseAvailabilityZones} -> relationalDatabaseAvailabilityZones) (\s@RegionInfo' {} a -> s {relationalDatabaseAvailabilityZones = a} :: RegionInfo) Prelude.. Lens.mapping Lens.coerced

-- | The display name (e.g., @Ohio@).
regionInfo_displayName :: Lens.Lens' RegionInfo (Prelude.Maybe Prelude.Text)
regionInfo_displayName = Lens.lens (\RegionInfo' {displayName} -> displayName) (\s@RegionInfo' {} a -> s {displayName = a} :: RegionInfo)

-- | The continent code (e.g., @NA@, meaning North America).
regionInfo_continentCode :: Lens.Lens' RegionInfo (Prelude.Maybe Prelude.Text)
regionInfo_continentCode = Lens.lens (\RegionInfo' {continentCode} -> continentCode) (\s@RegionInfo' {} a -> s {continentCode = a} :: RegionInfo)

-- | The description of the AWS Region (e.g.,
-- @This region is recommended to serve users in the eastern United States and eastern Canada@).
regionInfo_description :: Lens.Lens' RegionInfo (Prelude.Maybe Prelude.Text)
regionInfo_description = Lens.lens (\RegionInfo' {description} -> description) (\s@RegionInfo' {} a -> s {description = a} :: RegionInfo)

instance Core.FromJSON RegionInfo where
  parseJSON =
    Core.withObject
      "RegionInfo"
      ( \x ->
          RegionInfo'
            Prelude.<$> ( x Core..:? "availabilityZones"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> ( x Core..:? "relationalDatabaseAvailabilityZones"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "displayName")
            Prelude.<*> (x Core..:? "continentCode")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable RegionInfo where
  hashWithSalt salt' RegionInfo' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` continentCode
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` relationalDatabaseAvailabilityZones
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` availabilityZones

instance Prelude.NFData RegionInfo where
  rnf RegionInfo' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf continentCode
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf relationalDatabaseAvailabilityZones
      `Prelude.seq` Prelude.rnf name
