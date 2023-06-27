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
-- Module      : Amazonka.OpenSearch.Types.AvailabilityZoneInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AvailabilityZoneInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.ZoneStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about an Availability Zone on a domain.
--
-- /See:/ 'newAvailabilityZoneInfo' smart constructor.
data AvailabilityZoneInfo = AvailabilityZoneInfo'
  { -- | The name of the Availability Zone.
    availabilityZoneName :: Prelude.Maybe Prelude.Text,
    -- | The number of data nodes active in the Availability Zone.
    availableDataNodeCount :: Prelude.Maybe Prelude.Text,
    -- | The total number of data nodes configured in the Availability Zone.
    configuredDataNodeCount :: Prelude.Maybe Prelude.Text,
    -- | The total number of primary and replica shards in the Availability Zone.
    totalShards :: Prelude.Maybe Prelude.Text,
    -- | The total number of primary and replica shards that aren\'t allocated to
    -- any of the nodes in the Availability Zone.
    totalUnAssignedShards :: Prelude.Maybe Prelude.Text,
    -- | The current state of the Availability Zone. Current options are @Active@
    -- and @StandBy@.
    --
    -- -   @Active@ - Data nodes in the Availability Zone are in use.
    --
    -- -   @StandBy@ - Data nodes in the Availability Zone are in a standby
    --     state.
    --
    -- -   @NotAvailable@ - Unable to retrieve information.
    zoneStatus :: Prelude.Maybe ZoneStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvailabilityZoneInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneName', 'availabilityZoneInfo_availabilityZoneName' - The name of the Availability Zone.
--
-- 'availableDataNodeCount', 'availabilityZoneInfo_availableDataNodeCount' - The number of data nodes active in the Availability Zone.
--
-- 'configuredDataNodeCount', 'availabilityZoneInfo_configuredDataNodeCount' - The total number of data nodes configured in the Availability Zone.
--
-- 'totalShards', 'availabilityZoneInfo_totalShards' - The total number of primary and replica shards in the Availability Zone.
--
-- 'totalUnAssignedShards', 'availabilityZoneInfo_totalUnAssignedShards' - The total number of primary and replica shards that aren\'t allocated to
-- any of the nodes in the Availability Zone.
--
-- 'zoneStatus', 'availabilityZoneInfo_zoneStatus' - The current state of the Availability Zone. Current options are @Active@
-- and @StandBy@.
--
-- -   @Active@ - Data nodes in the Availability Zone are in use.
--
-- -   @StandBy@ - Data nodes in the Availability Zone are in a standby
--     state.
--
-- -   @NotAvailable@ - Unable to retrieve information.
newAvailabilityZoneInfo ::
  AvailabilityZoneInfo
newAvailabilityZoneInfo =
  AvailabilityZoneInfo'
    { availabilityZoneName =
        Prelude.Nothing,
      availableDataNodeCount = Prelude.Nothing,
      configuredDataNodeCount = Prelude.Nothing,
      totalShards = Prelude.Nothing,
      totalUnAssignedShards = Prelude.Nothing,
      zoneStatus = Prelude.Nothing
    }

-- | The name of the Availability Zone.
availabilityZoneInfo_availabilityZoneName :: Lens.Lens' AvailabilityZoneInfo (Prelude.Maybe Prelude.Text)
availabilityZoneInfo_availabilityZoneName = Lens.lens (\AvailabilityZoneInfo' {availabilityZoneName} -> availabilityZoneName) (\s@AvailabilityZoneInfo' {} a -> s {availabilityZoneName = a} :: AvailabilityZoneInfo)

-- | The number of data nodes active in the Availability Zone.
availabilityZoneInfo_availableDataNodeCount :: Lens.Lens' AvailabilityZoneInfo (Prelude.Maybe Prelude.Text)
availabilityZoneInfo_availableDataNodeCount = Lens.lens (\AvailabilityZoneInfo' {availableDataNodeCount} -> availableDataNodeCount) (\s@AvailabilityZoneInfo' {} a -> s {availableDataNodeCount = a} :: AvailabilityZoneInfo)

-- | The total number of data nodes configured in the Availability Zone.
availabilityZoneInfo_configuredDataNodeCount :: Lens.Lens' AvailabilityZoneInfo (Prelude.Maybe Prelude.Text)
availabilityZoneInfo_configuredDataNodeCount = Lens.lens (\AvailabilityZoneInfo' {configuredDataNodeCount} -> configuredDataNodeCount) (\s@AvailabilityZoneInfo' {} a -> s {configuredDataNodeCount = a} :: AvailabilityZoneInfo)

-- | The total number of primary and replica shards in the Availability Zone.
availabilityZoneInfo_totalShards :: Lens.Lens' AvailabilityZoneInfo (Prelude.Maybe Prelude.Text)
availabilityZoneInfo_totalShards = Lens.lens (\AvailabilityZoneInfo' {totalShards} -> totalShards) (\s@AvailabilityZoneInfo' {} a -> s {totalShards = a} :: AvailabilityZoneInfo)

-- | The total number of primary and replica shards that aren\'t allocated to
-- any of the nodes in the Availability Zone.
availabilityZoneInfo_totalUnAssignedShards :: Lens.Lens' AvailabilityZoneInfo (Prelude.Maybe Prelude.Text)
availabilityZoneInfo_totalUnAssignedShards = Lens.lens (\AvailabilityZoneInfo' {totalUnAssignedShards} -> totalUnAssignedShards) (\s@AvailabilityZoneInfo' {} a -> s {totalUnAssignedShards = a} :: AvailabilityZoneInfo)

-- | The current state of the Availability Zone. Current options are @Active@
-- and @StandBy@.
--
-- -   @Active@ - Data nodes in the Availability Zone are in use.
--
-- -   @StandBy@ - Data nodes in the Availability Zone are in a standby
--     state.
--
-- -   @NotAvailable@ - Unable to retrieve information.
availabilityZoneInfo_zoneStatus :: Lens.Lens' AvailabilityZoneInfo (Prelude.Maybe ZoneStatus)
availabilityZoneInfo_zoneStatus = Lens.lens (\AvailabilityZoneInfo' {zoneStatus} -> zoneStatus) (\s@AvailabilityZoneInfo' {} a -> s {zoneStatus = a} :: AvailabilityZoneInfo)

instance Data.FromJSON AvailabilityZoneInfo where
  parseJSON =
    Data.withObject
      "AvailabilityZoneInfo"
      ( \x ->
          AvailabilityZoneInfo'
            Prelude.<$> (x Data..:? "AvailabilityZoneName")
            Prelude.<*> (x Data..:? "AvailableDataNodeCount")
            Prelude.<*> (x Data..:? "ConfiguredDataNodeCount")
            Prelude.<*> (x Data..:? "TotalShards")
            Prelude.<*> (x Data..:? "TotalUnAssignedShards")
            Prelude.<*> (x Data..:? "ZoneStatus")
      )

instance Prelude.Hashable AvailabilityZoneInfo where
  hashWithSalt _salt AvailabilityZoneInfo' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZoneName
      `Prelude.hashWithSalt` availableDataNodeCount
      `Prelude.hashWithSalt` configuredDataNodeCount
      `Prelude.hashWithSalt` totalShards
      `Prelude.hashWithSalt` totalUnAssignedShards
      `Prelude.hashWithSalt` zoneStatus

instance Prelude.NFData AvailabilityZoneInfo where
  rnf AvailabilityZoneInfo' {..} =
    Prelude.rnf availabilityZoneName
      `Prelude.seq` Prelude.rnf availableDataNodeCount
      `Prelude.seq` Prelude.rnf configuredDataNodeCount
      `Prelude.seq` Prelude.rnf totalShards
      `Prelude.seq` Prelude.rnf totalUnAssignedShards
      `Prelude.seq` Prelude.rnf zoneStatus
