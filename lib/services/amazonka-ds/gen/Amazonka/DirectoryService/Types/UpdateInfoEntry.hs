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
-- Module      : Amazonka.DirectoryService.Types.UpdateInfoEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.UpdateInfoEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.UpdateStatus
import Amazonka.DirectoryService.Types.UpdateValue
import qualified Amazonka.Prelude as Prelude

-- | An entry of update information related to a requested update type.
--
-- /See:/ 'newUpdateInfoEntry' smart constructor.
data UpdateInfoEntry = UpdateInfoEntry'
  { -- | This specifies if the update was initiated by the customer or by the
    -- service team.
    initiatedBy :: Prelude.Maybe Prelude.Text,
    -- | The old value of the target setting.
    previousValue :: Prelude.Maybe UpdateValue,
    -- | The reason for the current status of the update type activity.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The status of the update performed on the directory.
    status :: Prelude.Maybe UpdateStatus,
    -- | The name of the Region.
    region :: Prelude.Maybe Prelude.Text,
    -- | The new value of the target setting.
    newValue' :: Prelude.Maybe UpdateValue,
    -- | The start time of the @UpdateDirectorySetup@ for the particular type.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The last updated date and time of a particular directory setting.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInfoEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initiatedBy', 'updateInfoEntry_initiatedBy' - This specifies if the update was initiated by the customer or by the
-- service team.
--
-- 'previousValue', 'updateInfoEntry_previousValue' - The old value of the target setting.
--
-- 'statusReason', 'updateInfoEntry_statusReason' - The reason for the current status of the update type activity.
--
-- 'status', 'updateInfoEntry_status' - The status of the update performed on the directory.
--
-- 'region', 'updateInfoEntry_region' - The name of the Region.
--
-- 'newValue'', 'updateInfoEntry_newValue' - The new value of the target setting.
--
-- 'startTime', 'updateInfoEntry_startTime' - The start time of the @UpdateDirectorySetup@ for the particular type.
--
-- 'lastUpdatedDateTime', 'updateInfoEntry_lastUpdatedDateTime' - The last updated date and time of a particular directory setting.
newUpdateInfoEntry ::
  UpdateInfoEntry
newUpdateInfoEntry =
  UpdateInfoEntry'
    { initiatedBy = Prelude.Nothing,
      previousValue = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      status = Prelude.Nothing,
      region = Prelude.Nothing,
      newValue' = Prelude.Nothing,
      startTime = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | This specifies if the update was initiated by the customer or by the
-- service team.
updateInfoEntry_initiatedBy :: Lens.Lens' UpdateInfoEntry (Prelude.Maybe Prelude.Text)
updateInfoEntry_initiatedBy = Lens.lens (\UpdateInfoEntry' {initiatedBy} -> initiatedBy) (\s@UpdateInfoEntry' {} a -> s {initiatedBy = a} :: UpdateInfoEntry)

-- | The old value of the target setting.
updateInfoEntry_previousValue :: Lens.Lens' UpdateInfoEntry (Prelude.Maybe UpdateValue)
updateInfoEntry_previousValue = Lens.lens (\UpdateInfoEntry' {previousValue} -> previousValue) (\s@UpdateInfoEntry' {} a -> s {previousValue = a} :: UpdateInfoEntry)

-- | The reason for the current status of the update type activity.
updateInfoEntry_statusReason :: Lens.Lens' UpdateInfoEntry (Prelude.Maybe Prelude.Text)
updateInfoEntry_statusReason = Lens.lens (\UpdateInfoEntry' {statusReason} -> statusReason) (\s@UpdateInfoEntry' {} a -> s {statusReason = a} :: UpdateInfoEntry)

-- | The status of the update performed on the directory.
updateInfoEntry_status :: Lens.Lens' UpdateInfoEntry (Prelude.Maybe UpdateStatus)
updateInfoEntry_status = Lens.lens (\UpdateInfoEntry' {status} -> status) (\s@UpdateInfoEntry' {} a -> s {status = a} :: UpdateInfoEntry)

-- | The name of the Region.
updateInfoEntry_region :: Lens.Lens' UpdateInfoEntry (Prelude.Maybe Prelude.Text)
updateInfoEntry_region = Lens.lens (\UpdateInfoEntry' {region} -> region) (\s@UpdateInfoEntry' {} a -> s {region = a} :: UpdateInfoEntry)

-- | The new value of the target setting.
updateInfoEntry_newValue :: Lens.Lens' UpdateInfoEntry (Prelude.Maybe UpdateValue)
updateInfoEntry_newValue = Lens.lens (\UpdateInfoEntry' {newValue'} -> newValue') (\s@UpdateInfoEntry' {} a -> s {newValue' = a} :: UpdateInfoEntry)

-- | The start time of the @UpdateDirectorySetup@ for the particular type.
updateInfoEntry_startTime :: Lens.Lens' UpdateInfoEntry (Prelude.Maybe Prelude.UTCTime)
updateInfoEntry_startTime = Lens.lens (\UpdateInfoEntry' {startTime} -> startTime) (\s@UpdateInfoEntry' {} a -> s {startTime = a} :: UpdateInfoEntry) Prelude.. Lens.mapping Data._Time

-- | The last updated date and time of a particular directory setting.
updateInfoEntry_lastUpdatedDateTime :: Lens.Lens' UpdateInfoEntry (Prelude.Maybe Prelude.UTCTime)
updateInfoEntry_lastUpdatedDateTime = Lens.lens (\UpdateInfoEntry' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@UpdateInfoEntry' {} a -> s {lastUpdatedDateTime = a} :: UpdateInfoEntry) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON UpdateInfoEntry where
  parseJSON =
    Data.withObject
      "UpdateInfoEntry"
      ( \x ->
          UpdateInfoEntry'
            Prelude.<$> (x Data..:? "InitiatedBy")
            Prelude.<*> (x Data..:? "PreviousValue")
            Prelude.<*> (x Data..:? "StatusReason")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "NewValue")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "LastUpdatedDateTime")
      )

instance Prelude.Hashable UpdateInfoEntry where
  hashWithSalt _salt UpdateInfoEntry' {..} =
    _salt `Prelude.hashWithSalt` initiatedBy
      `Prelude.hashWithSalt` previousValue
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` newValue'
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData UpdateInfoEntry where
  rnf UpdateInfoEntry' {..} =
    Prelude.rnf initiatedBy
      `Prelude.seq` Prelude.rnf previousValue
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf newValue'
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
