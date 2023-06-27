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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportJobRefreshScheduleOverrideParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportJobRefreshScheduleOverrideParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of overrides for a specific @RefreshsSchedule@ resource that is
-- present in the asset bundle that is imported.
--
-- /See:/ 'newAssetBundleImportJobRefreshScheduleOverrideParameters' smart constructor.
data AssetBundleImportJobRefreshScheduleOverrideParameters = AssetBundleImportJobRefreshScheduleOverrideParameters'
  { -- | An override for the @StartAfterDateTime@ of a @RefreshSchedule@. Make
    -- sure that the @StartAfterDateTime@ is set to a time that takes place in
    -- the future.
    startAfterDateTime :: Prelude.Maybe Data.POSIX,
    -- | A partial identifier for the specific @RefreshSchedule@ resource that is
    -- being overridden. This structure is used together with the @ScheduleID@
    -- structure.
    dataSetId :: Prelude.Text,
    -- | A partial identifier for the specific @RefreshSchedule@ resource being
    -- overridden. This structure is used together with the @DataSetId@
    -- structure.
    scheduleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleImportJobRefreshScheduleOverrideParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startAfterDateTime', 'assetBundleImportJobRefreshScheduleOverrideParameters_startAfterDateTime' - An override for the @StartAfterDateTime@ of a @RefreshSchedule@. Make
-- sure that the @StartAfterDateTime@ is set to a time that takes place in
-- the future.
--
-- 'dataSetId', 'assetBundleImportJobRefreshScheduleOverrideParameters_dataSetId' - A partial identifier for the specific @RefreshSchedule@ resource that is
-- being overridden. This structure is used together with the @ScheduleID@
-- structure.
--
-- 'scheduleId', 'assetBundleImportJobRefreshScheduleOverrideParameters_scheduleId' - A partial identifier for the specific @RefreshSchedule@ resource being
-- overridden. This structure is used together with the @DataSetId@
-- structure.
newAssetBundleImportJobRefreshScheduleOverrideParameters ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'scheduleId'
  Prelude.Text ->
  AssetBundleImportJobRefreshScheduleOverrideParameters
newAssetBundleImportJobRefreshScheduleOverrideParameters
  pDataSetId_
  pScheduleId_ =
    AssetBundleImportJobRefreshScheduleOverrideParameters'
      { startAfterDateTime =
          Prelude.Nothing,
        dataSetId =
          pDataSetId_,
        scheduleId =
          pScheduleId_
      }

-- | An override for the @StartAfterDateTime@ of a @RefreshSchedule@. Make
-- sure that the @StartAfterDateTime@ is set to a time that takes place in
-- the future.
assetBundleImportJobRefreshScheduleOverrideParameters_startAfterDateTime :: Lens.Lens' AssetBundleImportJobRefreshScheduleOverrideParameters (Prelude.Maybe Prelude.UTCTime)
assetBundleImportJobRefreshScheduleOverrideParameters_startAfterDateTime = Lens.lens (\AssetBundleImportJobRefreshScheduleOverrideParameters' {startAfterDateTime} -> startAfterDateTime) (\s@AssetBundleImportJobRefreshScheduleOverrideParameters' {} a -> s {startAfterDateTime = a} :: AssetBundleImportJobRefreshScheduleOverrideParameters) Prelude.. Lens.mapping Data._Time

-- | A partial identifier for the specific @RefreshSchedule@ resource that is
-- being overridden. This structure is used together with the @ScheduleID@
-- structure.
assetBundleImportJobRefreshScheduleOverrideParameters_dataSetId :: Lens.Lens' AssetBundleImportJobRefreshScheduleOverrideParameters Prelude.Text
assetBundleImportJobRefreshScheduleOverrideParameters_dataSetId = Lens.lens (\AssetBundleImportJobRefreshScheduleOverrideParameters' {dataSetId} -> dataSetId) (\s@AssetBundleImportJobRefreshScheduleOverrideParameters' {} a -> s {dataSetId = a} :: AssetBundleImportJobRefreshScheduleOverrideParameters)

-- | A partial identifier for the specific @RefreshSchedule@ resource being
-- overridden. This structure is used together with the @DataSetId@
-- structure.
assetBundleImportJobRefreshScheduleOverrideParameters_scheduleId :: Lens.Lens' AssetBundleImportJobRefreshScheduleOverrideParameters Prelude.Text
assetBundleImportJobRefreshScheduleOverrideParameters_scheduleId = Lens.lens (\AssetBundleImportJobRefreshScheduleOverrideParameters' {scheduleId} -> scheduleId) (\s@AssetBundleImportJobRefreshScheduleOverrideParameters' {} a -> s {scheduleId = a} :: AssetBundleImportJobRefreshScheduleOverrideParameters)

instance
  Data.FromJSON
    AssetBundleImportJobRefreshScheduleOverrideParameters
  where
  parseJSON =
    Data.withObject
      "AssetBundleImportJobRefreshScheduleOverrideParameters"
      ( \x ->
          AssetBundleImportJobRefreshScheduleOverrideParameters'
            Prelude.<$> (x Data..:? "StartAfterDateTime")
            Prelude.<*> (x Data..: "DataSetId")
            Prelude.<*> (x Data..: "ScheduleId")
      )

instance
  Prelude.Hashable
    AssetBundleImportJobRefreshScheduleOverrideParameters
  where
  hashWithSalt
    _salt
    AssetBundleImportJobRefreshScheduleOverrideParameters' {..} =
      _salt
        `Prelude.hashWithSalt` startAfterDateTime
        `Prelude.hashWithSalt` dataSetId
        `Prelude.hashWithSalt` scheduleId

instance
  Prelude.NFData
    AssetBundleImportJobRefreshScheduleOverrideParameters
  where
  rnf
    AssetBundleImportJobRefreshScheduleOverrideParameters' {..} =
      Prelude.rnf startAfterDateTime
        `Prelude.seq` Prelude.rnf dataSetId
        `Prelude.seq` Prelude.rnf scheduleId

instance
  Data.ToJSON
    AssetBundleImportJobRefreshScheduleOverrideParameters
  where
  toJSON
    AssetBundleImportJobRefreshScheduleOverrideParameters' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("StartAfterDateTime" Data..=)
                Prelude.<$> startAfterDateTime,
              Prelude.Just ("DataSetId" Data..= dataSetId),
              Prelude.Just ("ScheduleId" Data..= scheduleId)
            ]
        )
