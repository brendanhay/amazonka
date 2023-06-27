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
-- Module      : Amazonka.QuickSight.Types.RefreshSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RefreshSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.IngestionType
import Amazonka.QuickSight.Types.RefreshFrequency

-- | The refresh schedule of a dataset.
--
-- /See:/ 'newRefreshSchedule' smart constructor.
data RefreshSchedule = RefreshSchedule'
  { -- | The Amazon Resource Name (ARN) for the refresh schedule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Time after which the refresh schedule can be started, expressed in
    -- @YYYY-MM-DDTHH:MM:SS@ format.
    startAfterDateTime :: Prelude.Maybe Data.POSIX,
    -- | An identifier for the refresh schedule.
    scheduleId :: Prelude.Text,
    -- | The frequency for the refresh schedule.
    scheduleFrequency :: RefreshFrequency,
    -- | The type of refresh that a datset undergoes. Valid values are as
    -- follows:
    --
    -- -   @FULL_REFRESH@: A complete refresh of a dataset.
    --
    -- -   @INCREMENTAL_REFRESH@: A partial refresh of some rows of a dataset,
    --     based on the time window specified.
    --
    -- For more information on full and incremental refreshes, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/refreshing-imported-data.html Refreshing SPICE data>
    -- in the /Amazon QuickSight User Guide/.
    refreshType :: IngestionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RefreshSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'refreshSchedule_arn' - The Amazon Resource Name (ARN) for the refresh schedule.
--
-- 'startAfterDateTime', 'refreshSchedule_startAfterDateTime' - Time after which the refresh schedule can be started, expressed in
-- @YYYY-MM-DDTHH:MM:SS@ format.
--
-- 'scheduleId', 'refreshSchedule_scheduleId' - An identifier for the refresh schedule.
--
-- 'scheduleFrequency', 'refreshSchedule_scheduleFrequency' - The frequency for the refresh schedule.
--
-- 'refreshType', 'refreshSchedule_refreshType' - The type of refresh that a datset undergoes. Valid values are as
-- follows:
--
-- -   @FULL_REFRESH@: A complete refresh of a dataset.
--
-- -   @INCREMENTAL_REFRESH@: A partial refresh of some rows of a dataset,
--     based on the time window specified.
--
-- For more information on full and incremental refreshes, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/refreshing-imported-data.html Refreshing SPICE data>
-- in the /Amazon QuickSight User Guide/.
newRefreshSchedule ::
  -- | 'scheduleId'
  Prelude.Text ->
  -- | 'scheduleFrequency'
  RefreshFrequency ->
  -- | 'refreshType'
  IngestionType ->
  RefreshSchedule
newRefreshSchedule
  pScheduleId_
  pScheduleFrequency_
  pRefreshType_ =
    RefreshSchedule'
      { arn = Prelude.Nothing,
        startAfterDateTime = Prelude.Nothing,
        scheduleId = pScheduleId_,
        scheduleFrequency = pScheduleFrequency_,
        refreshType = pRefreshType_
      }

-- | The Amazon Resource Name (ARN) for the refresh schedule.
refreshSchedule_arn :: Lens.Lens' RefreshSchedule (Prelude.Maybe Prelude.Text)
refreshSchedule_arn = Lens.lens (\RefreshSchedule' {arn} -> arn) (\s@RefreshSchedule' {} a -> s {arn = a} :: RefreshSchedule)

-- | Time after which the refresh schedule can be started, expressed in
-- @YYYY-MM-DDTHH:MM:SS@ format.
refreshSchedule_startAfterDateTime :: Lens.Lens' RefreshSchedule (Prelude.Maybe Prelude.UTCTime)
refreshSchedule_startAfterDateTime = Lens.lens (\RefreshSchedule' {startAfterDateTime} -> startAfterDateTime) (\s@RefreshSchedule' {} a -> s {startAfterDateTime = a} :: RefreshSchedule) Prelude.. Lens.mapping Data._Time

-- | An identifier for the refresh schedule.
refreshSchedule_scheduleId :: Lens.Lens' RefreshSchedule Prelude.Text
refreshSchedule_scheduleId = Lens.lens (\RefreshSchedule' {scheduleId} -> scheduleId) (\s@RefreshSchedule' {} a -> s {scheduleId = a} :: RefreshSchedule)

-- | The frequency for the refresh schedule.
refreshSchedule_scheduleFrequency :: Lens.Lens' RefreshSchedule RefreshFrequency
refreshSchedule_scheduleFrequency = Lens.lens (\RefreshSchedule' {scheduleFrequency} -> scheduleFrequency) (\s@RefreshSchedule' {} a -> s {scheduleFrequency = a} :: RefreshSchedule)

-- | The type of refresh that a datset undergoes. Valid values are as
-- follows:
--
-- -   @FULL_REFRESH@: A complete refresh of a dataset.
--
-- -   @INCREMENTAL_REFRESH@: A partial refresh of some rows of a dataset,
--     based on the time window specified.
--
-- For more information on full and incremental refreshes, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/refreshing-imported-data.html Refreshing SPICE data>
-- in the /Amazon QuickSight User Guide/.
refreshSchedule_refreshType :: Lens.Lens' RefreshSchedule IngestionType
refreshSchedule_refreshType = Lens.lens (\RefreshSchedule' {refreshType} -> refreshType) (\s@RefreshSchedule' {} a -> s {refreshType = a} :: RefreshSchedule)

instance Data.FromJSON RefreshSchedule where
  parseJSON =
    Data.withObject
      "RefreshSchedule"
      ( \x ->
          RefreshSchedule'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "StartAfterDateTime")
            Prelude.<*> (x Data..: "ScheduleId")
            Prelude.<*> (x Data..: "ScheduleFrequency")
            Prelude.<*> (x Data..: "RefreshType")
      )

instance Prelude.Hashable RefreshSchedule where
  hashWithSalt _salt RefreshSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` startAfterDateTime
      `Prelude.hashWithSalt` scheduleId
      `Prelude.hashWithSalt` scheduleFrequency
      `Prelude.hashWithSalt` refreshType

instance Prelude.NFData RefreshSchedule where
  rnf RefreshSchedule' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf startAfterDateTime
      `Prelude.seq` Prelude.rnf scheduleId
      `Prelude.seq` Prelude.rnf scheduleFrequency
      `Prelude.seq` Prelude.rnf refreshType

instance Data.ToJSON RefreshSchedule where
  toJSON RefreshSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Arn" Data..=) Prelude.<$> arn,
            ("StartAfterDateTime" Data..=)
              Prelude.<$> startAfterDateTime,
            Prelude.Just ("ScheduleId" Data..= scheduleId),
            Prelude.Just
              ("ScheduleFrequency" Data..= scheduleFrequency),
            Prelude.Just ("RefreshType" Data..= refreshType)
          ]
      )
