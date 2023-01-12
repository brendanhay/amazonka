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
-- Module      : Amazonka.MediaLive.Types.BatchScheduleActionDeleteResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.BatchScheduleActionDeleteResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.ScheduleAction
import qualified Amazonka.Prelude as Prelude

-- | List of actions that have been deleted from the schedule.
--
-- /See:/ 'newBatchScheduleActionDeleteResult' smart constructor.
data BatchScheduleActionDeleteResult = BatchScheduleActionDeleteResult'
  { -- | List of actions that have been deleted from the schedule.
    scheduleActions :: [ScheduleAction]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchScheduleActionDeleteResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleActions', 'batchScheduleActionDeleteResult_scheduleActions' - List of actions that have been deleted from the schedule.
newBatchScheduleActionDeleteResult ::
  BatchScheduleActionDeleteResult
newBatchScheduleActionDeleteResult =
  BatchScheduleActionDeleteResult'
    { scheduleActions =
        Prelude.mempty
    }

-- | List of actions that have been deleted from the schedule.
batchScheduleActionDeleteResult_scheduleActions :: Lens.Lens' BatchScheduleActionDeleteResult [ScheduleAction]
batchScheduleActionDeleteResult_scheduleActions = Lens.lens (\BatchScheduleActionDeleteResult' {scheduleActions} -> scheduleActions) (\s@BatchScheduleActionDeleteResult' {} a -> s {scheduleActions = a} :: BatchScheduleActionDeleteResult) Prelude.. Lens.coerced

instance
  Data.FromJSON
    BatchScheduleActionDeleteResult
  where
  parseJSON =
    Data.withObject
      "BatchScheduleActionDeleteResult"
      ( \x ->
          BatchScheduleActionDeleteResult'
            Prelude.<$> ( x Data..:? "scheduleActions"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    BatchScheduleActionDeleteResult
  where
  hashWithSalt
    _salt
    BatchScheduleActionDeleteResult' {..} =
      _salt `Prelude.hashWithSalt` scheduleActions

instance
  Prelude.NFData
    BatchScheduleActionDeleteResult
  where
  rnf BatchScheduleActionDeleteResult' {..} =
    Prelude.rnf scheduleActions
