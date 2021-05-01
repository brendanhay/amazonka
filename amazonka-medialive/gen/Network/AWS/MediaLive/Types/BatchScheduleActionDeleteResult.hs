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
-- Module      : Network.AWS.MediaLive.Types.BatchScheduleActionDeleteResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchScheduleActionDeleteResult where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ScheduleAction
import qualified Network.AWS.Prelude as Prelude

-- | List of actions that have been deleted from the schedule.
--
-- /See:/ 'newBatchScheduleActionDeleteResult' smart constructor.
data BatchScheduleActionDeleteResult = BatchScheduleActionDeleteResult'
  { -- | List of actions that have been deleted from the schedule.
    scheduleActions :: [ScheduleAction]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
batchScheduleActionDeleteResult_scheduleActions = Lens.lens (\BatchScheduleActionDeleteResult' {scheduleActions} -> scheduleActions) (\s@BatchScheduleActionDeleteResult' {} a -> s {scheduleActions = a} :: BatchScheduleActionDeleteResult) Prelude.. Prelude._Coerce

instance
  Prelude.FromJSON
    BatchScheduleActionDeleteResult
  where
  parseJSON =
    Prelude.withObject
      "BatchScheduleActionDeleteResult"
      ( \x ->
          BatchScheduleActionDeleteResult'
            Prelude.<$> ( x Prelude..:? "scheduleActions"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    BatchScheduleActionDeleteResult

instance
  Prelude.NFData
    BatchScheduleActionDeleteResult
