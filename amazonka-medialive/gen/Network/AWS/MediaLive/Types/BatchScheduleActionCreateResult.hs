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
-- Module      : Network.AWS.MediaLive.Types.BatchScheduleActionCreateResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchScheduleActionCreateResult where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ScheduleAction
import qualified Network.AWS.Prelude as Prelude

-- | List of actions that have been created in the schedule.
--
-- /See:/ 'newBatchScheduleActionCreateResult' smart constructor.
data BatchScheduleActionCreateResult = BatchScheduleActionCreateResult'
  { -- | List of actions that have been created in the schedule.
    scheduleActions :: [ScheduleAction]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchScheduleActionCreateResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleActions', 'batchScheduleActionCreateResult_scheduleActions' - List of actions that have been created in the schedule.
newBatchScheduleActionCreateResult ::
  BatchScheduleActionCreateResult
newBatchScheduleActionCreateResult =
  BatchScheduleActionCreateResult'
    { scheduleActions =
        Prelude.mempty
    }

-- | List of actions that have been created in the schedule.
batchScheduleActionCreateResult_scheduleActions :: Lens.Lens' BatchScheduleActionCreateResult [ScheduleAction]
batchScheduleActionCreateResult_scheduleActions = Lens.lens (\BatchScheduleActionCreateResult' {scheduleActions} -> scheduleActions) (\s@BatchScheduleActionCreateResult' {} a -> s {scheduleActions = a} :: BatchScheduleActionCreateResult) Prelude.. Prelude._Coerce

instance
  Prelude.FromJSON
    BatchScheduleActionCreateResult
  where
  parseJSON =
    Prelude.withObject
      "BatchScheduleActionCreateResult"
      ( \x ->
          BatchScheduleActionCreateResult'
            Prelude.<$> ( x Prelude..:? "scheduleActions"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    BatchScheduleActionCreateResult

instance
  Prelude.NFData
    BatchScheduleActionCreateResult
