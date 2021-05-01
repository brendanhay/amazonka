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
-- Module      : Network.AWS.MediaLive.Types.BatchScheduleActionCreateRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchScheduleActionCreateRequest where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ScheduleAction
import qualified Network.AWS.Prelude as Prelude

-- | A list of schedule actions to create (in a request) or that have been
-- created (in a response).
--
-- /See:/ 'newBatchScheduleActionCreateRequest' smart constructor.
data BatchScheduleActionCreateRequest = BatchScheduleActionCreateRequest'
  { -- | A list of schedule actions to create.
    scheduleActions :: [ScheduleAction]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchScheduleActionCreateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleActions', 'batchScheduleActionCreateRequest_scheduleActions' - A list of schedule actions to create.
newBatchScheduleActionCreateRequest ::
  BatchScheduleActionCreateRequest
newBatchScheduleActionCreateRequest =
  BatchScheduleActionCreateRequest'
    { scheduleActions =
        Prelude.mempty
    }

-- | A list of schedule actions to create.
batchScheduleActionCreateRequest_scheduleActions :: Lens.Lens' BatchScheduleActionCreateRequest [ScheduleAction]
batchScheduleActionCreateRequest_scheduleActions = Lens.lens (\BatchScheduleActionCreateRequest' {scheduleActions} -> scheduleActions) (\s@BatchScheduleActionCreateRequest' {} a -> s {scheduleActions = a} :: BatchScheduleActionCreateRequest) Prelude.. Prelude._Coerce

instance
  Prelude.Hashable
    BatchScheduleActionCreateRequest

instance
  Prelude.NFData
    BatchScheduleActionCreateRequest

instance
  Prelude.ToJSON
    BatchScheduleActionCreateRequest
  where
  toJSON BatchScheduleActionCreateRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("scheduleActions" Prelude..= scheduleActions)
          ]
      )
