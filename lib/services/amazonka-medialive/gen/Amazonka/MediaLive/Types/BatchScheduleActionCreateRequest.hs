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
-- Module      : Amazonka.MediaLive.Types.BatchScheduleActionCreateRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.BatchScheduleActionCreateRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.ScheduleAction
import qualified Amazonka.Prelude as Prelude

-- | A list of schedule actions to create (in a request) or that have been
-- created (in a response).
--
-- /See:/ 'newBatchScheduleActionCreateRequest' smart constructor.
data BatchScheduleActionCreateRequest = BatchScheduleActionCreateRequest'
  { -- | A list of schedule actions to create.
    scheduleActions :: [ScheduleAction]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
batchScheduleActionCreateRequest_scheduleActions = Lens.lens (\BatchScheduleActionCreateRequest' {scheduleActions} -> scheduleActions) (\s@BatchScheduleActionCreateRequest' {} a -> s {scheduleActions = a} :: BatchScheduleActionCreateRequest) Prelude.. Lens.coerced

instance
  Prelude.Hashable
    BatchScheduleActionCreateRequest
  where
  hashWithSalt
    _salt
    BatchScheduleActionCreateRequest' {..} =
      _salt `Prelude.hashWithSalt` scheduleActions

instance
  Prelude.NFData
    BatchScheduleActionCreateRequest
  where
  rnf BatchScheduleActionCreateRequest' {..} =
    Prelude.rnf scheduleActions

instance Core.ToJSON BatchScheduleActionCreateRequest where
  toJSON BatchScheduleActionCreateRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("scheduleActions" Core..= scheduleActions)
          ]
      )
