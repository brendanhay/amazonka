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
-- Module      : Network.AWS.StepFunctions.Types.ActivityStartedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityStartedEventDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details about the start of an activity during an execution.
--
-- /See:/ 'newActivityStartedEventDetails' smart constructor.
data ActivityStartedEventDetails = ActivityStartedEventDetails'
  { -- | The name of the worker that the task is assigned to. These names are
    -- provided by the workers when calling GetActivityTask.
    workerName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActivityStartedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workerName', 'activityStartedEventDetails_workerName' - The name of the worker that the task is assigned to. These names are
-- provided by the workers when calling GetActivityTask.
newActivityStartedEventDetails ::
  ActivityStartedEventDetails
newActivityStartedEventDetails =
  ActivityStartedEventDetails'
    { workerName =
        Core.Nothing
    }

-- | The name of the worker that the task is assigned to. These names are
-- provided by the workers when calling GetActivityTask.
activityStartedEventDetails_workerName :: Lens.Lens' ActivityStartedEventDetails (Core.Maybe Core.Text)
activityStartedEventDetails_workerName = Lens.lens (\ActivityStartedEventDetails' {workerName} -> workerName) (\s@ActivityStartedEventDetails' {} a -> s {workerName = a} :: ActivityStartedEventDetails)

instance Core.FromJSON ActivityStartedEventDetails where
  parseJSON =
    Core.withObject
      "ActivityStartedEventDetails"
      ( \x ->
          ActivityStartedEventDetails'
            Core.<$> (x Core..:? "workerName")
      )

instance Core.Hashable ActivityStartedEventDetails

instance Core.NFData ActivityStartedEventDetails
