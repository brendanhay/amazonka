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
-- Module      : Network.AWS.StepFunctions.Types.ActivityScheduleFailedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityScheduleFailedEventDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details about an activity schedule failure that occurred during
-- an execution.
--
-- /See:/ 'newActivityScheduleFailedEventDetails' smart constructor.
data ActivityScheduleFailedEventDetails = ActivityScheduleFailedEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The error code of the failure.
    error :: Core.Maybe (Core.Sensitive Core.Text)
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActivityScheduleFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'activityScheduleFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'activityScheduleFailedEventDetails_error' - The error code of the failure.
newActivityScheduleFailedEventDetails ::
  ActivityScheduleFailedEventDetails
newActivityScheduleFailedEventDetails =
  ActivityScheduleFailedEventDetails'
    { cause =
        Core.Nothing,
      error = Core.Nothing
    }

-- | A more detailed explanation of the cause of the failure.
activityScheduleFailedEventDetails_cause :: Lens.Lens' ActivityScheduleFailedEventDetails (Core.Maybe Core.Text)
activityScheduleFailedEventDetails_cause = Lens.lens (\ActivityScheduleFailedEventDetails' {cause} -> cause) (\s@ActivityScheduleFailedEventDetails' {} a -> s {cause = a} :: ActivityScheduleFailedEventDetails) Core.. Lens.mapping Core._Sensitive

-- | The error code of the failure.
activityScheduleFailedEventDetails_error :: Lens.Lens' ActivityScheduleFailedEventDetails (Core.Maybe Core.Text)
activityScheduleFailedEventDetails_error = Lens.lens (\ActivityScheduleFailedEventDetails' {error} -> error) (\s@ActivityScheduleFailedEventDetails' {} a -> s {error = a} :: ActivityScheduleFailedEventDetails) Core.. Lens.mapping Core._Sensitive

instance
  Core.FromJSON
    ActivityScheduleFailedEventDetails
  where
  parseJSON =
    Core.withObject
      "ActivityScheduleFailedEventDetails"
      ( \x ->
          ActivityScheduleFailedEventDetails'
            Core.<$> (x Core..:? "cause") Core.<*> (x Core..:? "error")
      )

instance
  Core.Hashable
    ActivityScheduleFailedEventDetails

instance
  Core.NFData
    ActivityScheduleFailedEventDetails
