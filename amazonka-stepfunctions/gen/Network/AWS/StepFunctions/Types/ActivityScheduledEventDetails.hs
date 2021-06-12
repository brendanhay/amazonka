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
-- Module      : Network.AWS.StepFunctions.Types.ActivityScheduledEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityScheduledEventDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about an activity scheduled during an execution.
--
-- /See:/ 'newActivityScheduledEventDetails' smart constructor.
data ActivityScheduledEventDetails = ActivityScheduledEventDetails'
  { -- | The maximum allowed duration between two heartbeats for the activity
    -- task.
    heartbeatInSeconds :: Core.Maybe Core.Integer,
    -- | Contains details about the input for an execution history event.
    inputDetails :: Core.Maybe HistoryEventExecutionDataDetails,
    -- | The JSON data input to the activity task. Length constraints apply to
    -- the payload size, and are expressed as bytes in UTF-8 encoding.
    input :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The maximum allowed duration of the activity task.
    timeoutInSeconds :: Core.Maybe Core.Integer,
    -- | The Amazon Resource Name (ARN) of the scheduled activity.
    resource :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActivityScheduledEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'heartbeatInSeconds', 'activityScheduledEventDetails_heartbeatInSeconds' - The maximum allowed duration between two heartbeats for the activity
-- task.
--
-- 'inputDetails', 'activityScheduledEventDetails_inputDetails' - Contains details about the input for an execution history event.
--
-- 'input', 'activityScheduledEventDetails_input' - The JSON data input to the activity task. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- 'timeoutInSeconds', 'activityScheduledEventDetails_timeoutInSeconds' - The maximum allowed duration of the activity task.
--
-- 'resource', 'activityScheduledEventDetails_resource' - The Amazon Resource Name (ARN) of the scheduled activity.
newActivityScheduledEventDetails ::
  -- | 'resource'
  Core.Text ->
  ActivityScheduledEventDetails
newActivityScheduledEventDetails pResource_ =
  ActivityScheduledEventDetails'
    { heartbeatInSeconds =
        Core.Nothing,
      inputDetails = Core.Nothing,
      input = Core.Nothing,
      timeoutInSeconds = Core.Nothing,
      resource = pResource_
    }

-- | The maximum allowed duration between two heartbeats for the activity
-- task.
activityScheduledEventDetails_heartbeatInSeconds :: Lens.Lens' ActivityScheduledEventDetails (Core.Maybe Core.Integer)
activityScheduledEventDetails_heartbeatInSeconds = Lens.lens (\ActivityScheduledEventDetails' {heartbeatInSeconds} -> heartbeatInSeconds) (\s@ActivityScheduledEventDetails' {} a -> s {heartbeatInSeconds = a} :: ActivityScheduledEventDetails)

-- | Contains details about the input for an execution history event.
activityScheduledEventDetails_inputDetails :: Lens.Lens' ActivityScheduledEventDetails (Core.Maybe HistoryEventExecutionDataDetails)
activityScheduledEventDetails_inputDetails = Lens.lens (\ActivityScheduledEventDetails' {inputDetails} -> inputDetails) (\s@ActivityScheduledEventDetails' {} a -> s {inputDetails = a} :: ActivityScheduledEventDetails)

-- | The JSON data input to the activity task. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
activityScheduledEventDetails_input :: Lens.Lens' ActivityScheduledEventDetails (Core.Maybe Core.Text)
activityScheduledEventDetails_input = Lens.lens (\ActivityScheduledEventDetails' {input} -> input) (\s@ActivityScheduledEventDetails' {} a -> s {input = a} :: ActivityScheduledEventDetails) Core.. Lens.mapping Core._Sensitive

-- | The maximum allowed duration of the activity task.
activityScheduledEventDetails_timeoutInSeconds :: Lens.Lens' ActivityScheduledEventDetails (Core.Maybe Core.Integer)
activityScheduledEventDetails_timeoutInSeconds = Lens.lens (\ActivityScheduledEventDetails' {timeoutInSeconds} -> timeoutInSeconds) (\s@ActivityScheduledEventDetails' {} a -> s {timeoutInSeconds = a} :: ActivityScheduledEventDetails)

-- | The Amazon Resource Name (ARN) of the scheduled activity.
activityScheduledEventDetails_resource :: Lens.Lens' ActivityScheduledEventDetails Core.Text
activityScheduledEventDetails_resource = Lens.lens (\ActivityScheduledEventDetails' {resource} -> resource) (\s@ActivityScheduledEventDetails' {} a -> s {resource = a} :: ActivityScheduledEventDetails)

instance Core.FromJSON ActivityScheduledEventDetails where
  parseJSON =
    Core.withObject
      "ActivityScheduledEventDetails"
      ( \x ->
          ActivityScheduledEventDetails'
            Core.<$> (x Core..:? "heartbeatInSeconds")
            Core.<*> (x Core..:? "inputDetails")
            Core.<*> (x Core..:? "input")
            Core.<*> (x Core..:? "timeoutInSeconds")
            Core.<*> (x Core..: "resource")
      )

instance Core.Hashable ActivityScheduledEventDetails

instance Core.NFData ActivityScheduledEventDetails
