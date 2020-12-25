{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityScheduledEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityScheduledEventDetails
  ( ActivityScheduledEventDetails (..),

    -- * Smart constructor
    mkActivityScheduledEventDetails,

    -- * Lenses
    asedResource,
    asedHeartbeatInSeconds,
    asedInput,
    asedInputDetails,
    asedTimeoutInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.Arn as Types
import qualified Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveData as Types

-- | Contains details about an activity scheduled during an execution.
--
-- /See:/ 'mkActivityScheduledEventDetails' smart constructor.
data ActivityScheduledEventDetails = ActivityScheduledEventDetails'
  { -- | The Amazon Resource Name (ARN) of the scheduled activity.
    resource :: Types.Arn,
    -- | The maximum allowed duration between two heartbeats for the activity task.
    heartbeatInSeconds :: Core.Maybe Core.Integer,
    -- | The JSON data input to the activity task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
    input :: Core.Maybe Types.SensitiveData,
    -- | Contains details about the input for an execution history event.
    inputDetails :: Core.Maybe Types.HistoryEventExecutionDataDetails,
    -- | The maximum allowed duration of the activity task.
    timeoutInSeconds :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivityScheduledEventDetails' value with any optional fields omitted.
mkActivityScheduledEventDetails ::
  -- | 'resource'
  Types.Arn ->
  ActivityScheduledEventDetails
mkActivityScheduledEventDetails resource =
  ActivityScheduledEventDetails'
    { resource,
      heartbeatInSeconds = Core.Nothing,
      input = Core.Nothing,
      inputDetails = Core.Nothing,
      timeoutInSeconds = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the scheduled activity.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedResource :: Lens.Lens' ActivityScheduledEventDetails Types.Arn
asedResource = Lens.field @"resource"
{-# DEPRECATED asedResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | The maximum allowed duration between two heartbeats for the activity task.
--
-- /Note:/ Consider using 'heartbeatInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedHeartbeatInSeconds :: Lens.Lens' ActivityScheduledEventDetails (Core.Maybe Core.Integer)
asedHeartbeatInSeconds = Lens.field @"heartbeatInSeconds"
{-# DEPRECATED asedHeartbeatInSeconds "Use generic-lens or generic-optics with 'heartbeatInSeconds' instead." #-}

-- | The JSON data input to the activity task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedInput :: Lens.Lens' ActivityScheduledEventDetails (Core.Maybe Types.SensitiveData)
asedInput = Lens.field @"input"
{-# DEPRECATED asedInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | Contains details about the input for an execution history event.
--
-- /Note:/ Consider using 'inputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedInputDetails :: Lens.Lens' ActivityScheduledEventDetails (Core.Maybe Types.HistoryEventExecutionDataDetails)
asedInputDetails = Lens.field @"inputDetails"
{-# DEPRECATED asedInputDetails "Use generic-lens or generic-optics with 'inputDetails' instead." #-}

-- | The maximum allowed duration of the activity task.
--
-- /Note:/ Consider using 'timeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedTimeoutInSeconds :: Lens.Lens' ActivityScheduledEventDetails (Core.Maybe Core.Integer)
asedTimeoutInSeconds = Lens.field @"timeoutInSeconds"
{-# DEPRECATED asedTimeoutInSeconds "Use generic-lens or generic-optics with 'timeoutInSeconds' instead." #-}

instance Core.FromJSON ActivityScheduledEventDetails where
  parseJSON =
    Core.withObject "ActivityScheduledEventDetails" Core.$
      \x ->
        ActivityScheduledEventDetails'
          Core.<$> (x Core..: "resource")
          Core.<*> (x Core..:? "heartbeatInSeconds")
          Core.<*> (x Core..:? "input")
          Core.<*> (x Core..:? "inputDetails")
          Core.<*> (x Core..:? "timeoutInSeconds")
