{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityScheduleFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityScheduleFailedEventDetails
  ( ActivityScheduleFailedEventDetails (..),

    -- * Smart constructor
    mkActivityScheduleFailedEventDetails,

    -- * Lenses
    asfedCause,
    asfedError,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.SensitiveCause as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveError as Types

-- | Contains details about an activity schedule failure that occurred during an execution.
--
-- /See:/ 'mkActivityScheduleFailedEventDetails' smart constructor.
data ActivityScheduleFailedEventDetails = ActivityScheduleFailedEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Core.Maybe Types.SensitiveCause,
    -- | The error code of the failure.
    error :: Core.Maybe Types.SensitiveError
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivityScheduleFailedEventDetails' value with any optional fields omitted.
mkActivityScheduleFailedEventDetails ::
  ActivityScheduleFailedEventDetails
mkActivityScheduleFailedEventDetails =
  ActivityScheduleFailedEventDetails'
    { cause = Core.Nothing,
      error = Core.Nothing
    }

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asfedCause :: Lens.Lens' ActivityScheduleFailedEventDetails (Core.Maybe Types.SensitiveCause)
asfedCause = Lens.field @"cause"
{-# DEPRECATED asfedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asfedError :: Lens.Lens' ActivityScheduleFailedEventDetails (Core.Maybe Types.SensitiveError)
asfedError = Lens.field @"error"
{-# DEPRECATED asfedError "Use generic-lens or generic-optics with 'error' instead." #-}

instance Core.FromJSON ActivityScheduleFailedEventDetails where
  parseJSON =
    Core.withObject "ActivityScheduleFailedEventDetails" Core.$
      \x ->
        ActivityScheduleFailedEventDetails'
          Core.<$> (x Core..:? "cause") Core.<*> (x Core..:? "error")
