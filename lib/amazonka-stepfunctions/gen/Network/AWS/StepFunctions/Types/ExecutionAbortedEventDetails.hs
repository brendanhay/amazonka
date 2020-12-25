{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionAbortedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionAbortedEventDetails
  ( ExecutionAbortedEventDetails (..),

    -- * Smart constructor
    mkExecutionAbortedEventDetails,

    -- * Lenses
    eaedCause,
    eaedError,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.SensitiveCause as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveError as Types

-- | Contains details about an abort of an execution.
--
-- /See:/ 'mkExecutionAbortedEventDetails' smart constructor.
data ExecutionAbortedEventDetails = ExecutionAbortedEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Core.Maybe Types.SensitiveCause,
    -- | The error code of the failure.
    error :: Core.Maybe Types.SensitiveError
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecutionAbortedEventDetails' value with any optional fields omitted.
mkExecutionAbortedEventDetails ::
  ExecutionAbortedEventDetails
mkExecutionAbortedEventDetails =
  ExecutionAbortedEventDetails'
    { cause = Core.Nothing,
      error = Core.Nothing
    }

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaedCause :: Lens.Lens' ExecutionAbortedEventDetails (Core.Maybe Types.SensitiveCause)
eaedCause = Lens.field @"cause"
{-# DEPRECATED eaedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaedError :: Lens.Lens' ExecutionAbortedEventDetails (Core.Maybe Types.SensitiveError)
eaedError = Lens.field @"error"
{-# DEPRECATED eaedError "Use generic-lens or generic-optics with 'error' instead." #-}

instance Core.FromJSON ExecutionAbortedEventDetails where
  parseJSON =
    Core.withObject "ExecutionAbortedEventDetails" Core.$
      \x ->
        ExecutionAbortedEventDetails'
          Core.<$> (x Core..:? "cause") Core.<*> (x Core..:? "error")
