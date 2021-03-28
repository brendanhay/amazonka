{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails
  ( LambdaFunctionScheduleFailedEventDetails (..)
  -- * Smart constructor
  , mkLambdaFunctionScheduleFailedEventDetails
  -- * Lenses
  , lfsfedCause
  , lfsfedError
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.SensitiveCause as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveError as Types

-- | Contains details about a failed lambda function schedule event that occurred during an execution.
--
-- /See:/ 'mkLambdaFunctionScheduleFailedEventDetails' smart constructor.
data LambdaFunctionScheduleFailedEventDetails = LambdaFunctionScheduleFailedEventDetails'
  { cause :: Core.Maybe Types.SensitiveCause
    -- ^ A more detailed explanation of the cause of the failure.
  , error :: Core.Maybe Types.SensitiveError
    -- ^ The error code of the failure.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaFunctionScheduleFailedEventDetails' value with any optional fields omitted.
mkLambdaFunctionScheduleFailedEventDetails
    :: LambdaFunctionScheduleFailedEventDetails
mkLambdaFunctionScheduleFailedEventDetails
  = LambdaFunctionScheduleFailedEventDetails'{cause = Core.Nothing,
                                              error = Core.Nothing}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsfedCause :: Lens.Lens' LambdaFunctionScheduleFailedEventDetails (Core.Maybe Types.SensitiveCause)
lfsfedCause = Lens.field @"cause"
{-# INLINEABLE lfsfedCause #-}
{-# DEPRECATED cause "Use generic-lens or generic-optics with 'cause' instead"  #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsfedError :: Lens.Lens' LambdaFunctionScheduleFailedEventDetails (Core.Maybe Types.SensitiveError)
lfsfedError = Lens.field @"error"
{-# INLINEABLE lfsfedError #-}
{-# DEPRECATED error "Use generic-lens or generic-optics with 'error' instead"  #-}

instance Core.FromJSON LambdaFunctionScheduleFailedEventDetails
         where
        parseJSON
          = Core.withObject "LambdaFunctionScheduleFailedEventDetails" Core.$
              \ x ->
                LambdaFunctionScheduleFailedEventDetails' Core.<$>
                  (x Core..:? "cause") Core.<*> x Core..:? "error"
