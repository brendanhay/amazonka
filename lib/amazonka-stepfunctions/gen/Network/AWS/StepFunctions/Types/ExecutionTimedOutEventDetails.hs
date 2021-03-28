{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionTimedOutEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.ExecutionTimedOutEventDetails
  ( ExecutionTimedOutEventDetails (..)
  -- * Smart constructor
  , mkExecutionTimedOutEventDetails
  -- * Lenses
  , etoedCause
  , etoedError
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.SensitiveCause as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveError as Types

-- | Contains details about the execution timeout that occurred during the execution.
--
-- /See:/ 'mkExecutionTimedOutEventDetails' smart constructor.
data ExecutionTimedOutEventDetails = ExecutionTimedOutEventDetails'
  { cause :: Core.Maybe Types.SensitiveCause
    -- ^ A more detailed explanation of the cause of the timeout.
  , error :: Core.Maybe Types.SensitiveError
    -- ^ The error code of the failure.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecutionTimedOutEventDetails' value with any optional fields omitted.
mkExecutionTimedOutEventDetails
    :: ExecutionTimedOutEventDetails
mkExecutionTimedOutEventDetails
  = ExecutionTimedOutEventDetails'{cause = Core.Nothing,
                                   error = Core.Nothing}

-- | A more detailed explanation of the cause of the timeout.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etoedCause :: Lens.Lens' ExecutionTimedOutEventDetails (Core.Maybe Types.SensitiveCause)
etoedCause = Lens.field @"cause"
{-# INLINEABLE etoedCause #-}
{-# DEPRECATED cause "Use generic-lens or generic-optics with 'cause' instead"  #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etoedError :: Lens.Lens' ExecutionTimedOutEventDetails (Core.Maybe Types.SensitiveError)
etoedError = Lens.field @"error"
{-# INLINEABLE etoedError #-}
{-# DEPRECATED error "Use generic-lens or generic-optics with 'error' instead"  #-}

instance Core.FromJSON ExecutionTimedOutEventDetails where
        parseJSON
          = Core.withObject "ExecutionTimedOutEventDetails" Core.$
              \ x ->
                ExecutionTimedOutEventDetails' Core.<$>
                  (x Core..:? "cause") Core.<*> x Core..:? "error"
