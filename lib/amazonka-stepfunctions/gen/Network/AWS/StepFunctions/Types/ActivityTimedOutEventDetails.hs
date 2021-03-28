{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityTimedOutEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.ActivityTimedOutEventDetails
  ( ActivityTimedOutEventDetails (..)
  -- * Smart constructor
  , mkActivityTimedOutEventDetails
  -- * Lenses
  , atoedCause
  , atoedError
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.SensitiveCause as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveError as Types

-- | Contains details about an activity timeout that occurred during an execution.
--
-- /See:/ 'mkActivityTimedOutEventDetails' smart constructor.
data ActivityTimedOutEventDetails = ActivityTimedOutEventDetails'
  { cause :: Core.Maybe Types.SensitiveCause
    -- ^ A more detailed explanation of the cause of the timeout.
  , error :: Core.Maybe Types.SensitiveError
    -- ^ The error code of the failure.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivityTimedOutEventDetails' value with any optional fields omitted.
mkActivityTimedOutEventDetails
    :: ActivityTimedOutEventDetails
mkActivityTimedOutEventDetails
  = ActivityTimedOutEventDetails'{cause = Core.Nothing,
                                  error = Core.Nothing}

-- | A more detailed explanation of the cause of the timeout.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atoedCause :: Lens.Lens' ActivityTimedOutEventDetails (Core.Maybe Types.SensitiveCause)
atoedCause = Lens.field @"cause"
{-# INLINEABLE atoedCause #-}
{-# DEPRECATED cause "Use generic-lens or generic-optics with 'cause' instead"  #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atoedError :: Lens.Lens' ActivityTimedOutEventDetails (Core.Maybe Types.SensitiveError)
atoedError = Lens.field @"error"
{-# INLINEABLE atoedError #-}
{-# DEPRECATED error "Use generic-lens or generic-optics with 'error' instead"  #-}

instance Core.FromJSON ActivityTimedOutEventDetails where
        parseJSON
          = Core.withObject "ActivityTimedOutEventDetails" Core.$
              \ x ->
                ActivityTimedOutEventDetails' Core.<$>
                  (x Core..:? "cause") Core.<*> x Core..:? "error"
