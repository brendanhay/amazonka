{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepStateChangeReason
  ( StepStateChangeReason (..),

    -- * Smart constructor
    mkStepStateChangeReason,

    -- * Lenses
    sscrCode,
    sscrMessage,
  )
where

import qualified Network.AWS.EMR.Types.StepStateChangeReasonCode as Types
import qualified Network.AWS.EMR.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of the step state change reason.
--
-- /See:/ 'mkStepStateChangeReason' smart constructor.
data StepStateChangeReason = StepStateChangeReason'
  { -- | The programmable code for the state change reason. Note: Currently, the service provides no code for the state change.
    code :: Core.Maybe Types.StepStateChangeReasonCode,
    -- | The descriptive message for the state change reason.
    message :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StepStateChangeReason' value with any optional fields omitted.
mkStepStateChangeReason ::
  StepStateChangeReason
mkStepStateChangeReason =
  StepStateChangeReason'
    { code = Core.Nothing,
      message = Core.Nothing
    }

-- | The programmable code for the state change reason. Note: Currently, the service provides no code for the state change.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscrCode :: Lens.Lens' StepStateChangeReason (Core.Maybe Types.StepStateChangeReasonCode)
sscrCode = Lens.field @"code"
{-# DEPRECATED sscrCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The descriptive message for the state change reason.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscrMessage :: Lens.Lens' StepStateChangeReason (Core.Maybe Types.String)
sscrMessage = Lens.field @"message"
{-# DEPRECATED sscrMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON StepStateChangeReason where
  parseJSON =
    Core.withObject "StepStateChangeReason" Core.$
      \x ->
        StepStateChangeReason'
          Core.<$> (x Core..:? "Code") Core.<*> (x Core..:? "Message")
