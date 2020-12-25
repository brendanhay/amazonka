{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentStatus
  ( TrialComponentStatus (..),

    -- * Smart constructor
    mkTrialComponentStatus,

    -- * Lenses
    tcsMessage,
    tcsPrimaryStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentStatusMessage as Types

-- | The status of the trial component.
--
-- /See:/ 'mkTrialComponentStatus' smart constructor.
data TrialComponentStatus = TrialComponentStatus'
  { -- | If the component failed, a message describing why.
    message :: Core.Maybe Types.TrialComponentStatusMessage,
    -- | The status of the trial component.
    primaryStatus :: Core.Maybe Types.TrialComponentPrimaryStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrialComponentStatus' value with any optional fields omitted.
mkTrialComponentStatus ::
  TrialComponentStatus
mkTrialComponentStatus =
  TrialComponentStatus'
    { message = Core.Nothing,
      primaryStatus = Core.Nothing
    }

-- | If the component failed, a message describing why.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsMessage :: Lens.Lens' TrialComponentStatus (Core.Maybe Types.TrialComponentStatusMessage)
tcsMessage = Lens.field @"message"
{-# DEPRECATED tcsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The status of the trial component.
--
-- /Note:/ Consider using 'primaryStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsPrimaryStatus :: Lens.Lens' TrialComponentStatus (Core.Maybe Types.TrialComponentPrimaryStatus)
tcsPrimaryStatus = Lens.field @"primaryStatus"
{-# DEPRECATED tcsPrimaryStatus "Use generic-lens or generic-optics with 'primaryStatus' instead." #-}

instance Core.FromJSON TrialComponentStatus where
  toJSON TrialComponentStatus {..} =
    Core.object
      ( Core.catMaybes
          [ ("Message" Core..=) Core.<$> message,
            ("PrimaryStatus" Core..=) Core.<$> primaryStatus
          ]
      )

instance Core.FromJSON TrialComponentStatus where
  parseJSON =
    Core.withObject "TrialComponentStatus" Core.$
      \x ->
        TrialComponentStatus'
          Core.<$> (x Core..:? "Message") Core.<*> (x Core..:? "PrimaryStatus")
