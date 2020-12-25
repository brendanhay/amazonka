{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyStatus
  ( AutoScalingPolicyStatus (..),

    -- * Smart constructor
    mkAutoScalingPolicyStatus,

    -- * Lenses
    aspsState,
    aspsStateChangeReason,
  )
where

import qualified Network.AWS.EMR.Types.AutoScalingPolicyState as Types
import qualified Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status of an automatic scaling policy.
--
-- /See:/ 'mkAutoScalingPolicyStatus' smart constructor.
data AutoScalingPolicyStatus = AutoScalingPolicyStatus'
  { -- | Indicates the status of the automatic scaling policy.
    state :: Core.Maybe Types.AutoScalingPolicyState,
    -- | The reason for a change in status.
    stateChangeReason :: Core.Maybe Types.AutoScalingPolicyStateChangeReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingPolicyStatus' value with any optional fields omitted.
mkAutoScalingPolicyStatus ::
  AutoScalingPolicyStatus
mkAutoScalingPolicyStatus =
  AutoScalingPolicyStatus'
    { state = Core.Nothing,
      stateChangeReason = Core.Nothing
    }

-- | Indicates the status of the automatic scaling policy.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspsState :: Lens.Lens' AutoScalingPolicyStatus (Core.Maybe Types.AutoScalingPolicyState)
aspsState = Lens.field @"state"
{-# DEPRECATED aspsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The reason for a change in status.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspsStateChangeReason :: Lens.Lens' AutoScalingPolicyStatus (Core.Maybe Types.AutoScalingPolicyStateChangeReason)
aspsStateChangeReason = Lens.field @"stateChangeReason"
{-# DEPRECATED aspsStateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead." #-}

instance Core.FromJSON AutoScalingPolicyStatus where
  parseJSON =
    Core.withObject "AutoScalingPolicyStatus" Core.$
      \x ->
        AutoScalingPolicyStatus'
          Core.<$> (x Core..:? "State") Core.<*> (x Core..:? "StateChangeReason")
