{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.TransitionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.TransitionState
  ( TransitionState (..),

    -- * Smart constructor
    mkTransitionState,

    -- * Lenses
    tsDisabledReason,
    tsEnabled,
    tsLastChangedAt,
    tsLastChangedBy,
  )
where

import qualified Network.AWS.CodePipeline.Types.DisabledReason as Types
import qualified Network.AWS.CodePipeline.Types.LastChangedBy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about the state of transitions between one stage and another stage.
--
-- /See:/ 'mkTransitionState' smart constructor.
data TransitionState = TransitionState'
  { -- | The user-specified reason why the transition between two stages of a pipeline was disabled.
    disabledReason :: Core.Maybe Types.DisabledReason,
    -- | Whether the transition between stages is enabled (true) or disabled (false).
    enabled :: Core.Maybe Core.Bool,
    -- | The timestamp when the transition state was last changed.
    lastChangedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the user who last changed the transition state.
    lastChangedBy :: Core.Maybe Types.LastChangedBy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TransitionState' value with any optional fields omitted.
mkTransitionState ::
  TransitionState
mkTransitionState =
  TransitionState'
    { disabledReason = Core.Nothing,
      enabled = Core.Nothing,
      lastChangedAt = Core.Nothing,
      lastChangedBy = Core.Nothing
    }

-- | The user-specified reason why the transition between two stages of a pipeline was disabled.
--
-- /Note:/ Consider using 'disabledReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDisabledReason :: Lens.Lens' TransitionState (Core.Maybe Types.DisabledReason)
tsDisabledReason = Lens.field @"disabledReason"
{-# DEPRECATED tsDisabledReason "Use generic-lens or generic-optics with 'disabledReason' instead." #-}

-- | Whether the transition between stages is enabled (true) or disabled (false).
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsEnabled :: Lens.Lens' TransitionState (Core.Maybe Core.Bool)
tsEnabled = Lens.field @"enabled"
{-# DEPRECATED tsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The timestamp when the transition state was last changed.
--
-- /Note:/ Consider using 'lastChangedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsLastChangedAt :: Lens.Lens' TransitionState (Core.Maybe Core.NominalDiffTime)
tsLastChangedAt = Lens.field @"lastChangedAt"
{-# DEPRECATED tsLastChangedAt "Use generic-lens or generic-optics with 'lastChangedAt' instead." #-}

-- | The ID of the user who last changed the transition state.
--
-- /Note:/ Consider using 'lastChangedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsLastChangedBy :: Lens.Lens' TransitionState (Core.Maybe Types.LastChangedBy)
tsLastChangedBy = Lens.field @"lastChangedBy"
{-# DEPRECATED tsLastChangedBy "Use generic-lens or generic-optics with 'lastChangedBy' instead." #-}

instance Core.FromJSON TransitionState where
  parseJSON =
    Core.withObject "TransitionState" Core.$
      \x ->
        TransitionState'
          Core.<$> (x Core..:? "disabledReason")
          Core.<*> (x Core..:? "enabled")
          Core.<*> (x Core..:? "lastChangedAt")
          Core.<*> (x Core..:? "lastChangedBy")
