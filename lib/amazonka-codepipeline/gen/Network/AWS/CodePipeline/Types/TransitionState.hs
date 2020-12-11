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
    tsEnabled,
    tsDisabledReason,
    tsLastChangedAt,
    tsLastChangedBy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about the state of transitions between one stage and another stage.
--
-- /See:/ 'mkTransitionState' smart constructor.
data TransitionState = TransitionState'
  { enabled ::
      Lude.Maybe Lude.Bool,
    disabledReason :: Lude.Maybe Lude.Text,
    lastChangedAt :: Lude.Maybe Lude.Timestamp,
    lastChangedBy :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitionState' with the minimum fields required to make a request.
--
-- * 'disabledReason' - The user-specified reason why the transition between two stages of a pipeline was disabled.
-- * 'enabled' - Whether the transition between stages is enabled (true) or disabled (false).
-- * 'lastChangedAt' - The timestamp when the transition state was last changed.
-- * 'lastChangedBy' - The ID of the user who last changed the transition state.
mkTransitionState ::
  TransitionState
mkTransitionState =
  TransitionState'
    { enabled = Lude.Nothing,
      disabledReason = Lude.Nothing,
      lastChangedAt = Lude.Nothing,
      lastChangedBy = Lude.Nothing
    }

-- | Whether the transition between stages is enabled (true) or disabled (false).
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsEnabled :: Lens.Lens' TransitionState (Lude.Maybe Lude.Bool)
tsEnabled = Lens.lens (enabled :: TransitionState -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: TransitionState)
{-# DEPRECATED tsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The user-specified reason why the transition between two stages of a pipeline was disabled.
--
-- /Note:/ Consider using 'disabledReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDisabledReason :: Lens.Lens' TransitionState (Lude.Maybe Lude.Text)
tsDisabledReason = Lens.lens (disabledReason :: TransitionState -> Lude.Maybe Lude.Text) (\s a -> s {disabledReason = a} :: TransitionState)
{-# DEPRECATED tsDisabledReason "Use generic-lens or generic-optics with 'disabledReason' instead." #-}

-- | The timestamp when the transition state was last changed.
--
-- /Note:/ Consider using 'lastChangedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsLastChangedAt :: Lens.Lens' TransitionState (Lude.Maybe Lude.Timestamp)
tsLastChangedAt = Lens.lens (lastChangedAt :: TransitionState -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastChangedAt = a} :: TransitionState)
{-# DEPRECATED tsLastChangedAt "Use generic-lens or generic-optics with 'lastChangedAt' instead." #-}

-- | The ID of the user who last changed the transition state.
--
-- /Note:/ Consider using 'lastChangedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsLastChangedBy :: Lens.Lens' TransitionState (Lude.Maybe Lude.Text)
tsLastChangedBy = Lens.lens (lastChangedBy :: TransitionState -> Lude.Maybe Lude.Text) (\s a -> s {lastChangedBy = a} :: TransitionState)
{-# DEPRECATED tsLastChangedBy "Use generic-lens or generic-optics with 'lastChangedBy' instead." #-}

instance Lude.FromJSON TransitionState where
  parseJSON =
    Lude.withObject
      "TransitionState"
      ( \x ->
          TransitionState'
            Lude.<$> (x Lude..:? "enabled")
            Lude.<*> (x Lude..:? "disabledReason")
            Lude.<*> (x Lude..:? "lastChangedAt")
            Lude.<*> (x Lude..:? "lastChangedBy")
      )
