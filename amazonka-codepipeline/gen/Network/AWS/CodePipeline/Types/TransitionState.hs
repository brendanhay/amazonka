{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.TransitionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.TransitionState where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents information about the state of transitions between one stage
-- and another stage.
--
-- /See:/ 'newTransitionState' smart constructor.
data TransitionState = TransitionState'
  { -- | The ID of the user who last changed the transition state.
    lastChangedBy :: Core.Maybe Core.Text,
    -- | Whether the transition between stages is enabled (true) or disabled
    -- (false).
    enabled :: Core.Maybe Core.Bool,
    -- | The user-specified reason why the transition between two stages of a
    -- pipeline was disabled.
    disabledReason :: Core.Maybe Core.Text,
    -- | The timestamp when the transition state was last changed.
    lastChangedAt :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitionState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastChangedBy', 'transitionState_lastChangedBy' - The ID of the user who last changed the transition state.
--
-- 'enabled', 'transitionState_enabled' - Whether the transition between stages is enabled (true) or disabled
-- (false).
--
-- 'disabledReason', 'transitionState_disabledReason' - The user-specified reason why the transition between two stages of a
-- pipeline was disabled.
--
-- 'lastChangedAt', 'transitionState_lastChangedAt' - The timestamp when the transition state was last changed.
newTransitionState ::
  TransitionState
newTransitionState =
  TransitionState'
    { lastChangedBy = Core.Nothing,
      enabled = Core.Nothing,
      disabledReason = Core.Nothing,
      lastChangedAt = Core.Nothing
    }

-- | The ID of the user who last changed the transition state.
transitionState_lastChangedBy :: Lens.Lens' TransitionState (Core.Maybe Core.Text)
transitionState_lastChangedBy = Lens.lens (\TransitionState' {lastChangedBy} -> lastChangedBy) (\s@TransitionState' {} a -> s {lastChangedBy = a} :: TransitionState)

-- | Whether the transition between stages is enabled (true) or disabled
-- (false).
transitionState_enabled :: Lens.Lens' TransitionState (Core.Maybe Core.Bool)
transitionState_enabled = Lens.lens (\TransitionState' {enabled} -> enabled) (\s@TransitionState' {} a -> s {enabled = a} :: TransitionState)

-- | The user-specified reason why the transition between two stages of a
-- pipeline was disabled.
transitionState_disabledReason :: Lens.Lens' TransitionState (Core.Maybe Core.Text)
transitionState_disabledReason = Lens.lens (\TransitionState' {disabledReason} -> disabledReason) (\s@TransitionState' {} a -> s {disabledReason = a} :: TransitionState)

-- | The timestamp when the transition state was last changed.
transitionState_lastChangedAt :: Lens.Lens' TransitionState (Core.Maybe Core.UTCTime)
transitionState_lastChangedAt = Lens.lens (\TransitionState' {lastChangedAt} -> lastChangedAt) (\s@TransitionState' {} a -> s {lastChangedAt = a} :: TransitionState) Core.. Lens.mapping Core._Time

instance Core.FromJSON TransitionState where
  parseJSON =
    Core.withObject
      "TransitionState"
      ( \x ->
          TransitionState'
            Core.<$> (x Core..:? "lastChangedBy")
            Core.<*> (x Core..:? "enabled")
            Core.<*> (x Core..:? "disabledReason")
            Core.<*> (x Core..:? "lastChangedAt")
      )

instance Core.Hashable TransitionState

instance Core.NFData TransitionState
