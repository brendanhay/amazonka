{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about the state of transitions between one stage
-- and another stage.
--
-- /See:/ 'newTransitionState' smart constructor.
data TransitionState = TransitionState'
  { -- | The ID of the user who last changed the transition state.
    lastChangedBy :: Prelude.Maybe Prelude.Text,
    -- | Whether the transition between stages is enabled (true) or disabled
    -- (false).
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The user-specified reason why the transition between two stages of a
    -- pipeline was disabled.
    disabledReason :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the transition state was last changed.
    lastChangedAt :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { lastChangedBy = Prelude.Nothing,
      enabled = Prelude.Nothing,
      disabledReason = Prelude.Nothing,
      lastChangedAt = Prelude.Nothing
    }

-- | The ID of the user who last changed the transition state.
transitionState_lastChangedBy :: Lens.Lens' TransitionState (Prelude.Maybe Prelude.Text)
transitionState_lastChangedBy = Lens.lens (\TransitionState' {lastChangedBy} -> lastChangedBy) (\s@TransitionState' {} a -> s {lastChangedBy = a} :: TransitionState)

-- | Whether the transition between stages is enabled (true) or disabled
-- (false).
transitionState_enabled :: Lens.Lens' TransitionState (Prelude.Maybe Prelude.Bool)
transitionState_enabled = Lens.lens (\TransitionState' {enabled} -> enabled) (\s@TransitionState' {} a -> s {enabled = a} :: TransitionState)

-- | The user-specified reason why the transition between two stages of a
-- pipeline was disabled.
transitionState_disabledReason :: Lens.Lens' TransitionState (Prelude.Maybe Prelude.Text)
transitionState_disabledReason = Lens.lens (\TransitionState' {disabledReason} -> disabledReason) (\s@TransitionState' {} a -> s {disabledReason = a} :: TransitionState)

-- | The timestamp when the transition state was last changed.
transitionState_lastChangedAt :: Lens.Lens' TransitionState (Prelude.Maybe Prelude.UTCTime)
transitionState_lastChangedAt = Lens.lens (\TransitionState' {lastChangedAt} -> lastChangedAt) (\s@TransitionState' {} a -> s {lastChangedAt = a} :: TransitionState) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON TransitionState where
  parseJSON =
    Prelude.withObject
      "TransitionState"
      ( \x ->
          TransitionState'
            Prelude.<$> (x Prelude..:? "lastChangedBy")
            Prelude.<*> (x Prelude..:? "enabled")
            Prelude.<*> (x Prelude..:? "disabledReason")
            Prelude.<*> (x Prelude..:? "lastChangedAt")
      )

instance Prelude.Hashable TransitionState

instance Prelude.NFData TransitionState
