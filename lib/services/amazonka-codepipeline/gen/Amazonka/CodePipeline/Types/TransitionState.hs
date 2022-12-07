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
-- Module      : Amazonka.CodePipeline.Types.TransitionState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.TransitionState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about the state of transitions between one stage
-- and another stage.
--
-- /See:/ 'newTransitionState' smart constructor.
data TransitionState = TransitionState'
  { -- | The timestamp when the transition state was last changed.
    lastChangedAt :: Prelude.Maybe Data.POSIX,
    -- | The user-specified reason why the transition between two stages of a
    -- pipeline was disabled.
    disabledReason :: Prelude.Maybe Prelude.Text,
    -- | Whether the transition between stages is enabled (true) or disabled
    -- (false).
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the user who last changed the transition state.
    lastChangedBy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitionState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastChangedAt', 'transitionState_lastChangedAt' - The timestamp when the transition state was last changed.
--
-- 'disabledReason', 'transitionState_disabledReason' - The user-specified reason why the transition between two stages of a
-- pipeline was disabled.
--
-- 'enabled', 'transitionState_enabled' - Whether the transition between stages is enabled (true) or disabled
-- (false).
--
-- 'lastChangedBy', 'transitionState_lastChangedBy' - The ID of the user who last changed the transition state.
newTransitionState ::
  TransitionState
newTransitionState =
  TransitionState'
    { lastChangedAt = Prelude.Nothing,
      disabledReason = Prelude.Nothing,
      enabled = Prelude.Nothing,
      lastChangedBy = Prelude.Nothing
    }

-- | The timestamp when the transition state was last changed.
transitionState_lastChangedAt :: Lens.Lens' TransitionState (Prelude.Maybe Prelude.UTCTime)
transitionState_lastChangedAt = Lens.lens (\TransitionState' {lastChangedAt} -> lastChangedAt) (\s@TransitionState' {} a -> s {lastChangedAt = a} :: TransitionState) Prelude.. Lens.mapping Data._Time

-- | The user-specified reason why the transition between two stages of a
-- pipeline was disabled.
transitionState_disabledReason :: Lens.Lens' TransitionState (Prelude.Maybe Prelude.Text)
transitionState_disabledReason = Lens.lens (\TransitionState' {disabledReason} -> disabledReason) (\s@TransitionState' {} a -> s {disabledReason = a} :: TransitionState)

-- | Whether the transition between stages is enabled (true) or disabled
-- (false).
transitionState_enabled :: Lens.Lens' TransitionState (Prelude.Maybe Prelude.Bool)
transitionState_enabled = Lens.lens (\TransitionState' {enabled} -> enabled) (\s@TransitionState' {} a -> s {enabled = a} :: TransitionState)

-- | The ID of the user who last changed the transition state.
transitionState_lastChangedBy :: Lens.Lens' TransitionState (Prelude.Maybe Prelude.Text)
transitionState_lastChangedBy = Lens.lens (\TransitionState' {lastChangedBy} -> lastChangedBy) (\s@TransitionState' {} a -> s {lastChangedBy = a} :: TransitionState)

instance Data.FromJSON TransitionState where
  parseJSON =
    Data.withObject
      "TransitionState"
      ( \x ->
          TransitionState'
            Prelude.<$> (x Data..:? "lastChangedAt")
            Prelude.<*> (x Data..:? "disabledReason")
            Prelude.<*> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "lastChangedBy")
      )

instance Prelude.Hashable TransitionState where
  hashWithSalt _salt TransitionState' {..} =
    _salt `Prelude.hashWithSalt` lastChangedAt
      `Prelude.hashWithSalt` disabledReason
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` lastChangedBy

instance Prelude.NFData TransitionState where
  rnf TransitionState' {..} =
    Prelude.rnf lastChangedAt
      `Prelude.seq` Prelude.rnf disabledReason
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf lastChangedBy
