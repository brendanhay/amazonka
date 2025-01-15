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
-- Module      : Amazonka.Connect.Types.ParticipantTimerValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ParticipantTimerValue where

import Amazonka.Connect.Types.ParticipantTimerAction
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The value of the timer. Either the timer action (@Unset@ to delete the
-- timer), or the duration of the timer in minutes. Only one value can be
-- set.
--
-- For more information about how chat timeouts work, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/setup-chat-timeouts.html Set up chat timeouts for human participants>.
--
-- /See:/ 'newParticipantTimerValue' smart constructor.
data ParticipantTimerValue = ParticipantTimerValue'
  { -- | The timer action. Currently only one value is allowed: @Unset@. It
    -- deletes a timer.
    participantTimerAction :: Prelude.Maybe ParticipantTimerAction,
    -- | The duration of a timer, in minutes.
    participantTimerDurationInMinutes :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParticipantTimerValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'participantTimerAction', 'participantTimerValue_participantTimerAction' - The timer action. Currently only one value is allowed: @Unset@. It
-- deletes a timer.
--
-- 'participantTimerDurationInMinutes', 'participantTimerValue_participantTimerDurationInMinutes' - The duration of a timer, in minutes.
newParticipantTimerValue ::
  ParticipantTimerValue
newParticipantTimerValue =
  ParticipantTimerValue'
    { participantTimerAction =
        Prelude.Nothing,
      participantTimerDurationInMinutes = Prelude.Nothing
    }

-- | The timer action. Currently only one value is allowed: @Unset@. It
-- deletes a timer.
participantTimerValue_participantTimerAction :: Lens.Lens' ParticipantTimerValue (Prelude.Maybe ParticipantTimerAction)
participantTimerValue_participantTimerAction = Lens.lens (\ParticipantTimerValue' {participantTimerAction} -> participantTimerAction) (\s@ParticipantTimerValue' {} a -> s {participantTimerAction = a} :: ParticipantTimerValue)

-- | The duration of a timer, in minutes.
participantTimerValue_participantTimerDurationInMinutes :: Lens.Lens' ParticipantTimerValue (Prelude.Maybe Prelude.Natural)
participantTimerValue_participantTimerDurationInMinutes = Lens.lens (\ParticipantTimerValue' {participantTimerDurationInMinutes} -> participantTimerDurationInMinutes) (\s@ParticipantTimerValue' {} a -> s {participantTimerDurationInMinutes = a} :: ParticipantTimerValue)

instance Prelude.Hashable ParticipantTimerValue where
  hashWithSalt _salt ParticipantTimerValue' {..} =
    _salt
      `Prelude.hashWithSalt` participantTimerAction
      `Prelude.hashWithSalt` participantTimerDurationInMinutes

instance Prelude.NFData ParticipantTimerValue where
  rnf ParticipantTimerValue' {..} =
    Prelude.rnf participantTimerAction `Prelude.seq`
      Prelude.rnf participantTimerDurationInMinutes

instance Data.ToJSON ParticipantTimerValue where
  toJSON ParticipantTimerValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ParticipantTimerAction" Data..=)
              Prelude.<$> participantTimerAction,
            ("ParticipantTimerDurationInMinutes" Data..=)
              Prelude.<$> participantTimerDurationInMinutes
          ]
      )
