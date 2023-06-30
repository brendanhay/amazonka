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
-- Module      : Amazonka.Connect.Types.ParticipantTimerConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ParticipantTimerConfiguration where

import Amazonka.Connect.Types.ParticipantTimerType
import Amazonka.Connect.Types.ParticipantTimerValue
import Amazonka.Connect.Types.TimerEligibleParticipantRoles
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for the timer. After the timer configuration
-- is set, it persists for the duration of the chat. It persists across new
-- contacts in the chain, for example, transfer contacts.
--
-- For more information about how chat timeouts work, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/setup-chat-timeouts.html Set up chat timeouts for human participants>.
--
-- /See:/ 'newParticipantTimerConfiguration' smart constructor.
data ParticipantTimerConfiguration = ParticipantTimerConfiguration'
  { -- | The role of the participant in the chat conversation.
    participantRole :: TimerEligibleParticipantRoles,
    -- | The type of timer. @IDLE@ indicates the timer applies for considering a
    -- human chat participant as idle. @DISCONNECT_NONCUSTOMER@ indicates the
    -- timer applies to automatically disconnecting a chat participant due to
    -- idleness.
    timerType :: ParticipantTimerType,
    -- | The value of the timer. Either the timer action (Unset to delete the
    -- timer), or the duration of the timer in minutes. Only one value can be
    -- set.
    timerValue :: ParticipantTimerValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParticipantTimerConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'participantRole', 'participantTimerConfiguration_participantRole' - The role of the participant in the chat conversation.
--
-- 'timerType', 'participantTimerConfiguration_timerType' - The type of timer. @IDLE@ indicates the timer applies for considering a
-- human chat participant as idle. @DISCONNECT_NONCUSTOMER@ indicates the
-- timer applies to automatically disconnecting a chat participant due to
-- idleness.
--
-- 'timerValue', 'participantTimerConfiguration_timerValue' - The value of the timer. Either the timer action (Unset to delete the
-- timer), or the duration of the timer in minutes. Only one value can be
-- set.
newParticipantTimerConfiguration ::
  -- | 'participantRole'
  TimerEligibleParticipantRoles ->
  -- | 'timerType'
  ParticipantTimerType ->
  -- | 'timerValue'
  ParticipantTimerValue ->
  ParticipantTimerConfiguration
newParticipantTimerConfiguration
  pParticipantRole_
  pTimerType_
  pTimerValue_ =
    ParticipantTimerConfiguration'
      { participantRole =
          pParticipantRole_,
        timerType = pTimerType_,
        timerValue = pTimerValue_
      }

-- | The role of the participant in the chat conversation.
participantTimerConfiguration_participantRole :: Lens.Lens' ParticipantTimerConfiguration TimerEligibleParticipantRoles
participantTimerConfiguration_participantRole = Lens.lens (\ParticipantTimerConfiguration' {participantRole} -> participantRole) (\s@ParticipantTimerConfiguration' {} a -> s {participantRole = a} :: ParticipantTimerConfiguration)

-- | The type of timer. @IDLE@ indicates the timer applies for considering a
-- human chat participant as idle. @DISCONNECT_NONCUSTOMER@ indicates the
-- timer applies to automatically disconnecting a chat participant due to
-- idleness.
participantTimerConfiguration_timerType :: Lens.Lens' ParticipantTimerConfiguration ParticipantTimerType
participantTimerConfiguration_timerType = Lens.lens (\ParticipantTimerConfiguration' {timerType} -> timerType) (\s@ParticipantTimerConfiguration' {} a -> s {timerType = a} :: ParticipantTimerConfiguration)

-- | The value of the timer. Either the timer action (Unset to delete the
-- timer), or the duration of the timer in minutes. Only one value can be
-- set.
participantTimerConfiguration_timerValue :: Lens.Lens' ParticipantTimerConfiguration ParticipantTimerValue
participantTimerConfiguration_timerValue = Lens.lens (\ParticipantTimerConfiguration' {timerValue} -> timerValue) (\s@ParticipantTimerConfiguration' {} a -> s {timerValue = a} :: ParticipantTimerConfiguration)

instance
  Prelude.Hashable
    ParticipantTimerConfiguration
  where
  hashWithSalt _salt ParticipantTimerConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` participantRole
      `Prelude.hashWithSalt` timerType
      `Prelude.hashWithSalt` timerValue

instance Prelude.NFData ParticipantTimerConfiguration where
  rnf ParticipantTimerConfiguration' {..} =
    Prelude.rnf participantRole
      `Prelude.seq` Prelude.rnf timerType
      `Prelude.seq` Prelude.rnf timerValue

instance Data.ToJSON ParticipantTimerConfiguration where
  toJSON ParticipantTimerConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ParticipantRole" Data..= participantRole),
            Prelude.Just ("TimerType" Data..= timerType),
            Prelude.Just ("TimerValue" Data..= timerValue)
          ]
      )
