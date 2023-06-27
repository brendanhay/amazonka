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
-- Module      : Amazonka.IVSRealtime.Types.Participant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSRealtime.Types.Participant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types.ParticipantState
import qualified Amazonka.Prelude as Prelude

-- | Object describing a participant that has joined a stage.
--
-- /See:/ 'newParticipant' smart constructor.
data Participant = Participant'
  { -- | Application-provided attributes to encode into the token and attach to a
    -- stage. Map keys and values can contain UTF-8 encoded text. The maximum
    -- length of this field is 1 KB total. /This field is exposed to all stage
    -- participants and should not be used for personally identifying,
    -- confidential, or sensitive information/.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | ISO 8601 timestamp (returned as a string) when the participant first
    -- joined the stage session.
    firstJoinTime :: Prelude.Maybe Data.ISO8601,
    -- | Unique identifier for this participant, assigned by IVS.
    participantId :: Prelude.Maybe Prelude.Text,
    -- | Whether the participant ever published to the stage session.
    published :: Prelude.Maybe Prelude.Bool,
    -- | Whether the participant is connected to or disconnected from the stage.
    state :: Prelude.Maybe ParticipantState,
    -- | Customer-assigned name to help identify the token; this can be used to
    -- link a participant to a user in the customer’s own systems. This can be
    -- any UTF-8 encoded text. /This field is exposed to all stage participants
    -- and should not be used for personally identifying, confidential, or
    -- sensitive information/.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Participant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'participant_attributes' - Application-provided attributes to encode into the token and attach to a
-- stage. Map keys and values can contain UTF-8 encoded text. The maximum
-- length of this field is 1 KB total. /This field is exposed to all stage
-- participants and should not be used for personally identifying,
-- confidential, or sensitive information/.
--
-- 'firstJoinTime', 'participant_firstJoinTime' - ISO 8601 timestamp (returned as a string) when the participant first
-- joined the stage session.
--
-- 'participantId', 'participant_participantId' - Unique identifier for this participant, assigned by IVS.
--
-- 'published', 'participant_published' - Whether the participant ever published to the stage session.
--
-- 'state', 'participant_state' - Whether the participant is connected to or disconnected from the stage.
--
-- 'userId', 'participant_userId' - Customer-assigned name to help identify the token; this can be used to
-- link a participant to a user in the customer’s own systems. This can be
-- any UTF-8 encoded text. /This field is exposed to all stage participants
-- and should not be used for personally identifying, confidential, or
-- sensitive information/.
newParticipant ::
  Participant
newParticipant =
  Participant'
    { attributes = Prelude.Nothing,
      firstJoinTime = Prelude.Nothing,
      participantId = Prelude.Nothing,
      published = Prelude.Nothing,
      state = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | Application-provided attributes to encode into the token and attach to a
-- stage. Map keys and values can contain UTF-8 encoded text. The maximum
-- length of this field is 1 KB total. /This field is exposed to all stage
-- participants and should not be used for personally identifying,
-- confidential, or sensitive information/.
participant_attributes :: Lens.Lens' Participant (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
participant_attributes = Lens.lens (\Participant' {attributes} -> attributes) (\s@Participant' {} a -> s {attributes = a} :: Participant) Prelude.. Lens.mapping Lens.coerced

-- | ISO 8601 timestamp (returned as a string) when the participant first
-- joined the stage session.
participant_firstJoinTime :: Lens.Lens' Participant (Prelude.Maybe Prelude.UTCTime)
participant_firstJoinTime = Lens.lens (\Participant' {firstJoinTime} -> firstJoinTime) (\s@Participant' {} a -> s {firstJoinTime = a} :: Participant) Prelude.. Lens.mapping Data._Time

-- | Unique identifier for this participant, assigned by IVS.
participant_participantId :: Lens.Lens' Participant (Prelude.Maybe Prelude.Text)
participant_participantId = Lens.lens (\Participant' {participantId} -> participantId) (\s@Participant' {} a -> s {participantId = a} :: Participant)

-- | Whether the participant ever published to the stage session.
participant_published :: Lens.Lens' Participant (Prelude.Maybe Prelude.Bool)
participant_published = Lens.lens (\Participant' {published} -> published) (\s@Participant' {} a -> s {published = a} :: Participant)

-- | Whether the participant is connected to or disconnected from the stage.
participant_state :: Lens.Lens' Participant (Prelude.Maybe ParticipantState)
participant_state = Lens.lens (\Participant' {state} -> state) (\s@Participant' {} a -> s {state = a} :: Participant)

-- | Customer-assigned name to help identify the token; this can be used to
-- link a participant to a user in the customer’s own systems. This can be
-- any UTF-8 encoded text. /This field is exposed to all stage participants
-- and should not be used for personally identifying, confidential, or
-- sensitive information/.
participant_userId :: Lens.Lens' Participant (Prelude.Maybe Prelude.Text)
participant_userId = Lens.lens (\Participant' {userId} -> userId) (\s@Participant' {} a -> s {userId = a} :: Participant)

instance Data.FromJSON Participant where
  parseJSON =
    Data.withObject
      "Participant"
      ( \x ->
          Participant'
            Prelude.<$> (x Data..:? "attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "firstJoinTime")
            Prelude.<*> (x Data..:? "participantId")
            Prelude.<*> (x Data..:? "published")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "userId")
      )

instance Prelude.Hashable Participant where
  hashWithSalt _salt Participant' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` firstJoinTime
      `Prelude.hashWithSalt` participantId
      `Prelude.hashWithSalt` published
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` userId

instance Prelude.NFData Participant where
  rnf Participant' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf firstJoinTime
      `Prelude.seq` Prelude.rnf participantId
      `Prelude.seq` Prelude.rnf published
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf userId
