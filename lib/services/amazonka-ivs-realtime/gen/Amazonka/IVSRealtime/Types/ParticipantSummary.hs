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
-- Module      : Amazonka.IVSRealtime.Types.ParticipantSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSRealtime.Types.ParticipantSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types.ParticipantState
import qualified Amazonka.Prelude as Prelude

-- | Summary object describing a participant that has joined a stage.
--
-- /See:/ 'newParticipantSummary' smart constructor.
data ParticipantSummary = ParticipantSummary'
  { -- | ISO 8601 timestamp (returned as a string) when the participant first
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
-- Create a value of 'ParticipantSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firstJoinTime', 'participantSummary_firstJoinTime' - ISO 8601 timestamp (returned as a string) when the participant first
-- joined the stage session.
--
-- 'participantId', 'participantSummary_participantId' - Unique identifier for this participant, assigned by IVS.
--
-- 'published', 'participantSummary_published' - Whether the participant ever published to the stage session.
--
-- 'state', 'participantSummary_state' - Whether the participant is connected to or disconnected from the stage.
--
-- 'userId', 'participantSummary_userId' - Customer-assigned name to help identify the token; this can be used to
-- link a participant to a user in the customer’s own systems. This can be
-- any UTF-8 encoded text. /This field is exposed to all stage participants
-- and should not be used for personally identifying, confidential, or
-- sensitive information/.
newParticipantSummary ::
  ParticipantSummary
newParticipantSummary =
  ParticipantSummary'
    { firstJoinTime =
        Prelude.Nothing,
      participantId = Prelude.Nothing,
      published = Prelude.Nothing,
      state = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | ISO 8601 timestamp (returned as a string) when the participant first
-- joined the stage session.
participantSummary_firstJoinTime :: Lens.Lens' ParticipantSummary (Prelude.Maybe Prelude.UTCTime)
participantSummary_firstJoinTime = Lens.lens (\ParticipantSummary' {firstJoinTime} -> firstJoinTime) (\s@ParticipantSummary' {} a -> s {firstJoinTime = a} :: ParticipantSummary) Prelude.. Lens.mapping Data._Time

-- | Unique identifier for this participant, assigned by IVS.
participantSummary_participantId :: Lens.Lens' ParticipantSummary (Prelude.Maybe Prelude.Text)
participantSummary_participantId = Lens.lens (\ParticipantSummary' {participantId} -> participantId) (\s@ParticipantSummary' {} a -> s {participantId = a} :: ParticipantSummary)

-- | Whether the participant ever published to the stage session.
participantSummary_published :: Lens.Lens' ParticipantSummary (Prelude.Maybe Prelude.Bool)
participantSummary_published = Lens.lens (\ParticipantSummary' {published} -> published) (\s@ParticipantSummary' {} a -> s {published = a} :: ParticipantSummary)

-- | Whether the participant is connected to or disconnected from the stage.
participantSummary_state :: Lens.Lens' ParticipantSummary (Prelude.Maybe ParticipantState)
participantSummary_state = Lens.lens (\ParticipantSummary' {state} -> state) (\s@ParticipantSummary' {} a -> s {state = a} :: ParticipantSummary)

-- | Customer-assigned name to help identify the token; this can be used to
-- link a participant to a user in the customer’s own systems. This can be
-- any UTF-8 encoded text. /This field is exposed to all stage participants
-- and should not be used for personally identifying, confidential, or
-- sensitive information/.
participantSummary_userId :: Lens.Lens' ParticipantSummary (Prelude.Maybe Prelude.Text)
participantSummary_userId = Lens.lens (\ParticipantSummary' {userId} -> userId) (\s@ParticipantSummary' {} a -> s {userId = a} :: ParticipantSummary)

instance Data.FromJSON ParticipantSummary where
  parseJSON =
    Data.withObject
      "ParticipantSummary"
      ( \x ->
          ParticipantSummary'
            Prelude.<$> (x Data..:? "firstJoinTime")
            Prelude.<*> (x Data..:? "participantId")
            Prelude.<*> (x Data..:? "published")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "userId")
      )

instance Prelude.Hashable ParticipantSummary where
  hashWithSalt _salt ParticipantSummary' {..} =
    _salt
      `Prelude.hashWithSalt` firstJoinTime
      `Prelude.hashWithSalt` participantId
      `Prelude.hashWithSalt` published
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` userId

instance Prelude.NFData ParticipantSummary where
  rnf ParticipantSummary' {..} =
    Prelude.rnf firstJoinTime
      `Prelude.seq` Prelude.rnf participantId
      `Prelude.seq` Prelude.rnf published
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf userId
