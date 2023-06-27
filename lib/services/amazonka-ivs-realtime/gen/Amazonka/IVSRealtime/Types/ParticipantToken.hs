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
-- Module      : Amazonka.IVSRealtime.Types.ParticipantToken
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSRealtime.Types.ParticipantToken where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types.ParticipantTokenCapability
import qualified Amazonka.Prelude as Prelude

-- | Object specifying a participant token in a stage.
--
-- /See:/ 'newParticipantToken' smart constructor.
data ParticipantToken = ParticipantToken'
  { -- | Application-provided attributes to encode into the token and attach to a
    -- stage. /This field is exposed to all stage participants and should not
    -- be used for personally identifying, confidential, or sensitive
    -- information./
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Set of capabilities that the user is allowed to perform in the stage.
    capabilities :: Prelude.Maybe [ParticipantTokenCapability],
    -- | Duration (in minutes), after which the participant token expires.
    -- Default: 720 (12 hours).
    duration :: Prelude.Maybe Prelude.Natural,
    -- | ISO 8601 timestamp (returned as a string) for when this token expires.
    expirationTime :: Prelude.Maybe Data.ISO8601,
    -- | Unique identifier for this participant token, assigned by IVS.
    participantId :: Prelude.Maybe Prelude.Text,
    -- | The issued client token, encrypted.
    token :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Customer-assigned name to help identify the token; this can be used to
    -- link a participant to a user in the customer’s own systems. This can be
    -- any UTF-8 encoded text. /This field is exposed to all stage participants
    -- and should not be used for personally identifying, confidential, or
    -- sensitive information./
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParticipantToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'participantToken_attributes' - Application-provided attributes to encode into the token and attach to a
-- stage. /This field is exposed to all stage participants and should not
-- be used for personally identifying, confidential, or sensitive
-- information./
--
-- 'capabilities', 'participantToken_capabilities' - Set of capabilities that the user is allowed to perform in the stage.
--
-- 'duration', 'participantToken_duration' - Duration (in minutes), after which the participant token expires.
-- Default: 720 (12 hours).
--
-- 'expirationTime', 'participantToken_expirationTime' - ISO 8601 timestamp (returned as a string) for when this token expires.
--
-- 'participantId', 'participantToken_participantId' - Unique identifier for this participant token, assigned by IVS.
--
-- 'token', 'participantToken_token' - The issued client token, encrypted.
--
-- 'userId', 'participantToken_userId' - Customer-assigned name to help identify the token; this can be used to
-- link a participant to a user in the customer’s own systems. This can be
-- any UTF-8 encoded text. /This field is exposed to all stage participants
-- and should not be used for personally identifying, confidential, or
-- sensitive information./
newParticipantToken ::
  ParticipantToken
newParticipantToken =
  ParticipantToken'
    { attributes = Prelude.Nothing,
      capabilities = Prelude.Nothing,
      duration = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      participantId = Prelude.Nothing,
      token = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | Application-provided attributes to encode into the token and attach to a
-- stage. /This field is exposed to all stage participants and should not
-- be used for personally identifying, confidential, or sensitive
-- information./
participantToken_attributes :: Lens.Lens' ParticipantToken (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
participantToken_attributes = Lens.lens (\ParticipantToken' {attributes} -> attributes) (\s@ParticipantToken' {} a -> s {attributes = a} :: ParticipantToken) Prelude.. Lens.mapping Lens.coerced

-- | Set of capabilities that the user is allowed to perform in the stage.
participantToken_capabilities :: Lens.Lens' ParticipantToken (Prelude.Maybe [ParticipantTokenCapability])
participantToken_capabilities = Lens.lens (\ParticipantToken' {capabilities} -> capabilities) (\s@ParticipantToken' {} a -> s {capabilities = a} :: ParticipantToken) Prelude.. Lens.mapping Lens.coerced

-- | Duration (in minutes), after which the participant token expires.
-- Default: 720 (12 hours).
participantToken_duration :: Lens.Lens' ParticipantToken (Prelude.Maybe Prelude.Natural)
participantToken_duration = Lens.lens (\ParticipantToken' {duration} -> duration) (\s@ParticipantToken' {} a -> s {duration = a} :: ParticipantToken)

-- | ISO 8601 timestamp (returned as a string) for when this token expires.
participantToken_expirationTime :: Lens.Lens' ParticipantToken (Prelude.Maybe Prelude.UTCTime)
participantToken_expirationTime = Lens.lens (\ParticipantToken' {expirationTime} -> expirationTime) (\s@ParticipantToken' {} a -> s {expirationTime = a} :: ParticipantToken) Prelude.. Lens.mapping Data._Time

-- | Unique identifier for this participant token, assigned by IVS.
participantToken_participantId :: Lens.Lens' ParticipantToken (Prelude.Maybe Prelude.Text)
participantToken_participantId = Lens.lens (\ParticipantToken' {participantId} -> participantId) (\s@ParticipantToken' {} a -> s {participantId = a} :: ParticipantToken)

-- | The issued client token, encrypted.
participantToken_token :: Lens.Lens' ParticipantToken (Prelude.Maybe Prelude.Text)
participantToken_token = Lens.lens (\ParticipantToken' {token} -> token) (\s@ParticipantToken' {} a -> s {token = a} :: ParticipantToken) Prelude.. Lens.mapping Data._Sensitive

-- | Customer-assigned name to help identify the token; this can be used to
-- link a participant to a user in the customer’s own systems. This can be
-- any UTF-8 encoded text. /This field is exposed to all stage participants
-- and should not be used for personally identifying, confidential, or
-- sensitive information./
participantToken_userId :: Lens.Lens' ParticipantToken (Prelude.Maybe Prelude.Text)
participantToken_userId = Lens.lens (\ParticipantToken' {userId} -> userId) (\s@ParticipantToken' {} a -> s {userId = a} :: ParticipantToken)

instance Data.FromJSON ParticipantToken where
  parseJSON =
    Data.withObject
      "ParticipantToken"
      ( \x ->
          ParticipantToken'
            Prelude.<$> (x Data..:? "attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "capabilities" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "duration")
            Prelude.<*> (x Data..:? "expirationTime")
            Prelude.<*> (x Data..:? "participantId")
            Prelude.<*> (x Data..:? "token")
            Prelude.<*> (x Data..:? "userId")
      )

instance Prelude.Hashable ParticipantToken where
  hashWithSalt _salt ParticipantToken' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` expirationTime
      `Prelude.hashWithSalt` participantId
      `Prelude.hashWithSalt` token
      `Prelude.hashWithSalt` userId

instance Prelude.NFData ParticipantToken where
  rnf ParticipantToken' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf expirationTime
      `Prelude.seq` Prelude.rnf participantId
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf userId
