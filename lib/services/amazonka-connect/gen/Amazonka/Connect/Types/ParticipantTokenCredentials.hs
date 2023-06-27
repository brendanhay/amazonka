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
-- Module      : Amazonka.Connect.Types.ParticipantTokenCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ParticipantTokenCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The credentials used by the participant.
--
-- /See:/ 'newParticipantTokenCredentials' smart constructor.
data ParticipantTokenCredentials = ParticipantTokenCredentials'
  { -- | The expiration of the token. It\'s specified in ISO 8601 format:
    -- yyyy-MM-ddThh:mm:ss.SSSZ. For example, 2019-11-08T02:41:28.172Z.
    expiry :: Prelude.Maybe Prelude.Text,
    -- | The token used by the chat participant to call
    -- <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection>.
    -- The participant token is valid for the lifetime of a chat participant.
    participantToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParticipantTokenCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiry', 'participantTokenCredentials_expiry' - The expiration of the token. It\'s specified in ISO 8601 format:
-- yyyy-MM-ddThh:mm:ss.SSSZ. For example, 2019-11-08T02:41:28.172Z.
--
-- 'participantToken', 'participantTokenCredentials_participantToken' - The token used by the chat participant to call
-- <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection>.
-- The participant token is valid for the lifetime of a chat participant.
newParticipantTokenCredentials ::
  ParticipantTokenCredentials
newParticipantTokenCredentials =
  ParticipantTokenCredentials'
    { expiry =
        Prelude.Nothing,
      participantToken = Prelude.Nothing
    }

-- | The expiration of the token. It\'s specified in ISO 8601 format:
-- yyyy-MM-ddThh:mm:ss.SSSZ. For example, 2019-11-08T02:41:28.172Z.
participantTokenCredentials_expiry :: Lens.Lens' ParticipantTokenCredentials (Prelude.Maybe Prelude.Text)
participantTokenCredentials_expiry = Lens.lens (\ParticipantTokenCredentials' {expiry} -> expiry) (\s@ParticipantTokenCredentials' {} a -> s {expiry = a} :: ParticipantTokenCredentials)

-- | The token used by the chat participant to call
-- <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html CreateParticipantConnection>.
-- The participant token is valid for the lifetime of a chat participant.
participantTokenCredentials_participantToken :: Lens.Lens' ParticipantTokenCredentials (Prelude.Maybe Prelude.Text)
participantTokenCredentials_participantToken = Lens.lens (\ParticipantTokenCredentials' {participantToken} -> participantToken) (\s@ParticipantTokenCredentials' {} a -> s {participantToken = a} :: ParticipantTokenCredentials)

instance Data.FromJSON ParticipantTokenCredentials where
  parseJSON =
    Data.withObject
      "ParticipantTokenCredentials"
      ( \x ->
          ParticipantTokenCredentials'
            Prelude.<$> (x Data..:? "Expiry")
            Prelude.<*> (x Data..:? "ParticipantToken")
      )

instance Prelude.Hashable ParticipantTokenCredentials where
  hashWithSalt _salt ParticipantTokenCredentials' {..} =
    _salt
      `Prelude.hashWithSalt` expiry
      `Prelude.hashWithSalt` participantToken

instance Prelude.NFData ParticipantTokenCredentials where
  rnf ParticipantTokenCredentials' {..} =
    Prelude.rnf expiry
      `Prelude.seq` Prelude.rnf participantToken
