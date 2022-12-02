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
-- Module      : Amazonka.VoiceId.Types.AuthenticationResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.AuthenticationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.AuthenticationConfiguration
import Amazonka.VoiceId.Types.AuthenticationDecision

-- | The authentication result produced by Voice ID, processed against the
-- current session state and streamed audio of the speaker.
--
-- /See:/ 'newAuthenticationResult' smart constructor.
data AuthenticationResult = AuthenticationResult'
  { -- | A timestamp indicating when audio aggregation ended for this
    -- authentication result.
    audioAggregationEndedAt :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier for this authentication result. Because there can
    -- be multiple authentications for a given session, this field helps to
    -- identify if the returned result is from a previous streaming activity or
    -- a new result. Note that in absence of any new streaming activity,
    -- @AcceptanceThreshold@ changes, or @SpeakerId@ changes, Voice ID always
    -- returns cached Authentication Result for this API.
    authenticationResultId :: Prelude.Maybe Prelude.Text,
    -- | The authentication score for the speaker whose authentication result is
    -- produced. This value is only present if the authentication decision is
    -- either @ACCEPT@ or @REJECT@.
    score :: Prelude.Maybe Prelude.Natural,
    -- | The @AuthenticationConfiguration@ used to generate this authentication
    -- result.
    configuration :: Prelude.Maybe AuthenticationConfiguration,
    -- | The authentication decision produced by Voice ID, processed against the
    -- current session state and streamed audio of the speaker.
    decision :: Prelude.Maybe AuthenticationDecision,
    -- | The client-provided identifier for the speaker whose authentication
    -- result is produced. Only present if a @SpeakerId@ is provided for the
    -- session.
    customerSpeakerId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The service-generated identifier for the speaker whose authentication
    -- result is produced.
    generatedSpeakerId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp indicating when audio aggregation started for this
    -- authentication result.
    audioAggregationStartedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthenticationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioAggregationEndedAt', 'authenticationResult_audioAggregationEndedAt' - A timestamp indicating when audio aggregation ended for this
-- authentication result.
--
-- 'authenticationResultId', 'authenticationResult_authenticationResultId' - The unique identifier for this authentication result. Because there can
-- be multiple authentications for a given session, this field helps to
-- identify if the returned result is from a previous streaming activity or
-- a new result. Note that in absence of any new streaming activity,
-- @AcceptanceThreshold@ changes, or @SpeakerId@ changes, Voice ID always
-- returns cached Authentication Result for this API.
--
-- 'score', 'authenticationResult_score' - The authentication score for the speaker whose authentication result is
-- produced. This value is only present if the authentication decision is
-- either @ACCEPT@ or @REJECT@.
--
-- 'configuration', 'authenticationResult_configuration' - The @AuthenticationConfiguration@ used to generate this authentication
-- result.
--
-- 'decision', 'authenticationResult_decision' - The authentication decision produced by Voice ID, processed against the
-- current session state and streamed audio of the speaker.
--
-- 'customerSpeakerId', 'authenticationResult_customerSpeakerId' - The client-provided identifier for the speaker whose authentication
-- result is produced. Only present if a @SpeakerId@ is provided for the
-- session.
--
-- 'generatedSpeakerId', 'authenticationResult_generatedSpeakerId' - The service-generated identifier for the speaker whose authentication
-- result is produced.
--
-- 'audioAggregationStartedAt', 'authenticationResult_audioAggregationStartedAt' - A timestamp indicating when audio aggregation started for this
-- authentication result.
newAuthenticationResult ::
  AuthenticationResult
newAuthenticationResult =
  AuthenticationResult'
    { audioAggregationEndedAt =
        Prelude.Nothing,
      authenticationResultId = Prelude.Nothing,
      score = Prelude.Nothing,
      configuration = Prelude.Nothing,
      decision = Prelude.Nothing,
      customerSpeakerId = Prelude.Nothing,
      generatedSpeakerId = Prelude.Nothing,
      audioAggregationStartedAt = Prelude.Nothing
    }

-- | A timestamp indicating when audio aggregation ended for this
-- authentication result.
authenticationResult_audioAggregationEndedAt :: Lens.Lens' AuthenticationResult (Prelude.Maybe Prelude.UTCTime)
authenticationResult_audioAggregationEndedAt = Lens.lens (\AuthenticationResult' {audioAggregationEndedAt} -> audioAggregationEndedAt) (\s@AuthenticationResult' {} a -> s {audioAggregationEndedAt = a} :: AuthenticationResult) Prelude.. Lens.mapping Data._Time

-- | The unique identifier for this authentication result. Because there can
-- be multiple authentications for a given session, this field helps to
-- identify if the returned result is from a previous streaming activity or
-- a new result. Note that in absence of any new streaming activity,
-- @AcceptanceThreshold@ changes, or @SpeakerId@ changes, Voice ID always
-- returns cached Authentication Result for this API.
authenticationResult_authenticationResultId :: Lens.Lens' AuthenticationResult (Prelude.Maybe Prelude.Text)
authenticationResult_authenticationResultId = Lens.lens (\AuthenticationResult' {authenticationResultId} -> authenticationResultId) (\s@AuthenticationResult' {} a -> s {authenticationResultId = a} :: AuthenticationResult)

-- | The authentication score for the speaker whose authentication result is
-- produced. This value is only present if the authentication decision is
-- either @ACCEPT@ or @REJECT@.
authenticationResult_score :: Lens.Lens' AuthenticationResult (Prelude.Maybe Prelude.Natural)
authenticationResult_score = Lens.lens (\AuthenticationResult' {score} -> score) (\s@AuthenticationResult' {} a -> s {score = a} :: AuthenticationResult)

-- | The @AuthenticationConfiguration@ used to generate this authentication
-- result.
authenticationResult_configuration :: Lens.Lens' AuthenticationResult (Prelude.Maybe AuthenticationConfiguration)
authenticationResult_configuration = Lens.lens (\AuthenticationResult' {configuration} -> configuration) (\s@AuthenticationResult' {} a -> s {configuration = a} :: AuthenticationResult)

-- | The authentication decision produced by Voice ID, processed against the
-- current session state and streamed audio of the speaker.
authenticationResult_decision :: Lens.Lens' AuthenticationResult (Prelude.Maybe AuthenticationDecision)
authenticationResult_decision = Lens.lens (\AuthenticationResult' {decision} -> decision) (\s@AuthenticationResult' {} a -> s {decision = a} :: AuthenticationResult)

-- | The client-provided identifier for the speaker whose authentication
-- result is produced. Only present if a @SpeakerId@ is provided for the
-- session.
authenticationResult_customerSpeakerId :: Lens.Lens' AuthenticationResult (Prelude.Maybe Prelude.Text)
authenticationResult_customerSpeakerId = Lens.lens (\AuthenticationResult' {customerSpeakerId} -> customerSpeakerId) (\s@AuthenticationResult' {} a -> s {customerSpeakerId = a} :: AuthenticationResult) Prelude.. Lens.mapping Data._Sensitive

-- | The service-generated identifier for the speaker whose authentication
-- result is produced.
authenticationResult_generatedSpeakerId :: Lens.Lens' AuthenticationResult (Prelude.Maybe Prelude.Text)
authenticationResult_generatedSpeakerId = Lens.lens (\AuthenticationResult' {generatedSpeakerId} -> generatedSpeakerId) (\s@AuthenticationResult' {} a -> s {generatedSpeakerId = a} :: AuthenticationResult)

-- | A timestamp indicating when audio aggregation started for this
-- authentication result.
authenticationResult_audioAggregationStartedAt :: Lens.Lens' AuthenticationResult (Prelude.Maybe Prelude.UTCTime)
authenticationResult_audioAggregationStartedAt = Lens.lens (\AuthenticationResult' {audioAggregationStartedAt} -> audioAggregationStartedAt) (\s@AuthenticationResult' {} a -> s {audioAggregationStartedAt = a} :: AuthenticationResult) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON AuthenticationResult where
  parseJSON =
    Data.withObject
      "AuthenticationResult"
      ( \x ->
          AuthenticationResult'
            Prelude.<$> (x Data..:? "AudioAggregationEndedAt")
            Prelude.<*> (x Data..:? "AuthenticationResultId")
            Prelude.<*> (x Data..:? "Score")
            Prelude.<*> (x Data..:? "Configuration")
            Prelude.<*> (x Data..:? "Decision")
            Prelude.<*> (x Data..:? "CustomerSpeakerId")
            Prelude.<*> (x Data..:? "GeneratedSpeakerId")
            Prelude.<*> (x Data..:? "AudioAggregationStartedAt")
      )

instance Prelude.Hashable AuthenticationResult where
  hashWithSalt _salt AuthenticationResult' {..} =
    _salt
      `Prelude.hashWithSalt` audioAggregationEndedAt
      `Prelude.hashWithSalt` authenticationResultId
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` decision
      `Prelude.hashWithSalt` customerSpeakerId
      `Prelude.hashWithSalt` generatedSpeakerId
      `Prelude.hashWithSalt` audioAggregationStartedAt

instance Prelude.NFData AuthenticationResult where
  rnf AuthenticationResult' {..} =
    Prelude.rnf audioAggregationEndedAt
      `Prelude.seq` Prelude.rnf authenticationResultId
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf decision
      `Prelude.seq` Prelude.rnf customerSpeakerId
      `Prelude.seq` Prelude.rnf generatedSpeakerId
      `Prelude.seq` Prelude.rnf audioAggregationStartedAt
