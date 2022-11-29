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
-- Module      : Amazonka.VoiceId.Types.FraudDetectionResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.FraudDetectionResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.FraudDetectionConfiguration
import Amazonka.VoiceId.Types.FraudDetectionDecision
import Amazonka.VoiceId.Types.FraudDetectionReason
import Amazonka.VoiceId.Types.FraudRiskDetails

-- | The fraud detection result produced by Voice ID, processed against the
-- current session state and streamed audio of the speaker.
--
-- /See:/ 'newFraudDetectionResult' smart constructor.
data FraudDetectionResult = FraudDetectionResult'
  { -- | A timestamp indicating when audio aggregation ended for this fraud
    -- detection result.
    audioAggregationEndedAt :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier for this fraud detection result. Given there can
    -- be multiple fraud detections for a given session, this field helps in
    -- identifying if the returned result is from previous streaming activity
    -- or a new result. Note that in the absence of any new streaming activity
    -- or risk threshold changes, Voice ID always returns cached Fraud
    -- Detection result for this API.
    fraudDetectionResultId :: Prelude.Maybe Prelude.Text,
    -- | Details about each risk analyzed for this speaker. Currently, this
    -- contains KnownFraudsterRisk and VoiceSpoofingRisk details.
    riskDetails :: Prelude.Maybe FraudRiskDetails,
    -- | The @FraudDetectionConfiguration@ used to generate this fraud detection
    -- result.
    configuration :: Prelude.Maybe FraudDetectionConfiguration,
    -- | The fraud detection decision produced by Voice ID, processed against the
    -- current session state and streamed audio of the speaker.
    decision :: Prelude.Maybe FraudDetectionDecision,
    -- | A timestamp indicating when audio aggregation started for this fraud
    -- detection result.
    audioAggregationStartedAt :: Prelude.Maybe Core.POSIX,
    -- | The reason speaker was flagged by the fraud detection system. This is
    -- only be populated if fraud detection Decision is @HIGH_RISK@, and the
    -- following possible values: @KNOWN_FRAUDSTER@ and @VOICE_SPOOFING@.
    reasons :: Prelude.Maybe [FraudDetectionReason]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FraudDetectionResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioAggregationEndedAt', 'fraudDetectionResult_audioAggregationEndedAt' - A timestamp indicating when audio aggregation ended for this fraud
-- detection result.
--
-- 'fraudDetectionResultId', 'fraudDetectionResult_fraudDetectionResultId' - The unique identifier for this fraud detection result. Given there can
-- be multiple fraud detections for a given session, this field helps in
-- identifying if the returned result is from previous streaming activity
-- or a new result. Note that in the absence of any new streaming activity
-- or risk threshold changes, Voice ID always returns cached Fraud
-- Detection result for this API.
--
-- 'riskDetails', 'fraudDetectionResult_riskDetails' - Details about each risk analyzed for this speaker. Currently, this
-- contains KnownFraudsterRisk and VoiceSpoofingRisk details.
--
-- 'configuration', 'fraudDetectionResult_configuration' - The @FraudDetectionConfiguration@ used to generate this fraud detection
-- result.
--
-- 'decision', 'fraudDetectionResult_decision' - The fraud detection decision produced by Voice ID, processed against the
-- current session state and streamed audio of the speaker.
--
-- 'audioAggregationStartedAt', 'fraudDetectionResult_audioAggregationStartedAt' - A timestamp indicating when audio aggregation started for this fraud
-- detection result.
--
-- 'reasons', 'fraudDetectionResult_reasons' - The reason speaker was flagged by the fraud detection system. This is
-- only be populated if fraud detection Decision is @HIGH_RISK@, and the
-- following possible values: @KNOWN_FRAUDSTER@ and @VOICE_SPOOFING@.
newFraudDetectionResult ::
  FraudDetectionResult
newFraudDetectionResult =
  FraudDetectionResult'
    { audioAggregationEndedAt =
        Prelude.Nothing,
      fraudDetectionResultId = Prelude.Nothing,
      riskDetails = Prelude.Nothing,
      configuration = Prelude.Nothing,
      decision = Prelude.Nothing,
      audioAggregationStartedAt = Prelude.Nothing,
      reasons = Prelude.Nothing
    }

-- | A timestamp indicating when audio aggregation ended for this fraud
-- detection result.
fraudDetectionResult_audioAggregationEndedAt :: Lens.Lens' FraudDetectionResult (Prelude.Maybe Prelude.UTCTime)
fraudDetectionResult_audioAggregationEndedAt = Lens.lens (\FraudDetectionResult' {audioAggregationEndedAt} -> audioAggregationEndedAt) (\s@FraudDetectionResult' {} a -> s {audioAggregationEndedAt = a} :: FraudDetectionResult) Prelude.. Lens.mapping Core._Time

-- | The unique identifier for this fraud detection result. Given there can
-- be multiple fraud detections for a given session, this field helps in
-- identifying if the returned result is from previous streaming activity
-- or a new result. Note that in the absence of any new streaming activity
-- or risk threshold changes, Voice ID always returns cached Fraud
-- Detection result for this API.
fraudDetectionResult_fraudDetectionResultId :: Lens.Lens' FraudDetectionResult (Prelude.Maybe Prelude.Text)
fraudDetectionResult_fraudDetectionResultId = Lens.lens (\FraudDetectionResult' {fraudDetectionResultId} -> fraudDetectionResultId) (\s@FraudDetectionResult' {} a -> s {fraudDetectionResultId = a} :: FraudDetectionResult)

-- | Details about each risk analyzed for this speaker. Currently, this
-- contains KnownFraudsterRisk and VoiceSpoofingRisk details.
fraudDetectionResult_riskDetails :: Lens.Lens' FraudDetectionResult (Prelude.Maybe FraudRiskDetails)
fraudDetectionResult_riskDetails = Lens.lens (\FraudDetectionResult' {riskDetails} -> riskDetails) (\s@FraudDetectionResult' {} a -> s {riskDetails = a} :: FraudDetectionResult)

-- | The @FraudDetectionConfiguration@ used to generate this fraud detection
-- result.
fraudDetectionResult_configuration :: Lens.Lens' FraudDetectionResult (Prelude.Maybe FraudDetectionConfiguration)
fraudDetectionResult_configuration = Lens.lens (\FraudDetectionResult' {configuration} -> configuration) (\s@FraudDetectionResult' {} a -> s {configuration = a} :: FraudDetectionResult)

-- | The fraud detection decision produced by Voice ID, processed against the
-- current session state and streamed audio of the speaker.
fraudDetectionResult_decision :: Lens.Lens' FraudDetectionResult (Prelude.Maybe FraudDetectionDecision)
fraudDetectionResult_decision = Lens.lens (\FraudDetectionResult' {decision} -> decision) (\s@FraudDetectionResult' {} a -> s {decision = a} :: FraudDetectionResult)

-- | A timestamp indicating when audio aggregation started for this fraud
-- detection result.
fraudDetectionResult_audioAggregationStartedAt :: Lens.Lens' FraudDetectionResult (Prelude.Maybe Prelude.UTCTime)
fraudDetectionResult_audioAggregationStartedAt = Lens.lens (\FraudDetectionResult' {audioAggregationStartedAt} -> audioAggregationStartedAt) (\s@FraudDetectionResult' {} a -> s {audioAggregationStartedAt = a} :: FraudDetectionResult) Prelude.. Lens.mapping Core._Time

-- | The reason speaker was flagged by the fraud detection system. This is
-- only be populated if fraud detection Decision is @HIGH_RISK@, and the
-- following possible values: @KNOWN_FRAUDSTER@ and @VOICE_SPOOFING@.
fraudDetectionResult_reasons :: Lens.Lens' FraudDetectionResult (Prelude.Maybe [FraudDetectionReason])
fraudDetectionResult_reasons = Lens.lens (\FraudDetectionResult' {reasons} -> reasons) (\s@FraudDetectionResult' {} a -> s {reasons = a} :: FraudDetectionResult) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON FraudDetectionResult where
  parseJSON =
    Core.withObject
      "FraudDetectionResult"
      ( \x ->
          FraudDetectionResult'
            Prelude.<$> (x Core..:? "AudioAggregationEndedAt")
            Prelude.<*> (x Core..:? "FraudDetectionResultId")
            Prelude.<*> (x Core..:? "RiskDetails")
            Prelude.<*> (x Core..:? "Configuration")
            Prelude.<*> (x Core..:? "Decision")
            Prelude.<*> (x Core..:? "AudioAggregationStartedAt")
            Prelude.<*> (x Core..:? "Reasons" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable FraudDetectionResult where
  hashWithSalt _salt FraudDetectionResult' {..} =
    _salt
      `Prelude.hashWithSalt` audioAggregationEndedAt
      `Prelude.hashWithSalt` fraudDetectionResultId
      `Prelude.hashWithSalt` riskDetails
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` decision
      `Prelude.hashWithSalt` audioAggregationStartedAt
      `Prelude.hashWithSalt` reasons

instance Prelude.NFData FraudDetectionResult where
  rnf FraudDetectionResult' {..} =
    Prelude.rnf audioAggregationEndedAt
      `Prelude.seq` Prelude.rnf fraudDetectionResultId
      `Prelude.seq` Prelude.rnf riskDetails
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf decision
      `Prelude.seq` Prelude.rnf audioAggregationStartedAt
      `Prelude.seq` Prelude.rnf reasons
