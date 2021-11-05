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
-- Module      : Network.AWS.VoiceId.Types.FraudDetectionResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.VoiceId.Types.FraudDetectionResult where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.VoiceId.Types.FraudDetectionConfiguration
import Network.AWS.VoiceId.Types.FraudDetectionDecision
import Network.AWS.VoiceId.Types.FraudDetectionReason
import Network.AWS.VoiceId.Types.FraudRiskDetails

-- | The fraud detection result produced by Voice ID, processed against the
-- current session state and streamed audio of the speaker.
--
-- /See:/ 'newFraudDetectionResult' smart constructor.
data FraudDetectionResult = FraudDetectionResult'
  { -- | The reason speaker was flagged by the fraud detection system. This is
    -- only be populated if fraud detection Decision is @HIGH_RISK@, and only
    -- has one possible value: @KNOWN_FRAUDSTER@.
    reasons :: Prelude.Maybe [FraudDetectionReason],
    -- | Details about each risk analyzed for this speaker.
    riskDetails :: Prelude.Maybe FraudRiskDetails,
    -- | The unique identifier for this fraud detection result. Given there can
    -- be multiple fraud detections for a given session, this field helps in
    -- identifying if the returned result is from previous streaming activity
    -- or a new result. Note that in the absence of any new streaming activity
    -- or risk threshold changes, Voice ID always returns cached Fraud
    -- Detection result for this API.
    fraudDetectionResultId :: Prelude.Maybe Prelude.Text,
    -- | The fraud detection decision produced by Voice ID, processed against the
    -- current session state and streamed audio of the speaker.
    decision :: Prelude.Maybe FraudDetectionDecision,
    -- | The @FraudDetectionConfiguration@ used to generate this fraud detection
    -- result.
    configuration :: Prelude.Maybe FraudDetectionConfiguration,
    -- | A timestamp indicating when audio aggregation started for this fraud
    -- detection result.
    audioAggregationStartedAt :: Prelude.Maybe Core.POSIX,
    -- | A timestamp indicating when audio aggregation ended for this fraud
    -- detection result.
    audioAggregationEndedAt :: Prelude.Maybe Core.POSIX
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
-- 'reasons', 'fraudDetectionResult_reasons' - The reason speaker was flagged by the fraud detection system. This is
-- only be populated if fraud detection Decision is @HIGH_RISK@, and only
-- has one possible value: @KNOWN_FRAUDSTER@.
--
-- 'riskDetails', 'fraudDetectionResult_riskDetails' - Details about each risk analyzed for this speaker.
--
-- 'fraudDetectionResultId', 'fraudDetectionResult_fraudDetectionResultId' - The unique identifier for this fraud detection result. Given there can
-- be multiple fraud detections for a given session, this field helps in
-- identifying if the returned result is from previous streaming activity
-- or a new result. Note that in the absence of any new streaming activity
-- or risk threshold changes, Voice ID always returns cached Fraud
-- Detection result for this API.
--
-- 'decision', 'fraudDetectionResult_decision' - The fraud detection decision produced by Voice ID, processed against the
-- current session state and streamed audio of the speaker.
--
-- 'configuration', 'fraudDetectionResult_configuration' - The @FraudDetectionConfiguration@ used to generate this fraud detection
-- result.
--
-- 'audioAggregationStartedAt', 'fraudDetectionResult_audioAggregationStartedAt' - A timestamp indicating when audio aggregation started for this fraud
-- detection result.
--
-- 'audioAggregationEndedAt', 'fraudDetectionResult_audioAggregationEndedAt' - A timestamp indicating when audio aggregation ended for this fraud
-- detection result.
newFraudDetectionResult ::
  FraudDetectionResult
newFraudDetectionResult =
  FraudDetectionResult'
    { reasons = Prelude.Nothing,
      riskDetails = Prelude.Nothing,
      fraudDetectionResultId = Prelude.Nothing,
      decision = Prelude.Nothing,
      configuration = Prelude.Nothing,
      audioAggregationStartedAt = Prelude.Nothing,
      audioAggregationEndedAt = Prelude.Nothing
    }

-- | The reason speaker was flagged by the fraud detection system. This is
-- only be populated if fraud detection Decision is @HIGH_RISK@, and only
-- has one possible value: @KNOWN_FRAUDSTER@.
fraudDetectionResult_reasons :: Lens.Lens' FraudDetectionResult (Prelude.Maybe [FraudDetectionReason])
fraudDetectionResult_reasons = Lens.lens (\FraudDetectionResult' {reasons} -> reasons) (\s@FraudDetectionResult' {} a -> s {reasons = a} :: FraudDetectionResult) Prelude.. Lens.mapping Lens.coerced

-- | Details about each risk analyzed for this speaker.
fraudDetectionResult_riskDetails :: Lens.Lens' FraudDetectionResult (Prelude.Maybe FraudRiskDetails)
fraudDetectionResult_riskDetails = Lens.lens (\FraudDetectionResult' {riskDetails} -> riskDetails) (\s@FraudDetectionResult' {} a -> s {riskDetails = a} :: FraudDetectionResult)

-- | The unique identifier for this fraud detection result. Given there can
-- be multiple fraud detections for a given session, this field helps in
-- identifying if the returned result is from previous streaming activity
-- or a new result. Note that in the absence of any new streaming activity
-- or risk threshold changes, Voice ID always returns cached Fraud
-- Detection result for this API.
fraudDetectionResult_fraudDetectionResultId :: Lens.Lens' FraudDetectionResult (Prelude.Maybe Prelude.Text)
fraudDetectionResult_fraudDetectionResultId = Lens.lens (\FraudDetectionResult' {fraudDetectionResultId} -> fraudDetectionResultId) (\s@FraudDetectionResult' {} a -> s {fraudDetectionResultId = a} :: FraudDetectionResult)

-- | The fraud detection decision produced by Voice ID, processed against the
-- current session state and streamed audio of the speaker.
fraudDetectionResult_decision :: Lens.Lens' FraudDetectionResult (Prelude.Maybe FraudDetectionDecision)
fraudDetectionResult_decision = Lens.lens (\FraudDetectionResult' {decision} -> decision) (\s@FraudDetectionResult' {} a -> s {decision = a} :: FraudDetectionResult)

-- | The @FraudDetectionConfiguration@ used to generate this fraud detection
-- result.
fraudDetectionResult_configuration :: Lens.Lens' FraudDetectionResult (Prelude.Maybe FraudDetectionConfiguration)
fraudDetectionResult_configuration = Lens.lens (\FraudDetectionResult' {configuration} -> configuration) (\s@FraudDetectionResult' {} a -> s {configuration = a} :: FraudDetectionResult)

-- | A timestamp indicating when audio aggregation started for this fraud
-- detection result.
fraudDetectionResult_audioAggregationStartedAt :: Lens.Lens' FraudDetectionResult (Prelude.Maybe Prelude.UTCTime)
fraudDetectionResult_audioAggregationStartedAt = Lens.lens (\FraudDetectionResult' {audioAggregationStartedAt} -> audioAggregationStartedAt) (\s@FraudDetectionResult' {} a -> s {audioAggregationStartedAt = a} :: FraudDetectionResult) Prelude.. Lens.mapping Core._Time

-- | A timestamp indicating when audio aggregation ended for this fraud
-- detection result.
fraudDetectionResult_audioAggregationEndedAt :: Lens.Lens' FraudDetectionResult (Prelude.Maybe Prelude.UTCTime)
fraudDetectionResult_audioAggregationEndedAt = Lens.lens (\FraudDetectionResult' {audioAggregationEndedAt} -> audioAggregationEndedAt) (\s@FraudDetectionResult' {} a -> s {audioAggregationEndedAt = a} :: FraudDetectionResult) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON FraudDetectionResult where
  parseJSON =
    Core.withObject
      "FraudDetectionResult"
      ( \x ->
          FraudDetectionResult'
            Prelude.<$> (x Core..:? "Reasons" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "RiskDetails")
            Prelude.<*> (x Core..:? "FraudDetectionResultId")
            Prelude.<*> (x Core..:? "Decision")
            Prelude.<*> (x Core..:? "Configuration")
            Prelude.<*> (x Core..:? "AudioAggregationStartedAt")
            Prelude.<*> (x Core..:? "AudioAggregationEndedAt")
      )

instance Prelude.Hashable FraudDetectionResult

instance Prelude.NFData FraudDetectionResult
