{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FraudDetector.GetEventPredictionMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details of the past fraud predictions for the specified event ID,
-- event type, detector ID, and detector version ID that was generated in
-- the specified time period.
module Amazonka.FraudDetector.GetEventPredictionMetadata
  ( -- * Creating a Request
    GetEventPredictionMetadata (..),
    newGetEventPredictionMetadata,

    -- * Request Lenses
    getEventPredictionMetadata_eventId,
    getEventPredictionMetadata_eventTypeName,
    getEventPredictionMetadata_detectorId,
    getEventPredictionMetadata_detectorVersionId,
    getEventPredictionMetadata_predictionTimestamp,

    -- * Destructuring the Response
    GetEventPredictionMetadataResponse (..),
    newGetEventPredictionMetadataResponse,

    -- * Response Lenses
    getEventPredictionMetadataResponse_entityId,
    getEventPredictionMetadataResponse_eventTimestamp,
    getEventPredictionMetadataResponse_detectorVersionId,
    getEventPredictionMetadataResponse_rules,
    getEventPredictionMetadataResponse_evaluatedExternalModels,
    getEventPredictionMetadataResponse_detectorVersionStatus,
    getEventPredictionMetadataResponse_evaluatedModelVersions,
    getEventPredictionMetadataResponse_eventId,
    getEventPredictionMetadataResponse_outcomes,
    getEventPredictionMetadataResponse_entityType,
    getEventPredictionMetadataResponse_predictionTimestamp,
    getEventPredictionMetadataResponse_ruleExecutionMode,
    getEventPredictionMetadataResponse_eventTypeName,
    getEventPredictionMetadataResponse_detectorId,
    getEventPredictionMetadataResponse_eventVariables,
    getEventPredictionMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEventPredictionMetadata' smart constructor.
data GetEventPredictionMetadata = GetEventPredictionMetadata'
  { -- | The event ID.
    eventId :: Prelude.Text,
    -- | The event type associated with the detector specified for the
    -- prediction.
    eventTypeName :: Prelude.Text,
    -- | The detector ID.
    detectorId :: Prelude.Text,
    -- | The detector version ID.
    detectorVersionId :: Prelude.Text,
    -- | The timestamp that defines when the prediction was generated. The
    -- timestamp must be specified using ISO 8601 standard in UTC.
    --
    -- We recommend calling
    -- <https://docs.aws.amazon.com/frauddetector/latest/api/API_ListEventPredictions.html ListEventPredictions>
    -- first, and using the @predictionTimestamp@ value in the response to
    -- provide an accurate prediction timestamp value.
    predictionTimestamp :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventPredictionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventId', 'getEventPredictionMetadata_eventId' - The event ID.
--
-- 'eventTypeName', 'getEventPredictionMetadata_eventTypeName' - The event type associated with the detector specified for the
-- prediction.
--
-- 'detectorId', 'getEventPredictionMetadata_detectorId' - The detector ID.
--
-- 'detectorVersionId', 'getEventPredictionMetadata_detectorVersionId' - The detector version ID.
--
-- 'predictionTimestamp', 'getEventPredictionMetadata_predictionTimestamp' - The timestamp that defines when the prediction was generated. The
-- timestamp must be specified using ISO 8601 standard in UTC.
--
-- We recommend calling
-- <https://docs.aws.amazon.com/frauddetector/latest/api/API_ListEventPredictions.html ListEventPredictions>
-- first, and using the @predictionTimestamp@ value in the response to
-- provide an accurate prediction timestamp value.
newGetEventPredictionMetadata ::
  -- | 'eventId'
  Prelude.Text ->
  -- | 'eventTypeName'
  Prelude.Text ->
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'detectorVersionId'
  Prelude.Text ->
  -- | 'predictionTimestamp'
  Prelude.Text ->
  GetEventPredictionMetadata
newGetEventPredictionMetadata
  pEventId_
  pEventTypeName_
  pDetectorId_
  pDetectorVersionId_
  pPredictionTimestamp_ =
    GetEventPredictionMetadata'
      { eventId = pEventId_,
        eventTypeName = pEventTypeName_,
        detectorId = pDetectorId_,
        detectorVersionId = pDetectorVersionId_,
        predictionTimestamp = pPredictionTimestamp_
      }

-- | The event ID.
getEventPredictionMetadata_eventId :: Lens.Lens' GetEventPredictionMetadata Prelude.Text
getEventPredictionMetadata_eventId = Lens.lens (\GetEventPredictionMetadata' {eventId} -> eventId) (\s@GetEventPredictionMetadata' {} a -> s {eventId = a} :: GetEventPredictionMetadata)

-- | The event type associated with the detector specified for the
-- prediction.
getEventPredictionMetadata_eventTypeName :: Lens.Lens' GetEventPredictionMetadata Prelude.Text
getEventPredictionMetadata_eventTypeName = Lens.lens (\GetEventPredictionMetadata' {eventTypeName} -> eventTypeName) (\s@GetEventPredictionMetadata' {} a -> s {eventTypeName = a} :: GetEventPredictionMetadata)

-- | The detector ID.
getEventPredictionMetadata_detectorId :: Lens.Lens' GetEventPredictionMetadata Prelude.Text
getEventPredictionMetadata_detectorId = Lens.lens (\GetEventPredictionMetadata' {detectorId} -> detectorId) (\s@GetEventPredictionMetadata' {} a -> s {detectorId = a} :: GetEventPredictionMetadata)

-- | The detector version ID.
getEventPredictionMetadata_detectorVersionId :: Lens.Lens' GetEventPredictionMetadata Prelude.Text
getEventPredictionMetadata_detectorVersionId = Lens.lens (\GetEventPredictionMetadata' {detectorVersionId} -> detectorVersionId) (\s@GetEventPredictionMetadata' {} a -> s {detectorVersionId = a} :: GetEventPredictionMetadata)

-- | The timestamp that defines when the prediction was generated. The
-- timestamp must be specified using ISO 8601 standard in UTC.
--
-- We recommend calling
-- <https://docs.aws.amazon.com/frauddetector/latest/api/API_ListEventPredictions.html ListEventPredictions>
-- first, and using the @predictionTimestamp@ value in the response to
-- provide an accurate prediction timestamp value.
getEventPredictionMetadata_predictionTimestamp :: Lens.Lens' GetEventPredictionMetadata Prelude.Text
getEventPredictionMetadata_predictionTimestamp = Lens.lens (\GetEventPredictionMetadata' {predictionTimestamp} -> predictionTimestamp) (\s@GetEventPredictionMetadata' {} a -> s {predictionTimestamp = a} :: GetEventPredictionMetadata)

instance Core.AWSRequest GetEventPredictionMetadata where
  type
    AWSResponse GetEventPredictionMetadata =
      GetEventPredictionMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEventPredictionMetadataResponse'
            Prelude.<$> (x Core..?> "entityId")
            Prelude.<*> (x Core..?> "eventTimestamp")
            Prelude.<*> (x Core..?> "detectorVersionId")
            Prelude.<*> (x Core..?> "rules" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "evaluatedExternalModels"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "detectorVersionStatus")
            Prelude.<*> ( x Core..?> "evaluatedModelVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "eventId")
            Prelude.<*> (x Core..?> "outcomes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "entityType")
            Prelude.<*> (x Core..?> "predictionTimestamp")
            Prelude.<*> (x Core..?> "ruleExecutionMode")
            Prelude.<*> (x Core..?> "eventTypeName")
            Prelude.<*> (x Core..?> "detectorId")
            Prelude.<*> (x Core..?> "eventVariables" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEventPredictionMetadata where
  hashWithSalt _salt GetEventPredictionMetadata' {..} =
    _salt `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventTypeName
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` detectorVersionId
      `Prelude.hashWithSalt` predictionTimestamp

instance Prelude.NFData GetEventPredictionMetadata where
  rnf GetEventPredictionMetadata' {..} =
    Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf eventTypeName
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf detectorVersionId
      `Prelude.seq` Prelude.rnf predictionTimestamp

instance Core.ToHeaders GetEventPredictionMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.GetEventPredictionMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetEventPredictionMetadata where
  toJSON GetEventPredictionMetadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("eventId" Core..= eventId),
            Prelude.Just ("eventTypeName" Core..= eventTypeName),
            Prelude.Just ("detectorId" Core..= detectorId),
            Prelude.Just
              ("detectorVersionId" Core..= detectorVersionId),
            Prelude.Just
              ("predictionTimestamp" Core..= predictionTimestamp)
          ]
      )

instance Core.ToPath GetEventPredictionMetadata where
  toPath = Prelude.const "/"

instance Core.ToQuery GetEventPredictionMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEventPredictionMetadataResponse' smart constructor.
data GetEventPredictionMetadataResponse = GetEventPredictionMetadataResponse'
  { -- | The entity ID.
    entityId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp for when the prediction was generated for the associated
    -- event ID.
    eventTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The detector version ID.
    detectorVersionId :: Prelude.Maybe Prelude.Text,
    -- | List of rules associated with the detector version that were used for
    -- evaluating variable values.
    rules :: Prelude.Maybe [EvaluatedRule],
    -- | External (Amazon SageMaker) models that were evaluated for generating
    -- predictions.
    evaluatedExternalModels :: Prelude.Maybe [EvaluatedExternalModel],
    -- | The status of the detector version.
    detectorVersionStatus :: Prelude.Maybe Prelude.Text,
    -- | Model versions that were evaluated for generating predictions.
    evaluatedModelVersions :: Prelude.Maybe [EvaluatedModelVersion],
    -- | The event ID.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The outcomes of the matched rule, based on the rule execution mode.
    outcomes :: Prelude.Maybe [Prelude.Text],
    -- | The entity type.
    entityType :: Prelude.Maybe Prelude.Text,
    -- | The timestamp that defines when the prediction was generated.
    predictionTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The execution mode of the rule used for evaluating variable values.
    ruleExecutionMode :: Prelude.Maybe RuleExecutionMode,
    -- | The event type associated with the detector specified for this
    -- prediction.
    eventTypeName :: Prelude.Maybe Prelude.Text,
    -- | The detector ID.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | A list of event variables that influenced the prediction scores.
    eventVariables :: Prelude.Maybe [EventVariableSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventPredictionMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityId', 'getEventPredictionMetadataResponse_entityId' - The entity ID.
--
-- 'eventTimestamp', 'getEventPredictionMetadataResponse_eventTimestamp' - The timestamp for when the prediction was generated for the associated
-- event ID.
--
-- 'detectorVersionId', 'getEventPredictionMetadataResponse_detectorVersionId' - The detector version ID.
--
-- 'rules', 'getEventPredictionMetadataResponse_rules' - List of rules associated with the detector version that were used for
-- evaluating variable values.
--
-- 'evaluatedExternalModels', 'getEventPredictionMetadataResponse_evaluatedExternalModels' - External (Amazon SageMaker) models that were evaluated for generating
-- predictions.
--
-- 'detectorVersionStatus', 'getEventPredictionMetadataResponse_detectorVersionStatus' - The status of the detector version.
--
-- 'evaluatedModelVersions', 'getEventPredictionMetadataResponse_evaluatedModelVersions' - Model versions that were evaluated for generating predictions.
--
-- 'eventId', 'getEventPredictionMetadataResponse_eventId' - The event ID.
--
-- 'outcomes', 'getEventPredictionMetadataResponse_outcomes' - The outcomes of the matched rule, based on the rule execution mode.
--
-- 'entityType', 'getEventPredictionMetadataResponse_entityType' - The entity type.
--
-- 'predictionTimestamp', 'getEventPredictionMetadataResponse_predictionTimestamp' - The timestamp that defines when the prediction was generated.
--
-- 'ruleExecutionMode', 'getEventPredictionMetadataResponse_ruleExecutionMode' - The execution mode of the rule used for evaluating variable values.
--
-- 'eventTypeName', 'getEventPredictionMetadataResponse_eventTypeName' - The event type associated with the detector specified for this
-- prediction.
--
-- 'detectorId', 'getEventPredictionMetadataResponse_detectorId' - The detector ID.
--
-- 'eventVariables', 'getEventPredictionMetadataResponse_eventVariables' - A list of event variables that influenced the prediction scores.
--
-- 'httpStatus', 'getEventPredictionMetadataResponse_httpStatus' - The response's http status code.
newGetEventPredictionMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEventPredictionMetadataResponse
newGetEventPredictionMetadataResponse pHttpStatus_ =
  GetEventPredictionMetadataResponse'
    { entityId =
        Prelude.Nothing,
      eventTimestamp = Prelude.Nothing,
      detectorVersionId = Prelude.Nothing,
      rules = Prelude.Nothing,
      evaluatedExternalModels =
        Prelude.Nothing,
      detectorVersionStatus = Prelude.Nothing,
      evaluatedModelVersions =
        Prelude.Nothing,
      eventId = Prelude.Nothing,
      outcomes = Prelude.Nothing,
      entityType = Prelude.Nothing,
      predictionTimestamp = Prelude.Nothing,
      ruleExecutionMode = Prelude.Nothing,
      eventTypeName = Prelude.Nothing,
      detectorId = Prelude.Nothing,
      eventVariables = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The entity ID.
getEventPredictionMetadataResponse_entityId :: Lens.Lens' GetEventPredictionMetadataResponse (Prelude.Maybe Prelude.Text)
getEventPredictionMetadataResponse_entityId = Lens.lens (\GetEventPredictionMetadataResponse' {entityId} -> entityId) (\s@GetEventPredictionMetadataResponse' {} a -> s {entityId = a} :: GetEventPredictionMetadataResponse)

-- | The timestamp for when the prediction was generated for the associated
-- event ID.
getEventPredictionMetadataResponse_eventTimestamp :: Lens.Lens' GetEventPredictionMetadataResponse (Prelude.Maybe Prelude.Text)
getEventPredictionMetadataResponse_eventTimestamp = Lens.lens (\GetEventPredictionMetadataResponse' {eventTimestamp} -> eventTimestamp) (\s@GetEventPredictionMetadataResponse' {} a -> s {eventTimestamp = a} :: GetEventPredictionMetadataResponse)

-- | The detector version ID.
getEventPredictionMetadataResponse_detectorVersionId :: Lens.Lens' GetEventPredictionMetadataResponse (Prelude.Maybe Prelude.Text)
getEventPredictionMetadataResponse_detectorVersionId = Lens.lens (\GetEventPredictionMetadataResponse' {detectorVersionId} -> detectorVersionId) (\s@GetEventPredictionMetadataResponse' {} a -> s {detectorVersionId = a} :: GetEventPredictionMetadataResponse)

-- | List of rules associated with the detector version that were used for
-- evaluating variable values.
getEventPredictionMetadataResponse_rules :: Lens.Lens' GetEventPredictionMetadataResponse (Prelude.Maybe [EvaluatedRule])
getEventPredictionMetadataResponse_rules = Lens.lens (\GetEventPredictionMetadataResponse' {rules} -> rules) (\s@GetEventPredictionMetadataResponse' {} a -> s {rules = a} :: GetEventPredictionMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | External (Amazon SageMaker) models that were evaluated for generating
-- predictions.
getEventPredictionMetadataResponse_evaluatedExternalModels :: Lens.Lens' GetEventPredictionMetadataResponse (Prelude.Maybe [EvaluatedExternalModel])
getEventPredictionMetadataResponse_evaluatedExternalModels = Lens.lens (\GetEventPredictionMetadataResponse' {evaluatedExternalModels} -> evaluatedExternalModels) (\s@GetEventPredictionMetadataResponse' {} a -> s {evaluatedExternalModels = a} :: GetEventPredictionMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of the detector version.
getEventPredictionMetadataResponse_detectorVersionStatus :: Lens.Lens' GetEventPredictionMetadataResponse (Prelude.Maybe Prelude.Text)
getEventPredictionMetadataResponse_detectorVersionStatus = Lens.lens (\GetEventPredictionMetadataResponse' {detectorVersionStatus} -> detectorVersionStatus) (\s@GetEventPredictionMetadataResponse' {} a -> s {detectorVersionStatus = a} :: GetEventPredictionMetadataResponse)

-- | Model versions that were evaluated for generating predictions.
getEventPredictionMetadataResponse_evaluatedModelVersions :: Lens.Lens' GetEventPredictionMetadataResponse (Prelude.Maybe [EvaluatedModelVersion])
getEventPredictionMetadataResponse_evaluatedModelVersions = Lens.lens (\GetEventPredictionMetadataResponse' {evaluatedModelVersions} -> evaluatedModelVersions) (\s@GetEventPredictionMetadataResponse' {} a -> s {evaluatedModelVersions = a} :: GetEventPredictionMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The event ID.
getEventPredictionMetadataResponse_eventId :: Lens.Lens' GetEventPredictionMetadataResponse (Prelude.Maybe Prelude.Text)
getEventPredictionMetadataResponse_eventId = Lens.lens (\GetEventPredictionMetadataResponse' {eventId} -> eventId) (\s@GetEventPredictionMetadataResponse' {} a -> s {eventId = a} :: GetEventPredictionMetadataResponse)

-- | The outcomes of the matched rule, based on the rule execution mode.
getEventPredictionMetadataResponse_outcomes :: Lens.Lens' GetEventPredictionMetadataResponse (Prelude.Maybe [Prelude.Text])
getEventPredictionMetadataResponse_outcomes = Lens.lens (\GetEventPredictionMetadataResponse' {outcomes} -> outcomes) (\s@GetEventPredictionMetadataResponse' {} a -> s {outcomes = a} :: GetEventPredictionMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The entity type.
getEventPredictionMetadataResponse_entityType :: Lens.Lens' GetEventPredictionMetadataResponse (Prelude.Maybe Prelude.Text)
getEventPredictionMetadataResponse_entityType = Lens.lens (\GetEventPredictionMetadataResponse' {entityType} -> entityType) (\s@GetEventPredictionMetadataResponse' {} a -> s {entityType = a} :: GetEventPredictionMetadataResponse)

-- | The timestamp that defines when the prediction was generated.
getEventPredictionMetadataResponse_predictionTimestamp :: Lens.Lens' GetEventPredictionMetadataResponse (Prelude.Maybe Prelude.Text)
getEventPredictionMetadataResponse_predictionTimestamp = Lens.lens (\GetEventPredictionMetadataResponse' {predictionTimestamp} -> predictionTimestamp) (\s@GetEventPredictionMetadataResponse' {} a -> s {predictionTimestamp = a} :: GetEventPredictionMetadataResponse)

-- | The execution mode of the rule used for evaluating variable values.
getEventPredictionMetadataResponse_ruleExecutionMode :: Lens.Lens' GetEventPredictionMetadataResponse (Prelude.Maybe RuleExecutionMode)
getEventPredictionMetadataResponse_ruleExecutionMode = Lens.lens (\GetEventPredictionMetadataResponse' {ruleExecutionMode} -> ruleExecutionMode) (\s@GetEventPredictionMetadataResponse' {} a -> s {ruleExecutionMode = a} :: GetEventPredictionMetadataResponse)

-- | The event type associated with the detector specified for this
-- prediction.
getEventPredictionMetadataResponse_eventTypeName :: Lens.Lens' GetEventPredictionMetadataResponse (Prelude.Maybe Prelude.Text)
getEventPredictionMetadataResponse_eventTypeName = Lens.lens (\GetEventPredictionMetadataResponse' {eventTypeName} -> eventTypeName) (\s@GetEventPredictionMetadataResponse' {} a -> s {eventTypeName = a} :: GetEventPredictionMetadataResponse)

-- | The detector ID.
getEventPredictionMetadataResponse_detectorId :: Lens.Lens' GetEventPredictionMetadataResponse (Prelude.Maybe Prelude.Text)
getEventPredictionMetadataResponse_detectorId = Lens.lens (\GetEventPredictionMetadataResponse' {detectorId} -> detectorId) (\s@GetEventPredictionMetadataResponse' {} a -> s {detectorId = a} :: GetEventPredictionMetadataResponse)

-- | A list of event variables that influenced the prediction scores.
getEventPredictionMetadataResponse_eventVariables :: Lens.Lens' GetEventPredictionMetadataResponse (Prelude.Maybe [EventVariableSummary])
getEventPredictionMetadataResponse_eventVariables = Lens.lens (\GetEventPredictionMetadataResponse' {eventVariables} -> eventVariables) (\s@GetEventPredictionMetadataResponse' {} a -> s {eventVariables = a} :: GetEventPredictionMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEventPredictionMetadataResponse_httpStatus :: Lens.Lens' GetEventPredictionMetadataResponse Prelude.Int
getEventPredictionMetadataResponse_httpStatus = Lens.lens (\GetEventPredictionMetadataResponse' {httpStatus} -> httpStatus) (\s@GetEventPredictionMetadataResponse' {} a -> s {httpStatus = a} :: GetEventPredictionMetadataResponse)

instance
  Prelude.NFData
    GetEventPredictionMetadataResponse
  where
  rnf GetEventPredictionMetadataResponse' {..} =
    Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf eventTimestamp
      `Prelude.seq` Prelude.rnf detectorVersionId
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf evaluatedExternalModels
      `Prelude.seq` Prelude.rnf detectorVersionStatus
      `Prelude.seq` Prelude.rnf evaluatedModelVersions
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf outcomes
      `Prelude.seq` Prelude.rnf entityType
      `Prelude.seq` Prelude.rnf predictionTimestamp
      `Prelude.seq` Prelude.rnf ruleExecutionMode
      `Prelude.seq` Prelude.rnf eventTypeName
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf eventVariables
      `Prelude.seq` Prelude.rnf httpStatus
