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
-- Module      : Amazonka.FraudDetector.GetEventPrediction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Evaluates an event against a detector version. If a version ID is not
-- provided, the detector’s (@ACTIVE@) version is used.
module Amazonka.FraudDetector.GetEventPrediction
  ( -- * Creating a Request
    GetEventPrediction (..),
    newGetEventPrediction,

    -- * Request Lenses
    getEventPrediction_detectorVersionId,
    getEventPrediction_externalModelEndpointDataBlobs,
    getEventPrediction_detectorId,
    getEventPrediction_eventId,
    getEventPrediction_eventTypeName,
    getEventPrediction_entities,
    getEventPrediction_eventTimestamp,
    getEventPrediction_eventVariables,

    -- * Destructuring the Response
    GetEventPredictionResponse (..),
    newGetEventPredictionResponse,

    -- * Response Lenses
    getEventPredictionResponse_modelScores,
    getEventPredictionResponse_externalModelOutputs,
    getEventPredictionResponse_ruleResults,
    getEventPredictionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEventPrediction' smart constructor.
data GetEventPrediction = GetEventPrediction'
  { -- | The detector version ID.
    detectorVersionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon SageMaker model endpoint input data blobs.
    externalModelEndpointDataBlobs :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text ModelEndpointDataBlob)),
    -- | The detector ID.
    detectorId :: Prelude.Text,
    -- | The unique ID used to identify the event.
    eventId :: Prelude.Text,
    -- | The event type associated with the detector specified for the
    -- prediction.
    eventTypeName :: Prelude.Text,
    -- | The entity type (associated with the detector\'s event type) and
    -- specific entity ID representing who performed the event. If an entity id
    -- is not available, use \"UNKNOWN.\"
    entities :: [Core.Sensitive Entity],
    -- | Timestamp that defines when the event under evaluation occurred. The
    -- timestamp must be specified using ISO 8601 standard in UTC.
    eventTimestamp :: Prelude.Text,
    -- | Names of the event type\'s variables you defined in Amazon Fraud
    -- Detector to represent data elements and their corresponding values for
    -- the event you are sending for evaluation.
    --
    -- You must provide at least one eventVariable
    --
    -- To ensure most accurate fraud prediction and to simplify your data
    -- preparation, Amazon Fraud Detector will replace all missing variables or
    -- values as follows:
    --
    -- __For Amazon Fraud Detector trained models:__
    --
    -- If a null value is provided explicitly for a variable or if a variable
    -- is missing, model will replace the null value or the missing variable
    -- (no variable name in the eventVariables map) with calculated default
    -- mean\/medians for numeric variables and with special values for
    -- categorical variables.
    --
    -- __For imported SageMaker models:__
    --
    -- If a null value is provided explicitly for a variable, the model and
    -- rules will use “null” as the value. If a variable is not provided (no
    -- variable name in the eventVariables map), model and rules will use the
    -- default value that is provided for the variable.
    eventVariables :: Prelude.HashMap Prelude.Text (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventPrediction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorVersionId', 'getEventPrediction_detectorVersionId' - The detector version ID.
--
-- 'externalModelEndpointDataBlobs', 'getEventPrediction_externalModelEndpointDataBlobs' - The Amazon SageMaker model endpoint input data blobs.
--
-- 'detectorId', 'getEventPrediction_detectorId' - The detector ID.
--
-- 'eventId', 'getEventPrediction_eventId' - The unique ID used to identify the event.
--
-- 'eventTypeName', 'getEventPrediction_eventTypeName' - The event type associated with the detector specified for the
-- prediction.
--
-- 'entities', 'getEventPrediction_entities' - The entity type (associated with the detector\'s event type) and
-- specific entity ID representing who performed the event. If an entity id
-- is not available, use \"UNKNOWN.\"
--
-- 'eventTimestamp', 'getEventPrediction_eventTimestamp' - Timestamp that defines when the event under evaluation occurred. The
-- timestamp must be specified using ISO 8601 standard in UTC.
--
-- 'eventVariables', 'getEventPrediction_eventVariables' - Names of the event type\'s variables you defined in Amazon Fraud
-- Detector to represent data elements and their corresponding values for
-- the event you are sending for evaluation.
--
-- You must provide at least one eventVariable
--
-- To ensure most accurate fraud prediction and to simplify your data
-- preparation, Amazon Fraud Detector will replace all missing variables or
-- values as follows:
--
-- __For Amazon Fraud Detector trained models:__
--
-- If a null value is provided explicitly for a variable or if a variable
-- is missing, model will replace the null value or the missing variable
-- (no variable name in the eventVariables map) with calculated default
-- mean\/medians for numeric variables and with special values for
-- categorical variables.
--
-- __For imported SageMaker models:__
--
-- If a null value is provided explicitly for a variable, the model and
-- rules will use “null” as the value. If a variable is not provided (no
-- variable name in the eventVariables map), model and rules will use the
-- default value that is provided for the variable.
newGetEventPrediction ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'eventId'
  Prelude.Text ->
  -- | 'eventTypeName'
  Prelude.Text ->
  -- | 'eventTimestamp'
  Prelude.Text ->
  GetEventPrediction
newGetEventPrediction
  pDetectorId_
  pEventId_
  pEventTypeName_
  pEventTimestamp_ =
    GetEventPrediction'
      { detectorVersionId =
          Prelude.Nothing,
        externalModelEndpointDataBlobs = Prelude.Nothing,
        detectorId = pDetectorId_,
        eventId = pEventId_,
        eventTypeName = pEventTypeName_,
        entities = Prelude.mempty,
        eventTimestamp = pEventTimestamp_,
        eventVariables = Prelude.mempty
      }

-- | The detector version ID.
getEventPrediction_detectorVersionId :: Lens.Lens' GetEventPrediction (Prelude.Maybe Prelude.Text)
getEventPrediction_detectorVersionId = Lens.lens (\GetEventPrediction' {detectorVersionId} -> detectorVersionId) (\s@GetEventPrediction' {} a -> s {detectorVersionId = a} :: GetEventPrediction)

-- | The Amazon SageMaker model endpoint input data blobs.
getEventPrediction_externalModelEndpointDataBlobs :: Lens.Lens' GetEventPrediction (Prelude.Maybe (Prelude.HashMap Prelude.Text ModelEndpointDataBlob))
getEventPrediction_externalModelEndpointDataBlobs = Lens.lens (\GetEventPrediction' {externalModelEndpointDataBlobs} -> externalModelEndpointDataBlobs) (\s@GetEventPrediction' {} a -> s {externalModelEndpointDataBlobs = a} :: GetEventPrediction) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The detector ID.
getEventPrediction_detectorId :: Lens.Lens' GetEventPrediction Prelude.Text
getEventPrediction_detectorId = Lens.lens (\GetEventPrediction' {detectorId} -> detectorId) (\s@GetEventPrediction' {} a -> s {detectorId = a} :: GetEventPrediction)

-- | The unique ID used to identify the event.
getEventPrediction_eventId :: Lens.Lens' GetEventPrediction Prelude.Text
getEventPrediction_eventId = Lens.lens (\GetEventPrediction' {eventId} -> eventId) (\s@GetEventPrediction' {} a -> s {eventId = a} :: GetEventPrediction)

-- | The event type associated with the detector specified for the
-- prediction.
getEventPrediction_eventTypeName :: Lens.Lens' GetEventPrediction Prelude.Text
getEventPrediction_eventTypeName = Lens.lens (\GetEventPrediction' {eventTypeName} -> eventTypeName) (\s@GetEventPrediction' {} a -> s {eventTypeName = a} :: GetEventPrediction)

-- | The entity type (associated with the detector\'s event type) and
-- specific entity ID representing who performed the event. If an entity id
-- is not available, use \"UNKNOWN.\"
getEventPrediction_entities :: Lens.Lens' GetEventPrediction [Entity]
getEventPrediction_entities = Lens.lens (\GetEventPrediction' {entities} -> entities) (\s@GetEventPrediction' {} a -> s {entities = a} :: GetEventPrediction) Prelude.. Lens.coerced

-- | Timestamp that defines when the event under evaluation occurred. The
-- timestamp must be specified using ISO 8601 standard in UTC.
getEventPrediction_eventTimestamp :: Lens.Lens' GetEventPrediction Prelude.Text
getEventPrediction_eventTimestamp = Lens.lens (\GetEventPrediction' {eventTimestamp} -> eventTimestamp) (\s@GetEventPrediction' {} a -> s {eventTimestamp = a} :: GetEventPrediction)

-- | Names of the event type\'s variables you defined in Amazon Fraud
-- Detector to represent data elements and their corresponding values for
-- the event you are sending for evaluation.
--
-- You must provide at least one eventVariable
--
-- To ensure most accurate fraud prediction and to simplify your data
-- preparation, Amazon Fraud Detector will replace all missing variables or
-- values as follows:
--
-- __For Amazon Fraud Detector trained models:__
--
-- If a null value is provided explicitly for a variable or if a variable
-- is missing, model will replace the null value or the missing variable
-- (no variable name in the eventVariables map) with calculated default
-- mean\/medians for numeric variables and with special values for
-- categorical variables.
--
-- __For imported SageMaker models:__
--
-- If a null value is provided explicitly for a variable, the model and
-- rules will use “null” as the value. If a variable is not provided (no
-- variable name in the eventVariables map), model and rules will use the
-- default value that is provided for the variable.
getEventPrediction_eventVariables :: Lens.Lens' GetEventPrediction (Prelude.HashMap Prelude.Text Prelude.Text)
getEventPrediction_eventVariables = Lens.lens (\GetEventPrediction' {eventVariables} -> eventVariables) (\s@GetEventPrediction' {} a -> s {eventVariables = a} :: GetEventPrediction) Prelude.. Lens.coerced

instance Core.AWSRequest GetEventPrediction where
  type
    AWSResponse GetEventPrediction =
      GetEventPredictionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEventPredictionResponse'
            Prelude.<$> (x Core..?> "modelScores" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "externalModelOutputs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "ruleResults" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEventPrediction where
  hashWithSalt _salt GetEventPrediction' {..} =
    _salt `Prelude.hashWithSalt` detectorVersionId
      `Prelude.hashWithSalt` externalModelEndpointDataBlobs
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventTypeName
      `Prelude.hashWithSalt` entities
      `Prelude.hashWithSalt` eventTimestamp
      `Prelude.hashWithSalt` eventVariables

instance Prelude.NFData GetEventPrediction where
  rnf GetEventPrediction' {..} =
    Prelude.rnf detectorVersionId
      `Prelude.seq` Prelude.rnf externalModelEndpointDataBlobs
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf eventTypeName
      `Prelude.seq` Prelude.rnf entities
      `Prelude.seq` Prelude.rnf eventTimestamp
      `Prelude.seq` Prelude.rnf eventVariables

instance Core.ToHeaders GetEventPrediction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.GetEventPrediction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetEventPrediction where
  toJSON GetEventPrediction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("detectorVersionId" Core..=)
              Prelude.<$> detectorVersionId,
            ("externalModelEndpointDataBlobs" Core..=)
              Prelude.<$> externalModelEndpointDataBlobs,
            Prelude.Just ("detectorId" Core..= detectorId),
            Prelude.Just ("eventId" Core..= eventId),
            Prelude.Just ("eventTypeName" Core..= eventTypeName),
            Prelude.Just ("entities" Core..= entities),
            Prelude.Just
              ("eventTimestamp" Core..= eventTimestamp),
            Prelude.Just
              ("eventVariables" Core..= eventVariables)
          ]
      )

instance Core.ToPath GetEventPrediction where
  toPath = Prelude.const "/"

instance Core.ToQuery GetEventPrediction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEventPredictionResponse' smart constructor.
data GetEventPredictionResponse = GetEventPredictionResponse'
  { -- | The model scores. Amazon Fraud Detector generates model scores between 0
    -- and 1000, where 0 is low fraud risk and 1000 is high fraud risk. Model
    -- scores are directly related to the false positive rate (FPR). For
    -- example, a score of 600 corresponds to an estimated 10% false positive
    -- rate whereas a score of 900 corresponds to an estimated 2% false
    -- positive rate.
    modelScores :: Prelude.Maybe [ModelScores],
    -- | The model scores for Amazon SageMaker models.
    externalModelOutputs :: Prelude.Maybe [ExternalModelOutputs],
    -- | The results from the rules.
    ruleResults :: Prelude.Maybe [RuleResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventPredictionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelScores', 'getEventPredictionResponse_modelScores' - The model scores. Amazon Fraud Detector generates model scores between 0
-- and 1000, where 0 is low fraud risk and 1000 is high fraud risk. Model
-- scores are directly related to the false positive rate (FPR). For
-- example, a score of 600 corresponds to an estimated 10% false positive
-- rate whereas a score of 900 corresponds to an estimated 2% false
-- positive rate.
--
-- 'externalModelOutputs', 'getEventPredictionResponse_externalModelOutputs' - The model scores for Amazon SageMaker models.
--
-- 'ruleResults', 'getEventPredictionResponse_ruleResults' - The results from the rules.
--
-- 'httpStatus', 'getEventPredictionResponse_httpStatus' - The response's http status code.
newGetEventPredictionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEventPredictionResponse
newGetEventPredictionResponse pHttpStatus_ =
  GetEventPredictionResponse'
    { modelScores =
        Prelude.Nothing,
      externalModelOutputs = Prelude.Nothing,
      ruleResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The model scores. Amazon Fraud Detector generates model scores between 0
-- and 1000, where 0 is low fraud risk and 1000 is high fraud risk. Model
-- scores are directly related to the false positive rate (FPR). For
-- example, a score of 600 corresponds to an estimated 10% false positive
-- rate whereas a score of 900 corresponds to an estimated 2% false
-- positive rate.
getEventPredictionResponse_modelScores :: Lens.Lens' GetEventPredictionResponse (Prelude.Maybe [ModelScores])
getEventPredictionResponse_modelScores = Lens.lens (\GetEventPredictionResponse' {modelScores} -> modelScores) (\s@GetEventPredictionResponse' {} a -> s {modelScores = a} :: GetEventPredictionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The model scores for Amazon SageMaker models.
getEventPredictionResponse_externalModelOutputs :: Lens.Lens' GetEventPredictionResponse (Prelude.Maybe [ExternalModelOutputs])
getEventPredictionResponse_externalModelOutputs = Lens.lens (\GetEventPredictionResponse' {externalModelOutputs} -> externalModelOutputs) (\s@GetEventPredictionResponse' {} a -> s {externalModelOutputs = a} :: GetEventPredictionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The results from the rules.
getEventPredictionResponse_ruleResults :: Lens.Lens' GetEventPredictionResponse (Prelude.Maybe [RuleResult])
getEventPredictionResponse_ruleResults = Lens.lens (\GetEventPredictionResponse' {ruleResults} -> ruleResults) (\s@GetEventPredictionResponse' {} a -> s {ruleResults = a} :: GetEventPredictionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEventPredictionResponse_httpStatus :: Lens.Lens' GetEventPredictionResponse Prelude.Int
getEventPredictionResponse_httpStatus = Lens.lens (\GetEventPredictionResponse' {httpStatus} -> httpStatus) (\s@GetEventPredictionResponse' {} a -> s {httpStatus = a} :: GetEventPredictionResponse)

instance Prelude.NFData GetEventPredictionResponse where
  rnf GetEventPredictionResponse' {..} =
    Prelude.rnf modelScores
      `Prelude.seq` Prelude.rnf externalModelOutputs
      `Prelude.seq` Prelude.rnf ruleResults
      `Prelude.seq` Prelude.rnf httpStatus
