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
-- Module      : Amazonka.FraudDetector.GetDetectorVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a particular detector version.
module Amazonka.FraudDetector.GetDetectorVersion
  ( -- * Creating a Request
    GetDetectorVersion (..),
    newGetDetectorVersion,

    -- * Request Lenses
    getDetectorVersion_detectorId,
    getDetectorVersion_detectorVersionId,

    -- * Destructuring the Response
    GetDetectorVersionResponse (..),
    newGetDetectorVersionResponse,

    -- * Response Lenses
    getDetectorVersionResponse_arn,
    getDetectorVersionResponse_createdTime,
    getDetectorVersionResponse_description,
    getDetectorVersionResponse_detectorId,
    getDetectorVersionResponse_detectorVersionId,
    getDetectorVersionResponse_externalModelEndpoints,
    getDetectorVersionResponse_lastUpdatedTime,
    getDetectorVersionResponse_modelVersions,
    getDetectorVersionResponse_ruleExecutionMode,
    getDetectorVersionResponse_rules,
    getDetectorVersionResponse_status,
    getDetectorVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDetectorVersion' smart constructor.
data GetDetectorVersion = GetDetectorVersion'
  { -- | The detector ID.
    detectorId :: Prelude.Text,
    -- | The detector version ID.
    detectorVersionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDetectorVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'getDetectorVersion_detectorId' - The detector ID.
--
-- 'detectorVersionId', 'getDetectorVersion_detectorVersionId' - The detector version ID.
newGetDetectorVersion ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'detectorVersionId'
  Prelude.Text ->
  GetDetectorVersion
newGetDetectorVersion
  pDetectorId_
  pDetectorVersionId_ =
    GetDetectorVersion'
      { detectorId = pDetectorId_,
        detectorVersionId = pDetectorVersionId_
      }

-- | The detector ID.
getDetectorVersion_detectorId :: Lens.Lens' GetDetectorVersion Prelude.Text
getDetectorVersion_detectorId = Lens.lens (\GetDetectorVersion' {detectorId} -> detectorId) (\s@GetDetectorVersion' {} a -> s {detectorId = a} :: GetDetectorVersion)

-- | The detector version ID.
getDetectorVersion_detectorVersionId :: Lens.Lens' GetDetectorVersion Prelude.Text
getDetectorVersion_detectorVersionId = Lens.lens (\GetDetectorVersion' {detectorVersionId} -> detectorVersionId) (\s@GetDetectorVersion' {} a -> s {detectorVersionId = a} :: GetDetectorVersion)

instance Core.AWSRequest GetDetectorVersion where
  type
    AWSResponse GetDetectorVersion =
      GetDetectorVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDetectorVersionResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "createdTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "detectorId")
            Prelude.<*> (x Data..?> "detectorVersionId")
            Prelude.<*> ( x
                            Data..?> "externalModelEndpoints"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "lastUpdatedTime")
            Prelude.<*> (x Data..?> "modelVersions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ruleExecutionMode")
            Prelude.<*> (x Data..?> "rules" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDetectorVersion where
  hashWithSalt _salt GetDetectorVersion' {..} =
    _salt
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` detectorVersionId

instance Prelude.NFData GetDetectorVersion where
  rnf GetDetectorVersion' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf detectorVersionId

instance Data.ToHeaders GetDetectorVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.GetDetectorVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDetectorVersion where
  toJSON GetDetectorVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("detectorId" Data..= detectorId),
            Prelude.Just
              ("detectorVersionId" Data..= detectorVersionId)
          ]
      )

instance Data.ToPath GetDetectorVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDetectorVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDetectorVersionResponse' smart constructor.
data GetDetectorVersionResponse = GetDetectorVersionResponse'
  { -- | The detector version ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the detector version was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The detector version description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The detector ID.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | The detector version ID.
    detectorVersionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon SageMaker model endpoints included in the detector version.
    externalModelEndpoints :: Prelude.Maybe [Prelude.Text],
    -- | The timestamp when the detector version was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The model versions included in the detector version.
    modelVersions :: Prelude.Maybe [ModelVersion],
    -- | The execution mode of the rule in the dectector
    --
    -- @FIRST_MATCHED@ indicates that Amazon Fraud Detector evaluates rules
    -- sequentially, first to last, stopping at the first matched rule. Amazon
    -- Fraud dectector then provides the outcomes for that single rule.
    --
    -- @ALL_MATCHED@ indicates that Amazon Fraud Detector evaluates all rules
    -- and returns the outcomes for all matched rules. You can define and edit
    -- the rule mode at the detector version level, when it is in draft status.
    ruleExecutionMode :: Prelude.Maybe RuleExecutionMode,
    -- | The rules included in the detector version.
    rules :: Prelude.Maybe [Rule],
    -- | The status of the detector version.
    status :: Prelude.Maybe DetectorVersionStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDetectorVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getDetectorVersionResponse_arn' - The detector version ARN.
--
-- 'createdTime', 'getDetectorVersionResponse_createdTime' - The timestamp when the detector version was created.
--
-- 'description', 'getDetectorVersionResponse_description' - The detector version description.
--
-- 'detectorId', 'getDetectorVersionResponse_detectorId' - The detector ID.
--
-- 'detectorVersionId', 'getDetectorVersionResponse_detectorVersionId' - The detector version ID.
--
-- 'externalModelEndpoints', 'getDetectorVersionResponse_externalModelEndpoints' - The Amazon SageMaker model endpoints included in the detector version.
--
-- 'lastUpdatedTime', 'getDetectorVersionResponse_lastUpdatedTime' - The timestamp when the detector version was last updated.
--
-- 'modelVersions', 'getDetectorVersionResponse_modelVersions' - The model versions included in the detector version.
--
-- 'ruleExecutionMode', 'getDetectorVersionResponse_ruleExecutionMode' - The execution mode of the rule in the dectector
--
-- @FIRST_MATCHED@ indicates that Amazon Fraud Detector evaluates rules
-- sequentially, first to last, stopping at the first matched rule. Amazon
-- Fraud dectector then provides the outcomes for that single rule.
--
-- @ALL_MATCHED@ indicates that Amazon Fraud Detector evaluates all rules
-- and returns the outcomes for all matched rules. You can define and edit
-- the rule mode at the detector version level, when it is in draft status.
--
-- 'rules', 'getDetectorVersionResponse_rules' - The rules included in the detector version.
--
-- 'status', 'getDetectorVersionResponse_status' - The status of the detector version.
--
-- 'httpStatus', 'getDetectorVersionResponse_httpStatus' - The response's http status code.
newGetDetectorVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDetectorVersionResponse
newGetDetectorVersionResponse pHttpStatus_ =
  GetDetectorVersionResponse'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      detectorId = Prelude.Nothing,
      detectorVersionId = Prelude.Nothing,
      externalModelEndpoints = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      modelVersions = Prelude.Nothing,
      ruleExecutionMode = Prelude.Nothing,
      rules = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detector version ARN.
getDetectorVersionResponse_arn :: Lens.Lens' GetDetectorVersionResponse (Prelude.Maybe Prelude.Text)
getDetectorVersionResponse_arn = Lens.lens (\GetDetectorVersionResponse' {arn} -> arn) (\s@GetDetectorVersionResponse' {} a -> s {arn = a} :: GetDetectorVersionResponse)

-- | The timestamp when the detector version was created.
getDetectorVersionResponse_createdTime :: Lens.Lens' GetDetectorVersionResponse (Prelude.Maybe Prelude.Text)
getDetectorVersionResponse_createdTime = Lens.lens (\GetDetectorVersionResponse' {createdTime} -> createdTime) (\s@GetDetectorVersionResponse' {} a -> s {createdTime = a} :: GetDetectorVersionResponse)

-- | The detector version description.
getDetectorVersionResponse_description :: Lens.Lens' GetDetectorVersionResponse (Prelude.Maybe Prelude.Text)
getDetectorVersionResponse_description = Lens.lens (\GetDetectorVersionResponse' {description} -> description) (\s@GetDetectorVersionResponse' {} a -> s {description = a} :: GetDetectorVersionResponse)

-- | The detector ID.
getDetectorVersionResponse_detectorId :: Lens.Lens' GetDetectorVersionResponse (Prelude.Maybe Prelude.Text)
getDetectorVersionResponse_detectorId = Lens.lens (\GetDetectorVersionResponse' {detectorId} -> detectorId) (\s@GetDetectorVersionResponse' {} a -> s {detectorId = a} :: GetDetectorVersionResponse)

-- | The detector version ID.
getDetectorVersionResponse_detectorVersionId :: Lens.Lens' GetDetectorVersionResponse (Prelude.Maybe Prelude.Text)
getDetectorVersionResponse_detectorVersionId = Lens.lens (\GetDetectorVersionResponse' {detectorVersionId} -> detectorVersionId) (\s@GetDetectorVersionResponse' {} a -> s {detectorVersionId = a} :: GetDetectorVersionResponse)

-- | The Amazon SageMaker model endpoints included in the detector version.
getDetectorVersionResponse_externalModelEndpoints :: Lens.Lens' GetDetectorVersionResponse (Prelude.Maybe [Prelude.Text])
getDetectorVersionResponse_externalModelEndpoints = Lens.lens (\GetDetectorVersionResponse' {externalModelEndpoints} -> externalModelEndpoints) (\s@GetDetectorVersionResponse' {} a -> s {externalModelEndpoints = a} :: GetDetectorVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp when the detector version was last updated.
getDetectorVersionResponse_lastUpdatedTime :: Lens.Lens' GetDetectorVersionResponse (Prelude.Maybe Prelude.Text)
getDetectorVersionResponse_lastUpdatedTime = Lens.lens (\GetDetectorVersionResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@GetDetectorVersionResponse' {} a -> s {lastUpdatedTime = a} :: GetDetectorVersionResponse)

-- | The model versions included in the detector version.
getDetectorVersionResponse_modelVersions :: Lens.Lens' GetDetectorVersionResponse (Prelude.Maybe [ModelVersion])
getDetectorVersionResponse_modelVersions = Lens.lens (\GetDetectorVersionResponse' {modelVersions} -> modelVersions) (\s@GetDetectorVersionResponse' {} a -> s {modelVersions = a} :: GetDetectorVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The execution mode of the rule in the dectector
--
-- @FIRST_MATCHED@ indicates that Amazon Fraud Detector evaluates rules
-- sequentially, first to last, stopping at the first matched rule. Amazon
-- Fraud dectector then provides the outcomes for that single rule.
--
-- @ALL_MATCHED@ indicates that Amazon Fraud Detector evaluates all rules
-- and returns the outcomes for all matched rules. You can define and edit
-- the rule mode at the detector version level, when it is in draft status.
getDetectorVersionResponse_ruleExecutionMode :: Lens.Lens' GetDetectorVersionResponse (Prelude.Maybe RuleExecutionMode)
getDetectorVersionResponse_ruleExecutionMode = Lens.lens (\GetDetectorVersionResponse' {ruleExecutionMode} -> ruleExecutionMode) (\s@GetDetectorVersionResponse' {} a -> s {ruleExecutionMode = a} :: GetDetectorVersionResponse)

-- | The rules included in the detector version.
getDetectorVersionResponse_rules :: Lens.Lens' GetDetectorVersionResponse (Prelude.Maybe [Rule])
getDetectorVersionResponse_rules = Lens.lens (\GetDetectorVersionResponse' {rules} -> rules) (\s@GetDetectorVersionResponse' {} a -> s {rules = a} :: GetDetectorVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of the detector version.
getDetectorVersionResponse_status :: Lens.Lens' GetDetectorVersionResponse (Prelude.Maybe DetectorVersionStatus)
getDetectorVersionResponse_status = Lens.lens (\GetDetectorVersionResponse' {status} -> status) (\s@GetDetectorVersionResponse' {} a -> s {status = a} :: GetDetectorVersionResponse)

-- | The response's http status code.
getDetectorVersionResponse_httpStatus :: Lens.Lens' GetDetectorVersionResponse Prelude.Int
getDetectorVersionResponse_httpStatus = Lens.lens (\GetDetectorVersionResponse' {httpStatus} -> httpStatus) (\s@GetDetectorVersionResponse' {} a -> s {httpStatus = a} :: GetDetectorVersionResponse)

instance Prelude.NFData GetDetectorVersionResponse where
  rnf GetDetectorVersionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf detectorVersionId
      `Prelude.seq` Prelude.rnf externalModelEndpoints
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf modelVersions
      `Prelude.seq` Prelude.rnf ruleExecutionMode
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
