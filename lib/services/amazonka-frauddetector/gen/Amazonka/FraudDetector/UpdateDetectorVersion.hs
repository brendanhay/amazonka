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
-- Module      : Amazonka.FraudDetector.UpdateDetectorVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a detector version. The detector version attributes that you can
-- update include models, external model endpoints, rules, rule execution
-- mode, and description. You can only update a @DRAFT@ detector version.
module Amazonka.FraudDetector.UpdateDetectorVersion
  ( -- * Creating a Request
    UpdateDetectorVersion (..),
    newUpdateDetectorVersion,

    -- * Request Lenses
    updateDetectorVersion_modelVersions,
    updateDetectorVersion_description,
    updateDetectorVersion_ruleExecutionMode,
    updateDetectorVersion_detectorId,
    updateDetectorVersion_detectorVersionId,
    updateDetectorVersion_externalModelEndpoints,
    updateDetectorVersion_rules,

    -- * Destructuring the Response
    UpdateDetectorVersionResponse (..),
    newUpdateDetectorVersionResponse,

    -- * Response Lenses
    updateDetectorVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDetectorVersion' smart constructor.
data UpdateDetectorVersion = UpdateDetectorVersion'
  { -- | The model versions to include in the detector version.
    modelVersions :: Prelude.Maybe [ModelVersion],
    -- | The detector version description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The rule execution mode to add to the detector.
    --
    -- If you specify @FIRST_MATCHED@, Amazon Fraud Detector evaluates rules
    -- sequentially, first to last, stopping at the first matched rule. Amazon
    -- Fraud dectector then provides the outcomes for that single rule.
    --
    -- If you specifiy @ALL_MATCHED@, Amazon Fraud Detector evaluates all rules
    -- and returns the outcomes for all matched rules. You can define and edit
    -- the rule mode at the detector version level, when it is in draft status.
    --
    -- The default behavior is @FIRST_MATCHED@.
    ruleExecutionMode :: Prelude.Maybe RuleExecutionMode,
    -- | The parent detector ID for the detector version you want to update.
    detectorId :: Prelude.Text,
    -- | The detector version ID.
    detectorVersionId :: Prelude.Text,
    -- | The Amazon SageMaker model endpoints to include in the detector version.
    externalModelEndpoints :: [Prelude.Text],
    -- | The rules to include in the detector version.
    rules :: [Rule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDetectorVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelVersions', 'updateDetectorVersion_modelVersions' - The model versions to include in the detector version.
--
-- 'description', 'updateDetectorVersion_description' - The detector version description.
--
-- 'ruleExecutionMode', 'updateDetectorVersion_ruleExecutionMode' - The rule execution mode to add to the detector.
--
-- If you specify @FIRST_MATCHED@, Amazon Fraud Detector evaluates rules
-- sequentially, first to last, stopping at the first matched rule. Amazon
-- Fraud dectector then provides the outcomes for that single rule.
--
-- If you specifiy @ALL_MATCHED@, Amazon Fraud Detector evaluates all rules
-- and returns the outcomes for all matched rules. You can define and edit
-- the rule mode at the detector version level, when it is in draft status.
--
-- The default behavior is @FIRST_MATCHED@.
--
-- 'detectorId', 'updateDetectorVersion_detectorId' - The parent detector ID for the detector version you want to update.
--
-- 'detectorVersionId', 'updateDetectorVersion_detectorVersionId' - The detector version ID.
--
-- 'externalModelEndpoints', 'updateDetectorVersion_externalModelEndpoints' - The Amazon SageMaker model endpoints to include in the detector version.
--
-- 'rules', 'updateDetectorVersion_rules' - The rules to include in the detector version.
newUpdateDetectorVersion ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'detectorVersionId'
  Prelude.Text ->
  UpdateDetectorVersion
newUpdateDetectorVersion
  pDetectorId_
  pDetectorVersionId_ =
    UpdateDetectorVersion'
      { modelVersions =
          Prelude.Nothing,
        description = Prelude.Nothing,
        ruleExecutionMode = Prelude.Nothing,
        detectorId = pDetectorId_,
        detectorVersionId = pDetectorVersionId_,
        externalModelEndpoints = Prelude.mempty,
        rules = Prelude.mempty
      }

-- | The model versions to include in the detector version.
updateDetectorVersion_modelVersions :: Lens.Lens' UpdateDetectorVersion (Prelude.Maybe [ModelVersion])
updateDetectorVersion_modelVersions = Lens.lens (\UpdateDetectorVersion' {modelVersions} -> modelVersions) (\s@UpdateDetectorVersion' {} a -> s {modelVersions = a} :: UpdateDetectorVersion) Prelude.. Lens.mapping Lens.coerced

-- | The detector version description.
updateDetectorVersion_description :: Lens.Lens' UpdateDetectorVersion (Prelude.Maybe Prelude.Text)
updateDetectorVersion_description = Lens.lens (\UpdateDetectorVersion' {description} -> description) (\s@UpdateDetectorVersion' {} a -> s {description = a} :: UpdateDetectorVersion)

-- | The rule execution mode to add to the detector.
--
-- If you specify @FIRST_MATCHED@, Amazon Fraud Detector evaluates rules
-- sequentially, first to last, stopping at the first matched rule. Amazon
-- Fraud dectector then provides the outcomes for that single rule.
--
-- If you specifiy @ALL_MATCHED@, Amazon Fraud Detector evaluates all rules
-- and returns the outcomes for all matched rules. You can define and edit
-- the rule mode at the detector version level, when it is in draft status.
--
-- The default behavior is @FIRST_MATCHED@.
updateDetectorVersion_ruleExecutionMode :: Lens.Lens' UpdateDetectorVersion (Prelude.Maybe RuleExecutionMode)
updateDetectorVersion_ruleExecutionMode = Lens.lens (\UpdateDetectorVersion' {ruleExecutionMode} -> ruleExecutionMode) (\s@UpdateDetectorVersion' {} a -> s {ruleExecutionMode = a} :: UpdateDetectorVersion)

-- | The parent detector ID for the detector version you want to update.
updateDetectorVersion_detectorId :: Lens.Lens' UpdateDetectorVersion Prelude.Text
updateDetectorVersion_detectorId = Lens.lens (\UpdateDetectorVersion' {detectorId} -> detectorId) (\s@UpdateDetectorVersion' {} a -> s {detectorId = a} :: UpdateDetectorVersion)

-- | The detector version ID.
updateDetectorVersion_detectorVersionId :: Lens.Lens' UpdateDetectorVersion Prelude.Text
updateDetectorVersion_detectorVersionId = Lens.lens (\UpdateDetectorVersion' {detectorVersionId} -> detectorVersionId) (\s@UpdateDetectorVersion' {} a -> s {detectorVersionId = a} :: UpdateDetectorVersion)

-- | The Amazon SageMaker model endpoints to include in the detector version.
updateDetectorVersion_externalModelEndpoints :: Lens.Lens' UpdateDetectorVersion [Prelude.Text]
updateDetectorVersion_externalModelEndpoints = Lens.lens (\UpdateDetectorVersion' {externalModelEndpoints} -> externalModelEndpoints) (\s@UpdateDetectorVersion' {} a -> s {externalModelEndpoints = a} :: UpdateDetectorVersion) Prelude.. Lens.coerced

-- | The rules to include in the detector version.
updateDetectorVersion_rules :: Lens.Lens' UpdateDetectorVersion [Rule]
updateDetectorVersion_rules = Lens.lens (\UpdateDetectorVersion' {rules} -> rules) (\s@UpdateDetectorVersion' {} a -> s {rules = a} :: UpdateDetectorVersion) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateDetectorVersion where
  type
    AWSResponse UpdateDetectorVersion =
      UpdateDetectorVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDetectorVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDetectorVersion where
  hashWithSalt _salt UpdateDetectorVersion' {..} =
    _salt `Prelude.hashWithSalt` modelVersions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ruleExecutionMode
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` detectorVersionId
      `Prelude.hashWithSalt` externalModelEndpoints
      `Prelude.hashWithSalt` rules

instance Prelude.NFData UpdateDetectorVersion where
  rnf UpdateDetectorVersion' {..} =
    Prelude.rnf modelVersions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf ruleExecutionMode
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf detectorVersionId
      `Prelude.seq` Prelude.rnf externalModelEndpoints
      `Prelude.seq` Prelude.rnf rules

instance Data.ToHeaders UpdateDetectorVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.UpdateDetectorVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDetectorVersion where
  toJSON UpdateDetectorVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("modelVersions" Data..=) Prelude.<$> modelVersions,
            ("description" Data..=) Prelude.<$> description,
            ("ruleExecutionMode" Data..=)
              Prelude.<$> ruleExecutionMode,
            Prelude.Just ("detectorId" Data..= detectorId),
            Prelude.Just
              ("detectorVersionId" Data..= detectorVersionId),
            Prelude.Just
              ( "externalModelEndpoints"
                  Data..= externalModelEndpoints
              ),
            Prelude.Just ("rules" Data..= rules)
          ]
      )

instance Data.ToPath UpdateDetectorVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDetectorVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDetectorVersionResponse' smart constructor.
data UpdateDetectorVersionResponse = UpdateDetectorVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDetectorVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDetectorVersionResponse_httpStatus' - The response's http status code.
newUpdateDetectorVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDetectorVersionResponse
newUpdateDetectorVersionResponse pHttpStatus_ =
  UpdateDetectorVersionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDetectorVersionResponse_httpStatus :: Lens.Lens' UpdateDetectorVersionResponse Prelude.Int
updateDetectorVersionResponse_httpStatus = Lens.lens (\UpdateDetectorVersionResponse' {httpStatus} -> httpStatus) (\s@UpdateDetectorVersionResponse' {} a -> s {httpStatus = a} :: UpdateDetectorVersionResponse)

instance Prelude.NFData UpdateDetectorVersionResponse where
  rnf UpdateDetectorVersionResponse' {..} =
    Prelude.rnf httpStatus
