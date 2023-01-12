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
-- Module      : Amazonka.FraudDetector.CreateDetectorVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a detector version. The detector version starts in a @DRAFT@
-- status.
module Amazonka.FraudDetector.CreateDetectorVersion
  ( -- * Creating a Request
    CreateDetectorVersion (..),
    newCreateDetectorVersion,

    -- * Request Lenses
    createDetectorVersion_description,
    createDetectorVersion_externalModelEndpoints,
    createDetectorVersion_modelVersions,
    createDetectorVersion_ruleExecutionMode,
    createDetectorVersion_tags,
    createDetectorVersion_detectorId,
    createDetectorVersion_rules,

    -- * Destructuring the Response
    CreateDetectorVersionResponse (..),
    newCreateDetectorVersionResponse,

    -- * Response Lenses
    createDetectorVersionResponse_detectorId,
    createDetectorVersionResponse_detectorVersionId,
    createDetectorVersionResponse_status,
    createDetectorVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDetectorVersion' smart constructor.
data CreateDetectorVersion = CreateDetectorVersion'
  { -- | The description of the detector version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Sagemaker model endpoints to include in the detector version.
    externalModelEndpoints :: Prelude.Maybe [Prelude.Text],
    -- | The model versions to include in the detector version.
    modelVersions :: Prelude.Maybe [ModelVersion],
    -- | The rule execution mode for the rules included in the detector version.
    --
    -- You can define and edit the rule mode at the detector version level,
    -- when it is in draft status.
    --
    -- If you specify @FIRST_MATCHED@, Amazon Fraud Detector evaluates rules
    -- sequentially, first to last, stopping at the first matched rule. Amazon
    -- Fraud dectector then provides the outcomes for that single rule.
    --
    -- If you specifiy @ALL_MATCHED@, Amazon Fraud Detector evaluates all rules
    -- and returns the outcomes for all matched rules.
    --
    -- The default behavior is @FIRST_MATCHED@.
    ruleExecutionMode :: Prelude.Maybe RuleExecutionMode,
    -- | A collection of key and value pairs.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the detector under which you want to create a new version.
    detectorId :: Prelude.Text,
    -- | The rules to include in the detector version.
    rules :: [Rule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDetectorVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createDetectorVersion_description' - The description of the detector version.
--
-- 'externalModelEndpoints', 'createDetectorVersion_externalModelEndpoints' - The Amazon Sagemaker model endpoints to include in the detector version.
--
-- 'modelVersions', 'createDetectorVersion_modelVersions' - The model versions to include in the detector version.
--
-- 'ruleExecutionMode', 'createDetectorVersion_ruleExecutionMode' - The rule execution mode for the rules included in the detector version.
--
-- You can define and edit the rule mode at the detector version level,
-- when it is in draft status.
--
-- If you specify @FIRST_MATCHED@, Amazon Fraud Detector evaluates rules
-- sequentially, first to last, stopping at the first matched rule. Amazon
-- Fraud dectector then provides the outcomes for that single rule.
--
-- If you specifiy @ALL_MATCHED@, Amazon Fraud Detector evaluates all rules
-- and returns the outcomes for all matched rules.
--
-- The default behavior is @FIRST_MATCHED@.
--
-- 'tags', 'createDetectorVersion_tags' - A collection of key and value pairs.
--
-- 'detectorId', 'createDetectorVersion_detectorId' - The ID of the detector under which you want to create a new version.
--
-- 'rules', 'createDetectorVersion_rules' - The rules to include in the detector version.
newCreateDetectorVersion ::
  -- | 'detectorId'
  Prelude.Text ->
  CreateDetectorVersion
newCreateDetectorVersion pDetectorId_ =
  CreateDetectorVersion'
    { description =
        Prelude.Nothing,
      externalModelEndpoints = Prelude.Nothing,
      modelVersions = Prelude.Nothing,
      ruleExecutionMode = Prelude.Nothing,
      tags = Prelude.Nothing,
      detectorId = pDetectorId_,
      rules = Prelude.mempty
    }

-- | The description of the detector version.
createDetectorVersion_description :: Lens.Lens' CreateDetectorVersion (Prelude.Maybe Prelude.Text)
createDetectorVersion_description = Lens.lens (\CreateDetectorVersion' {description} -> description) (\s@CreateDetectorVersion' {} a -> s {description = a} :: CreateDetectorVersion)

-- | The Amazon Sagemaker model endpoints to include in the detector version.
createDetectorVersion_externalModelEndpoints :: Lens.Lens' CreateDetectorVersion (Prelude.Maybe [Prelude.Text])
createDetectorVersion_externalModelEndpoints = Lens.lens (\CreateDetectorVersion' {externalModelEndpoints} -> externalModelEndpoints) (\s@CreateDetectorVersion' {} a -> s {externalModelEndpoints = a} :: CreateDetectorVersion) Prelude.. Lens.mapping Lens.coerced

-- | The model versions to include in the detector version.
createDetectorVersion_modelVersions :: Lens.Lens' CreateDetectorVersion (Prelude.Maybe [ModelVersion])
createDetectorVersion_modelVersions = Lens.lens (\CreateDetectorVersion' {modelVersions} -> modelVersions) (\s@CreateDetectorVersion' {} a -> s {modelVersions = a} :: CreateDetectorVersion) Prelude.. Lens.mapping Lens.coerced

-- | The rule execution mode for the rules included in the detector version.
--
-- You can define and edit the rule mode at the detector version level,
-- when it is in draft status.
--
-- If you specify @FIRST_MATCHED@, Amazon Fraud Detector evaluates rules
-- sequentially, first to last, stopping at the first matched rule. Amazon
-- Fraud dectector then provides the outcomes for that single rule.
--
-- If you specifiy @ALL_MATCHED@, Amazon Fraud Detector evaluates all rules
-- and returns the outcomes for all matched rules.
--
-- The default behavior is @FIRST_MATCHED@.
createDetectorVersion_ruleExecutionMode :: Lens.Lens' CreateDetectorVersion (Prelude.Maybe RuleExecutionMode)
createDetectorVersion_ruleExecutionMode = Lens.lens (\CreateDetectorVersion' {ruleExecutionMode} -> ruleExecutionMode) (\s@CreateDetectorVersion' {} a -> s {ruleExecutionMode = a} :: CreateDetectorVersion)

-- | A collection of key and value pairs.
createDetectorVersion_tags :: Lens.Lens' CreateDetectorVersion (Prelude.Maybe [Tag])
createDetectorVersion_tags = Lens.lens (\CreateDetectorVersion' {tags} -> tags) (\s@CreateDetectorVersion' {} a -> s {tags = a} :: CreateDetectorVersion) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the detector under which you want to create a new version.
createDetectorVersion_detectorId :: Lens.Lens' CreateDetectorVersion Prelude.Text
createDetectorVersion_detectorId = Lens.lens (\CreateDetectorVersion' {detectorId} -> detectorId) (\s@CreateDetectorVersion' {} a -> s {detectorId = a} :: CreateDetectorVersion)

-- | The rules to include in the detector version.
createDetectorVersion_rules :: Lens.Lens' CreateDetectorVersion [Rule]
createDetectorVersion_rules = Lens.lens (\CreateDetectorVersion' {rules} -> rules) (\s@CreateDetectorVersion' {} a -> s {rules = a} :: CreateDetectorVersion) Prelude.. Lens.coerced

instance Core.AWSRequest CreateDetectorVersion where
  type
    AWSResponse CreateDetectorVersion =
      CreateDetectorVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDetectorVersionResponse'
            Prelude.<$> (x Data..?> "detectorId")
            Prelude.<*> (x Data..?> "detectorVersionId")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDetectorVersion where
  hashWithSalt _salt CreateDetectorVersion' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` externalModelEndpoints
      `Prelude.hashWithSalt` modelVersions
      `Prelude.hashWithSalt` ruleExecutionMode
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` rules

instance Prelude.NFData CreateDetectorVersion where
  rnf CreateDetectorVersion' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf externalModelEndpoints
      `Prelude.seq` Prelude.rnf modelVersions
      `Prelude.seq` Prelude.rnf ruleExecutionMode
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf rules

instance Data.ToHeaders CreateDetectorVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.CreateDetectorVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDetectorVersion where
  toJSON CreateDetectorVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("externalModelEndpoints" Data..=)
              Prelude.<$> externalModelEndpoints,
            ("modelVersions" Data..=) Prelude.<$> modelVersions,
            ("ruleExecutionMode" Data..=)
              Prelude.<$> ruleExecutionMode,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("detectorId" Data..= detectorId),
            Prelude.Just ("rules" Data..= rules)
          ]
      )

instance Data.ToPath CreateDetectorVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDetectorVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDetectorVersionResponse' smart constructor.
data CreateDetectorVersionResponse = CreateDetectorVersionResponse'
  { -- | The ID for the created version\'s parent detector.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | The ID for the created detector.
    detectorVersionId :: Prelude.Maybe Prelude.Text,
    -- | The status of the detector version.
    status :: Prelude.Maybe DetectorVersionStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDetectorVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'createDetectorVersionResponse_detectorId' - The ID for the created version\'s parent detector.
--
-- 'detectorVersionId', 'createDetectorVersionResponse_detectorVersionId' - The ID for the created detector.
--
-- 'status', 'createDetectorVersionResponse_status' - The status of the detector version.
--
-- 'httpStatus', 'createDetectorVersionResponse_httpStatus' - The response's http status code.
newCreateDetectorVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDetectorVersionResponse
newCreateDetectorVersionResponse pHttpStatus_ =
  CreateDetectorVersionResponse'
    { detectorId =
        Prelude.Nothing,
      detectorVersionId = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID for the created version\'s parent detector.
createDetectorVersionResponse_detectorId :: Lens.Lens' CreateDetectorVersionResponse (Prelude.Maybe Prelude.Text)
createDetectorVersionResponse_detectorId = Lens.lens (\CreateDetectorVersionResponse' {detectorId} -> detectorId) (\s@CreateDetectorVersionResponse' {} a -> s {detectorId = a} :: CreateDetectorVersionResponse)

-- | The ID for the created detector.
createDetectorVersionResponse_detectorVersionId :: Lens.Lens' CreateDetectorVersionResponse (Prelude.Maybe Prelude.Text)
createDetectorVersionResponse_detectorVersionId = Lens.lens (\CreateDetectorVersionResponse' {detectorVersionId} -> detectorVersionId) (\s@CreateDetectorVersionResponse' {} a -> s {detectorVersionId = a} :: CreateDetectorVersionResponse)

-- | The status of the detector version.
createDetectorVersionResponse_status :: Lens.Lens' CreateDetectorVersionResponse (Prelude.Maybe DetectorVersionStatus)
createDetectorVersionResponse_status = Lens.lens (\CreateDetectorVersionResponse' {status} -> status) (\s@CreateDetectorVersionResponse' {} a -> s {status = a} :: CreateDetectorVersionResponse)

-- | The response's http status code.
createDetectorVersionResponse_httpStatus :: Lens.Lens' CreateDetectorVersionResponse Prelude.Int
createDetectorVersionResponse_httpStatus = Lens.lens (\CreateDetectorVersionResponse' {httpStatus} -> httpStatus) (\s@CreateDetectorVersionResponse' {} a -> s {httpStatus = a} :: CreateDetectorVersionResponse)

instance Prelude.NFData CreateDetectorVersionResponse where
  rnf CreateDetectorVersionResponse' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf detectorVersionId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
