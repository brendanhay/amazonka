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
-- Module      : Amazonka.AuditManager.ValidateAssessmentReportIntegrity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the integrity of an assessment report in Audit Manager.
module Amazonka.AuditManager.ValidateAssessmentReportIntegrity
  ( -- * Creating a Request
    ValidateAssessmentReportIntegrity (..),
    newValidateAssessmentReportIntegrity,

    -- * Request Lenses
    validateAssessmentReportIntegrity_s3RelativePath,

    -- * Destructuring the Response
    ValidateAssessmentReportIntegrityResponse (..),
    newValidateAssessmentReportIntegrityResponse,

    -- * Response Lenses
    validateAssessmentReportIntegrityResponse_signatureAlgorithm,
    validateAssessmentReportIntegrityResponse_signatureDateTime,
    validateAssessmentReportIntegrityResponse_signatureKeyId,
    validateAssessmentReportIntegrityResponse_signatureValid,
    validateAssessmentReportIntegrityResponse_validationErrors,
    validateAssessmentReportIntegrityResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newValidateAssessmentReportIntegrity' smart constructor.
data ValidateAssessmentReportIntegrity = ValidateAssessmentReportIntegrity'
  { -- | The relative path of the Amazon S3 bucket that the assessment report is
    -- stored in.
    s3RelativePath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateAssessmentReportIntegrity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3RelativePath', 'validateAssessmentReportIntegrity_s3RelativePath' - The relative path of the Amazon S3 bucket that the assessment report is
-- stored in.
newValidateAssessmentReportIntegrity ::
  -- | 's3RelativePath'
  Prelude.Text ->
  ValidateAssessmentReportIntegrity
newValidateAssessmentReportIntegrity pS3RelativePath_ =
  ValidateAssessmentReportIntegrity'
    { s3RelativePath =
        pS3RelativePath_
    }

-- | The relative path of the Amazon S3 bucket that the assessment report is
-- stored in.
validateAssessmentReportIntegrity_s3RelativePath :: Lens.Lens' ValidateAssessmentReportIntegrity Prelude.Text
validateAssessmentReportIntegrity_s3RelativePath = Lens.lens (\ValidateAssessmentReportIntegrity' {s3RelativePath} -> s3RelativePath) (\s@ValidateAssessmentReportIntegrity' {} a -> s {s3RelativePath = a} :: ValidateAssessmentReportIntegrity)

instance
  Core.AWSRequest
    ValidateAssessmentReportIntegrity
  where
  type
    AWSResponse ValidateAssessmentReportIntegrity =
      ValidateAssessmentReportIntegrityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidateAssessmentReportIntegrityResponse'
            Prelude.<$> (x Data..?> "signatureAlgorithm")
              Prelude.<*> (x Data..?> "signatureDateTime")
              Prelude.<*> (x Data..?> "signatureKeyId")
              Prelude.<*> (x Data..?> "signatureValid")
              Prelude.<*> ( x Data..?> "validationErrors"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ValidateAssessmentReportIntegrity
  where
  hashWithSalt
    _salt
    ValidateAssessmentReportIntegrity' {..} =
      _salt `Prelude.hashWithSalt` s3RelativePath

instance
  Prelude.NFData
    ValidateAssessmentReportIntegrity
  where
  rnf ValidateAssessmentReportIntegrity' {..} =
    Prelude.rnf s3RelativePath

instance
  Data.ToHeaders
    ValidateAssessmentReportIntegrity
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ValidateAssessmentReportIntegrity
  where
  toJSON ValidateAssessmentReportIntegrity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("s3RelativePath" Data..= s3RelativePath)
          ]
      )

instance
  Data.ToPath
    ValidateAssessmentReportIntegrity
  where
  toPath = Prelude.const "/assessmentReports/integrity"

instance
  Data.ToQuery
    ValidateAssessmentReportIntegrity
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newValidateAssessmentReportIntegrityResponse' smart constructor.
data ValidateAssessmentReportIntegrityResponse = ValidateAssessmentReportIntegrityResponse'
  { -- | The signature algorithm that\'s used to code sign the assessment report
    -- file.
    signatureAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | The date and time signature that specifies when the assessment report
    -- was created.
    signatureDateTime :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the validation signature key.
    signatureKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the signature key is valid.
    signatureValid :: Prelude.Maybe Prelude.Bool,
    -- | Represents any errors that occurred when validating the assessment
    -- report.
    validationErrors :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateAssessmentReportIntegrityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signatureAlgorithm', 'validateAssessmentReportIntegrityResponse_signatureAlgorithm' - The signature algorithm that\'s used to code sign the assessment report
-- file.
--
-- 'signatureDateTime', 'validateAssessmentReportIntegrityResponse_signatureDateTime' - The date and time signature that specifies when the assessment report
-- was created.
--
-- 'signatureKeyId', 'validateAssessmentReportIntegrityResponse_signatureKeyId' - The unique identifier for the validation signature key.
--
-- 'signatureValid', 'validateAssessmentReportIntegrityResponse_signatureValid' - Specifies whether the signature key is valid.
--
-- 'validationErrors', 'validateAssessmentReportIntegrityResponse_validationErrors' - Represents any errors that occurred when validating the assessment
-- report.
--
-- 'httpStatus', 'validateAssessmentReportIntegrityResponse_httpStatus' - The response's http status code.
newValidateAssessmentReportIntegrityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ValidateAssessmentReportIntegrityResponse
newValidateAssessmentReportIntegrityResponse
  pHttpStatus_ =
    ValidateAssessmentReportIntegrityResponse'
      { signatureAlgorithm =
          Prelude.Nothing,
        signatureDateTime =
          Prelude.Nothing,
        signatureKeyId = Prelude.Nothing,
        signatureValid = Prelude.Nothing,
        validationErrors =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The signature algorithm that\'s used to code sign the assessment report
-- file.
validateAssessmentReportIntegrityResponse_signatureAlgorithm :: Lens.Lens' ValidateAssessmentReportIntegrityResponse (Prelude.Maybe Prelude.Text)
validateAssessmentReportIntegrityResponse_signatureAlgorithm = Lens.lens (\ValidateAssessmentReportIntegrityResponse' {signatureAlgorithm} -> signatureAlgorithm) (\s@ValidateAssessmentReportIntegrityResponse' {} a -> s {signatureAlgorithm = a} :: ValidateAssessmentReportIntegrityResponse)

-- | The date and time signature that specifies when the assessment report
-- was created.
validateAssessmentReportIntegrityResponse_signatureDateTime :: Lens.Lens' ValidateAssessmentReportIntegrityResponse (Prelude.Maybe Prelude.Text)
validateAssessmentReportIntegrityResponse_signatureDateTime = Lens.lens (\ValidateAssessmentReportIntegrityResponse' {signatureDateTime} -> signatureDateTime) (\s@ValidateAssessmentReportIntegrityResponse' {} a -> s {signatureDateTime = a} :: ValidateAssessmentReportIntegrityResponse)

-- | The unique identifier for the validation signature key.
validateAssessmentReportIntegrityResponse_signatureKeyId :: Lens.Lens' ValidateAssessmentReportIntegrityResponse (Prelude.Maybe Prelude.Text)
validateAssessmentReportIntegrityResponse_signatureKeyId = Lens.lens (\ValidateAssessmentReportIntegrityResponse' {signatureKeyId} -> signatureKeyId) (\s@ValidateAssessmentReportIntegrityResponse' {} a -> s {signatureKeyId = a} :: ValidateAssessmentReportIntegrityResponse)

-- | Specifies whether the signature key is valid.
validateAssessmentReportIntegrityResponse_signatureValid :: Lens.Lens' ValidateAssessmentReportIntegrityResponse (Prelude.Maybe Prelude.Bool)
validateAssessmentReportIntegrityResponse_signatureValid = Lens.lens (\ValidateAssessmentReportIntegrityResponse' {signatureValid} -> signatureValid) (\s@ValidateAssessmentReportIntegrityResponse' {} a -> s {signatureValid = a} :: ValidateAssessmentReportIntegrityResponse)

-- | Represents any errors that occurred when validating the assessment
-- report.
validateAssessmentReportIntegrityResponse_validationErrors :: Lens.Lens' ValidateAssessmentReportIntegrityResponse (Prelude.Maybe [Prelude.Text])
validateAssessmentReportIntegrityResponse_validationErrors = Lens.lens (\ValidateAssessmentReportIntegrityResponse' {validationErrors} -> validationErrors) (\s@ValidateAssessmentReportIntegrityResponse' {} a -> s {validationErrors = a} :: ValidateAssessmentReportIntegrityResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
validateAssessmentReportIntegrityResponse_httpStatus :: Lens.Lens' ValidateAssessmentReportIntegrityResponse Prelude.Int
validateAssessmentReportIntegrityResponse_httpStatus = Lens.lens (\ValidateAssessmentReportIntegrityResponse' {httpStatus} -> httpStatus) (\s@ValidateAssessmentReportIntegrityResponse' {} a -> s {httpStatus = a} :: ValidateAssessmentReportIntegrityResponse)

instance
  Prelude.NFData
    ValidateAssessmentReportIntegrityResponse
  where
  rnf ValidateAssessmentReportIntegrityResponse' {..} =
    Prelude.rnf signatureAlgorithm
      `Prelude.seq` Prelude.rnf signatureDateTime
      `Prelude.seq` Prelude.rnf signatureKeyId
      `Prelude.seq` Prelude.rnf signatureValid
      `Prelude.seq` Prelude.rnf validationErrors
      `Prelude.seq` Prelude.rnf httpStatus
