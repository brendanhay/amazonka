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
-- Module      : Amazonka.AuditManager.GetAssessmentReportUrl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the URL of an assessment report in Audit Manager.
module Amazonka.AuditManager.GetAssessmentReportUrl
  ( -- * Creating a Request
    GetAssessmentReportUrl (..),
    newGetAssessmentReportUrl,

    -- * Request Lenses
    getAssessmentReportUrl_assessmentReportId,
    getAssessmentReportUrl_assessmentId,

    -- * Destructuring the Response
    GetAssessmentReportUrlResponse (..),
    newGetAssessmentReportUrlResponse,

    -- * Response Lenses
    getAssessmentReportUrlResponse_preSignedUrl,
    getAssessmentReportUrlResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAssessmentReportUrl' smart constructor.
data GetAssessmentReportUrl = GetAssessmentReportUrl'
  { -- | The unique identifier for the assessment report.
    assessmentReportId :: Prelude.Text,
    -- | The unique identifier for the assessment.
    assessmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssessmentReportUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentReportId', 'getAssessmentReportUrl_assessmentReportId' - The unique identifier for the assessment report.
--
-- 'assessmentId', 'getAssessmentReportUrl_assessmentId' - The unique identifier for the assessment.
newGetAssessmentReportUrl ::
  -- | 'assessmentReportId'
  Prelude.Text ->
  -- | 'assessmentId'
  Prelude.Text ->
  GetAssessmentReportUrl
newGetAssessmentReportUrl
  pAssessmentReportId_
  pAssessmentId_ =
    GetAssessmentReportUrl'
      { assessmentReportId =
          pAssessmentReportId_,
        assessmentId = pAssessmentId_
      }

-- | The unique identifier for the assessment report.
getAssessmentReportUrl_assessmentReportId :: Lens.Lens' GetAssessmentReportUrl Prelude.Text
getAssessmentReportUrl_assessmentReportId = Lens.lens (\GetAssessmentReportUrl' {assessmentReportId} -> assessmentReportId) (\s@GetAssessmentReportUrl' {} a -> s {assessmentReportId = a} :: GetAssessmentReportUrl)

-- | The unique identifier for the assessment.
getAssessmentReportUrl_assessmentId :: Lens.Lens' GetAssessmentReportUrl Prelude.Text
getAssessmentReportUrl_assessmentId = Lens.lens (\GetAssessmentReportUrl' {assessmentId} -> assessmentId) (\s@GetAssessmentReportUrl' {} a -> s {assessmentId = a} :: GetAssessmentReportUrl)

instance Core.AWSRequest GetAssessmentReportUrl where
  type
    AWSResponse GetAssessmentReportUrl =
      GetAssessmentReportUrlResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssessmentReportUrlResponse'
            Prelude.<$> (x Data..?> "preSignedUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAssessmentReportUrl where
  hashWithSalt _salt GetAssessmentReportUrl' {..} =
    _salt
      `Prelude.hashWithSalt` assessmentReportId
      `Prelude.hashWithSalt` assessmentId

instance Prelude.NFData GetAssessmentReportUrl where
  rnf GetAssessmentReportUrl' {..} =
    Prelude.rnf assessmentReportId
      `Prelude.seq` Prelude.rnf assessmentId

instance Data.ToHeaders GetAssessmentReportUrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAssessmentReportUrl where
  toPath GetAssessmentReportUrl' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Data.toBS assessmentId,
        "/reports/",
        Data.toBS assessmentReportId,
        "/url"
      ]

instance Data.ToQuery GetAssessmentReportUrl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAssessmentReportUrlResponse' smart constructor.
data GetAssessmentReportUrlResponse = GetAssessmentReportUrlResponse'
  { preSignedUrl :: Prelude.Maybe URL,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssessmentReportUrlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preSignedUrl', 'getAssessmentReportUrlResponse_preSignedUrl' - Undocumented member.
--
-- 'httpStatus', 'getAssessmentReportUrlResponse_httpStatus' - The response's http status code.
newGetAssessmentReportUrlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssessmentReportUrlResponse
newGetAssessmentReportUrlResponse pHttpStatus_ =
  GetAssessmentReportUrlResponse'
    { preSignedUrl =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getAssessmentReportUrlResponse_preSignedUrl :: Lens.Lens' GetAssessmentReportUrlResponse (Prelude.Maybe URL)
getAssessmentReportUrlResponse_preSignedUrl = Lens.lens (\GetAssessmentReportUrlResponse' {preSignedUrl} -> preSignedUrl) (\s@GetAssessmentReportUrlResponse' {} a -> s {preSignedUrl = a} :: GetAssessmentReportUrlResponse)

-- | The response's http status code.
getAssessmentReportUrlResponse_httpStatus :: Lens.Lens' GetAssessmentReportUrlResponse Prelude.Int
getAssessmentReportUrlResponse_httpStatus = Lens.lens (\GetAssessmentReportUrlResponse' {httpStatus} -> httpStatus) (\s@GetAssessmentReportUrlResponse' {} a -> s {httpStatus = a} :: GetAssessmentReportUrlResponse)

instance
  Prelude.NFData
    GetAssessmentReportUrlResponse
  where
  rnf GetAssessmentReportUrlResponse' {..} =
    Prelude.rnf preSignedUrl
      `Prelude.seq` Prelude.rnf httpStatus
