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
-- Module      : Amazonka.AuditManager.GetInsightsByAssessment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the latest analytics data for a specific active assessment.
module Amazonka.AuditManager.GetInsightsByAssessment
  ( -- * Creating a Request
    GetInsightsByAssessment (..),
    newGetInsightsByAssessment,

    -- * Request Lenses
    getInsightsByAssessment_assessmentId,

    -- * Destructuring the Response
    GetInsightsByAssessmentResponse (..),
    newGetInsightsByAssessmentResponse,

    -- * Response Lenses
    getInsightsByAssessmentResponse_insights,
    getInsightsByAssessmentResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetInsightsByAssessment' smart constructor.
data GetInsightsByAssessment = GetInsightsByAssessment'
  { -- | The unique identifier for the assessment.
    assessmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsightsByAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentId', 'getInsightsByAssessment_assessmentId' - The unique identifier for the assessment.
newGetInsightsByAssessment ::
  -- | 'assessmentId'
  Prelude.Text ->
  GetInsightsByAssessment
newGetInsightsByAssessment pAssessmentId_ =
  GetInsightsByAssessment'
    { assessmentId =
        pAssessmentId_
    }

-- | The unique identifier for the assessment.
getInsightsByAssessment_assessmentId :: Lens.Lens' GetInsightsByAssessment Prelude.Text
getInsightsByAssessment_assessmentId = Lens.lens (\GetInsightsByAssessment' {assessmentId} -> assessmentId) (\s@GetInsightsByAssessment' {} a -> s {assessmentId = a} :: GetInsightsByAssessment)

instance Core.AWSRequest GetInsightsByAssessment where
  type
    AWSResponse GetInsightsByAssessment =
      GetInsightsByAssessmentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightsByAssessmentResponse'
            Prelude.<$> (x Data..?> "insights")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInsightsByAssessment where
  hashWithSalt _salt GetInsightsByAssessment' {..} =
    _salt `Prelude.hashWithSalt` assessmentId

instance Prelude.NFData GetInsightsByAssessment where
  rnf GetInsightsByAssessment' {..} =
    Prelude.rnf assessmentId

instance Data.ToHeaders GetInsightsByAssessment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetInsightsByAssessment where
  toPath GetInsightsByAssessment' {..} =
    Prelude.mconcat
      ["/insights/assessments/", Data.toBS assessmentId]

instance Data.ToQuery GetInsightsByAssessment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInsightsByAssessmentResponse' smart constructor.
data GetInsightsByAssessmentResponse = GetInsightsByAssessmentResponse'
  { -- | The assessment analytics data that the @GetInsightsByAssessment@ API
    -- returned.
    insights :: Prelude.Maybe InsightsByAssessment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsightsByAssessmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insights', 'getInsightsByAssessmentResponse_insights' - The assessment analytics data that the @GetInsightsByAssessment@ API
-- returned.
--
-- 'httpStatus', 'getInsightsByAssessmentResponse_httpStatus' - The response's http status code.
newGetInsightsByAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInsightsByAssessmentResponse
newGetInsightsByAssessmentResponse pHttpStatus_ =
  GetInsightsByAssessmentResponse'
    { insights =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The assessment analytics data that the @GetInsightsByAssessment@ API
-- returned.
getInsightsByAssessmentResponse_insights :: Lens.Lens' GetInsightsByAssessmentResponse (Prelude.Maybe InsightsByAssessment)
getInsightsByAssessmentResponse_insights = Lens.lens (\GetInsightsByAssessmentResponse' {insights} -> insights) (\s@GetInsightsByAssessmentResponse' {} a -> s {insights = a} :: GetInsightsByAssessmentResponse)

-- | The response's http status code.
getInsightsByAssessmentResponse_httpStatus :: Lens.Lens' GetInsightsByAssessmentResponse Prelude.Int
getInsightsByAssessmentResponse_httpStatus = Lens.lens (\GetInsightsByAssessmentResponse' {httpStatus} -> httpStatus) (\s@GetInsightsByAssessmentResponse' {} a -> s {httpStatus = a} :: GetInsightsByAssessmentResponse)

instance
  Prelude.NFData
    GetInsightsByAssessmentResponse
  where
  rnf GetInsightsByAssessmentResponse' {..} =
    Prelude.rnf insights
      `Prelude.seq` Prelude.rnf httpStatus
