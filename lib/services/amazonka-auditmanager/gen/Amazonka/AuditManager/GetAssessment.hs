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
-- Module      : Amazonka.AuditManager.GetAssessment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an assessment from Audit Manager.
module Amazonka.AuditManager.GetAssessment
  ( -- * Creating a Request
    GetAssessment (..),
    newGetAssessment,

    -- * Request Lenses
    getAssessment_assessmentId,

    -- * Destructuring the Response
    GetAssessmentResponse (..),
    newGetAssessmentResponse,

    -- * Response Lenses
    getAssessmentResponse_assessment,
    getAssessmentResponse_userRole,
    getAssessmentResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAssessment' smart constructor.
data GetAssessment = GetAssessment'
  { -- | The unique identifier for the assessment.
    assessmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentId', 'getAssessment_assessmentId' - The unique identifier for the assessment.
newGetAssessment ::
  -- | 'assessmentId'
  Prelude.Text ->
  GetAssessment
newGetAssessment pAssessmentId_ =
  GetAssessment' {assessmentId = pAssessmentId_}

-- | The unique identifier for the assessment.
getAssessment_assessmentId :: Lens.Lens' GetAssessment Prelude.Text
getAssessment_assessmentId = Lens.lens (\GetAssessment' {assessmentId} -> assessmentId) (\s@GetAssessment' {} a -> s {assessmentId = a} :: GetAssessment)

instance Core.AWSRequest GetAssessment where
  type
    AWSResponse GetAssessment =
      GetAssessmentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssessmentResponse'
            Prelude.<$> (x Data..?> "assessment")
            Prelude.<*> (x Data..?> "userRole")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAssessment where
  hashWithSalt _salt GetAssessment' {..} =
    _salt `Prelude.hashWithSalt` assessmentId

instance Prelude.NFData GetAssessment where
  rnf GetAssessment' {..} = Prelude.rnf assessmentId

instance Data.ToHeaders GetAssessment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAssessment where
  toPath GetAssessment' {..} =
    Prelude.mconcat
      ["/assessments/", Data.toBS assessmentId]

instance Data.ToQuery GetAssessment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAssessmentResponse' smart constructor.
data GetAssessmentResponse = GetAssessmentResponse'
  { assessment :: Prelude.Maybe Assessment,
    userRole :: Prelude.Maybe Role,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssessmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessment', 'getAssessmentResponse_assessment' - Undocumented member.
--
-- 'userRole', 'getAssessmentResponse_userRole' - Undocumented member.
--
-- 'httpStatus', 'getAssessmentResponse_httpStatus' - The response's http status code.
newGetAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssessmentResponse
newGetAssessmentResponse pHttpStatus_ =
  GetAssessmentResponse'
    { assessment =
        Prelude.Nothing,
      userRole = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getAssessmentResponse_assessment :: Lens.Lens' GetAssessmentResponse (Prelude.Maybe Assessment)
getAssessmentResponse_assessment = Lens.lens (\GetAssessmentResponse' {assessment} -> assessment) (\s@GetAssessmentResponse' {} a -> s {assessment = a} :: GetAssessmentResponse)

-- | Undocumented member.
getAssessmentResponse_userRole :: Lens.Lens' GetAssessmentResponse (Prelude.Maybe Role)
getAssessmentResponse_userRole = Lens.lens (\GetAssessmentResponse' {userRole} -> userRole) (\s@GetAssessmentResponse' {} a -> s {userRole = a} :: GetAssessmentResponse)

-- | The response's http status code.
getAssessmentResponse_httpStatus :: Lens.Lens' GetAssessmentResponse Prelude.Int
getAssessmentResponse_httpStatus = Lens.lens (\GetAssessmentResponse' {httpStatus} -> httpStatus) (\s@GetAssessmentResponse' {} a -> s {httpStatus = a} :: GetAssessmentResponse)

instance Prelude.NFData GetAssessmentResponse where
  rnf GetAssessmentResponse' {..} =
    Prelude.rnf assessment
      `Prelude.seq` Prelude.rnf userRole
      `Prelude.seq` Prelude.rnf httpStatus
