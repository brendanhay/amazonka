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
-- Module      : Network.AWS.AuditManager.GetAssessment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an assessment from Audit Manager.
module Network.AWS.AuditManager.GetAssessment
  ( -- * Creating a Request
    GetAssessment (..),
    newGetAssessment,

    -- * Request Lenses
    getAssessment_assessmentId,

    -- * Destructuring the Response
    GetAssessmentResponse (..),
    newGetAssessmentResponse,

    -- * Response Lenses
    getAssessmentResponse_userRole,
    getAssessmentResponse_assessment,
    getAssessmentResponse_httpStatus,
  )
where

import Network.AWS.AuditManager.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAssessment' smart constructor.
data GetAssessment = GetAssessment'
  { -- | The identifier for the specified assessment.
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
-- 'assessmentId', 'getAssessment_assessmentId' - The identifier for the specified assessment.
newGetAssessment ::
  -- | 'assessmentId'
  Prelude.Text ->
  GetAssessment
newGetAssessment pAssessmentId_ =
  GetAssessment' {assessmentId = pAssessmentId_}

-- | The identifier for the specified assessment.
getAssessment_assessmentId :: Lens.Lens' GetAssessment Prelude.Text
getAssessment_assessmentId = Lens.lens (\GetAssessment' {assessmentId} -> assessmentId) (\s@GetAssessment' {} a -> s {assessmentId = a} :: GetAssessment)

instance Core.AWSRequest GetAssessment where
  type
    AWSResponse GetAssessment =
      GetAssessmentResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssessmentResponse'
            Prelude.<$> (x Core..?> "userRole")
            Prelude.<*> (x Core..?> "assessment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAssessment

instance Prelude.NFData GetAssessment

instance Core.ToHeaders GetAssessment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetAssessment where
  toPath GetAssessment' {..} =
    Prelude.mconcat
      ["/assessments/", Core.toBS assessmentId]

instance Core.ToQuery GetAssessment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAssessmentResponse' smart constructor.
data GetAssessmentResponse = GetAssessmentResponse'
  { userRole :: Prelude.Maybe Role,
    assessment :: Prelude.Maybe Assessment,
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
-- 'userRole', 'getAssessmentResponse_userRole' - Undocumented member.
--
-- 'assessment', 'getAssessmentResponse_assessment' - Undocumented member.
--
-- 'httpStatus', 'getAssessmentResponse_httpStatus' - The response's http status code.
newGetAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssessmentResponse
newGetAssessmentResponse pHttpStatus_ =
  GetAssessmentResponse'
    { userRole = Prelude.Nothing,
      assessment = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getAssessmentResponse_userRole :: Lens.Lens' GetAssessmentResponse (Prelude.Maybe Role)
getAssessmentResponse_userRole = Lens.lens (\GetAssessmentResponse' {userRole} -> userRole) (\s@GetAssessmentResponse' {} a -> s {userRole = a} :: GetAssessmentResponse)

-- | Undocumented member.
getAssessmentResponse_assessment :: Lens.Lens' GetAssessmentResponse (Prelude.Maybe Assessment)
getAssessmentResponse_assessment = Lens.lens (\GetAssessmentResponse' {assessment} -> assessment) (\s@GetAssessmentResponse' {} a -> s {assessment = a} :: GetAssessmentResponse)

-- | The response's http status code.
getAssessmentResponse_httpStatus :: Lens.Lens' GetAssessmentResponse Prelude.Int
getAssessmentResponse_httpStatus = Lens.lens (\GetAssessmentResponse' {httpStatus} -> httpStatus) (\s@GetAssessmentResponse' {} a -> s {httpStatus = a} :: GetAssessmentResponse)

instance Prelude.NFData GetAssessmentResponse
