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
-- Module      : Network.AWS.MechanicalTurk.GetQualificationScore
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetQualificationScore@ operation returns the value of a Worker\'s
-- Qualification for a given Qualification type.
--
-- To get a Worker\'s Qualification, you must know the Worker\'s ID. The
-- Worker\'s ID is included in the assignment data returned by the
-- @ListAssignmentsForHIT@ operation.
--
-- Only the owner of a Qualification type can query the value of a
-- Worker\'s Qualification of that type.
module Network.AWS.MechanicalTurk.GetQualificationScore
  ( -- * Creating a Request
    GetQualificationScore (..),
    newGetQualificationScore,

    -- * Request Lenses
    getQualificationScore_qualificationTypeId,
    getQualificationScore_workerId,

    -- * Destructuring the Response
    GetQualificationScoreResponse (..),
    newGetQualificationScoreResponse,

    -- * Response Lenses
    getQualificationScoreResponse_qualification,
    getQualificationScoreResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetQualificationScore' smart constructor.
data GetQualificationScore = GetQualificationScore'
  { -- | The ID of the QualificationType.
    qualificationTypeId :: Core.Text,
    -- | The ID of the Worker whose Qualification is being updated.
    workerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetQualificationScore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualificationTypeId', 'getQualificationScore_qualificationTypeId' - The ID of the QualificationType.
--
-- 'workerId', 'getQualificationScore_workerId' - The ID of the Worker whose Qualification is being updated.
newGetQualificationScore ::
  -- | 'qualificationTypeId'
  Core.Text ->
  -- | 'workerId'
  Core.Text ->
  GetQualificationScore
newGetQualificationScore
  pQualificationTypeId_
  pWorkerId_ =
    GetQualificationScore'
      { qualificationTypeId =
          pQualificationTypeId_,
        workerId = pWorkerId_
      }

-- | The ID of the QualificationType.
getQualificationScore_qualificationTypeId :: Lens.Lens' GetQualificationScore Core.Text
getQualificationScore_qualificationTypeId = Lens.lens (\GetQualificationScore' {qualificationTypeId} -> qualificationTypeId) (\s@GetQualificationScore' {} a -> s {qualificationTypeId = a} :: GetQualificationScore)

-- | The ID of the Worker whose Qualification is being updated.
getQualificationScore_workerId :: Lens.Lens' GetQualificationScore Core.Text
getQualificationScore_workerId = Lens.lens (\GetQualificationScore' {workerId} -> workerId) (\s@GetQualificationScore' {} a -> s {workerId = a} :: GetQualificationScore)

instance Core.AWSRequest GetQualificationScore where
  type
    AWSResponse GetQualificationScore =
      GetQualificationScoreResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQualificationScoreResponse'
            Core.<$> (x Core..?> "Qualification")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetQualificationScore

instance Core.NFData GetQualificationScore

instance Core.ToHeaders GetQualificationScore where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.GetQualificationScore" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetQualificationScore where
  toJSON GetQualificationScore' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("QualificationTypeId" Core..= qualificationTypeId),
            Core.Just ("WorkerId" Core..= workerId)
          ]
      )

instance Core.ToPath GetQualificationScore where
  toPath = Core.const "/"

instance Core.ToQuery GetQualificationScore where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetQualificationScoreResponse' smart constructor.
data GetQualificationScoreResponse = GetQualificationScoreResponse'
  { -- | The Qualification data structure of the Qualification assigned to a
    -- user, including the Qualification type and the value (score).
    qualification :: Core.Maybe Qualification,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetQualificationScoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualification', 'getQualificationScoreResponse_qualification' - The Qualification data structure of the Qualification assigned to a
-- user, including the Qualification type and the value (score).
--
-- 'httpStatus', 'getQualificationScoreResponse_httpStatus' - The response's http status code.
newGetQualificationScoreResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetQualificationScoreResponse
newGetQualificationScoreResponse pHttpStatus_ =
  GetQualificationScoreResponse'
    { qualification =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Qualification data structure of the Qualification assigned to a
-- user, including the Qualification type and the value (score).
getQualificationScoreResponse_qualification :: Lens.Lens' GetQualificationScoreResponse (Core.Maybe Qualification)
getQualificationScoreResponse_qualification = Lens.lens (\GetQualificationScoreResponse' {qualification} -> qualification) (\s@GetQualificationScoreResponse' {} a -> s {qualification = a} :: GetQualificationScoreResponse)

-- | The response's http status code.
getQualificationScoreResponse_httpStatus :: Lens.Lens' GetQualificationScoreResponse Core.Int
getQualificationScoreResponse_httpStatus = Lens.lens (\GetQualificationScoreResponse' {httpStatus} -> httpStatus) (\s@GetQualificationScoreResponse' {} a -> s {httpStatus = a} :: GetQualificationScoreResponse)

instance Core.NFData GetQualificationScoreResponse
