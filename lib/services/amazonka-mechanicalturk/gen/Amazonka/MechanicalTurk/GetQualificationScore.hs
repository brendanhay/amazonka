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
-- Module      : Amazonka.MechanicalTurk.GetQualificationScore
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.MechanicalTurk.GetQualificationScore
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetQualificationScore' smart constructor.
data GetQualificationScore = GetQualificationScore'
  { -- | The ID of the QualificationType.
    qualificationTypeId :: Prelude.Text,
    -- | The ID of the Worker whose Qualification is being updated.
    workerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'workerId'
  Prelude.Text ->
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
getQualificationScore_qualificationTypeId :: Lens.Lens' GetQualificationScore Prelude.Text
getQualificationScore_qualificationTypeId = Lens.lens (\GetQualificationScore' {qualificationTypeId} -> qualificationTypeId) (\s@GetQualificationScore' {} a -> s {qualificationTypeId = a} :: GetQualificationScore)

-- | The ID of the Worker whose Qualification is being updated.
getQualificationScore_workerId :: Lens.Lens' GetQualificationScore Prelude.Text
getQualificationScore_workerId = Lens.lens (\GetQualificationScore' {workerId} -> workerId) (\s@GetQualificationScore' {} a -> s {workerId = a} :: GetQualificationScore)

instance Core.AWSRequest GetQualificationScore where
  type
    AWSResponse GetQualificationScore =
      GetQualificationScoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQualificationScoreResponse'
            Prelude.<$> (x Data..?> "Qualification")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetQualificationScore where
  hashWithSalt _salt GetQualificationScore' {..} =
    _salt
      `Prelude.hashWithSalt` qualificationTypeId
      `Prelude.hashWithSalt` workerId

instance Prelude.NFData GetQualificationScore where
  rnf GetQualificationScore' {..} =
    Prelude.rnf qualificationTypeId
      `Prelude.seq` Prelude.rnf workerId

instance Data.ToHeaders GetQualificationScore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.GetQualificationScore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetQualificationScore where
  toJSON GetQualificationScore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("QualificationTypeId" Data..= qualificationTypeId),
            Prelude.Just ("WorkerId" Data..= workerId)
          ]
      )

instance Data.ToPath GetQualificationScore where
  toPath = Prelude.const "/"

instance Data.ToQuery GetQualificationScore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetQualificationScoreResponse' smart constructor.
data GetQualificationScoreResponse = GetQualificationScoreResponse'
  { -- | The Qualification data structure of the Qualification assigned to a
    -- user, including the Qualification type and the value (score).
    qualification :: Prelude.Maybe Qualification,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetQualificationScoreResponse
newGetQualificationScoreResponse pHttpStatus_ =
  GetQualificationScoreResponse'
    { qualification =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Qualification data structure of the Qualification assigned to a
-- user, including the Qualification type and the value (score).
getQualificationScoreResponse_qualification :: Lens.Lens' GetQualificationScoreResponse (Prelude.Maybe Qualification)
getQualificationScoreResponse_qualification = Lens.lens (\GetQualificationScoreResponse' {qualification} -> qualification) (\s@GetQualificationScoreResponse' {} a -> s {qualification = a} :: GetQualificationScoreResponse)

-- | The response's http status code.
getQualificationScoreResponse_httpStatus :: Lens.Lens' GetQualificationScoreResponse Prelude.Int
getQualificationScoreResponse_httpStatus = Lens.lens (\GetQualificationScoreResponse' {httpStatus} -> httpStatus) (\s@GetQualificationScoreResponse' {} a -> s {httpStatus = a} :: GetQualificationScoreResponse)

instance Prelude.NFData GetQualificationScoreResponse where
  rnf GetQualificationScoreResponse' {..} =
    Prelude.rnf qualification
      `Prelude.seq` Prelude.rnf httpStatus
