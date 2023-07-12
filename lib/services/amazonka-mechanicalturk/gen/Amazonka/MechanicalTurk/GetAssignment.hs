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
-- Module      : Amazonka.MechanicalTurk.GetAssignment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetAssignment@ operation retrieves the details of the specified
-- Assignment.
module Amazonka.MechanicalTurk.GetAssignment
  ( -- * Creating a Request
    GetAssignment (..),
    newGetAssignment,

    -- * Request Lenses
    getAssignment_assignmentId,

    -- * Destructuring the Response
    GetAssignmentResponse (..),
    newGetAssignmentResponse,

    -- * Response Lenses
    getAssignmentResponse_assignment,
    getAssignmentResponse_hit,
    getAssignmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAssignment' smart constructor.
data GetAssignment = GetAssignment'
  { -- | The ID of the Assignment to be retrieved.
    assignmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssignment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignmentId', 'getAssignment_assignmentId' - The ID of the Assignment to be retrieved.
newGetAssignment ::
  -- | 'assignmentId'
  Prelude.Text ->
  GetAssignment
newGetAssignment pAssignmentId_ =
  GetAssignment' {assignmentId = pAssignmentId_}

-- | The ID of the Assignment to be retrieved.
getAssignment_assignmentId :: Lens.Lens' GetAssignment Prelude.Text
getAssignment_assignmentId = Lens.lens (\GetAssignment' {assignmentId} -> assignmentId) (\s@GetAssignment' {} a -> s {assignmentId = a} :: GetAssignment)

instance Core.AWSRequest GetAssignment where
  type
    AWSResponse GetAssignment =
      GetAssignmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssignmentResponse'
            Prelude.<$> (x Data..?> "Assignment")
            Prelude.<*> (x Data..?> "HIT")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAssignment where
  hashWithSalt _salt GetAssignment' {..} =
    _salt `Prelude.hashWithSalt` assignmentId

instance Prelude.NFData GetAssignment where
  rnf GetAssignment' {..} = Prelude.rnf assignmentId

instance Data.ToHeaders GetAssignment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.GetAssignment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAssignment where
  toJSON GetAssignment' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AssignmentId" Data..= assignmentId)]
      )

instance Data.ToPath GetAssignment where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAssignment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAssignmentResponse' smart constructor.
data GetAssignmentResponse = GetAssignmentResponse'
  { -- | The assignment. The response includes one Assignment element.
    assignment :: Prelude.Maybe Assignment,
    -- | The HIT associated with this assignment. The response includes one HIT
    -- element.
    hit :: Prelude.Maybe HIT,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssignmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignment', 'getAssignmentResponse_assignment' - The assignment. The response includes one Assignment element.
--
-- 'hit', 'getAssignmentResponse_hit' - The HIT associated with this assignment. The response includes one HIT
-- element.
--
-- 'httpStatus', 'getAssignmentResponse_httpStatus' - The response's http status code.
newGetAssignmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssignmentResponse
newGetAssignmentResponse pHttpStatus_ =
  GetAssignmentResponse'
    { assignment =
        Prelude.Nothing,
      hit = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The assignment. The response includes one Assignment element.
getAssignmentResponse_assignment :: Lens.Lens' GetAssignmentResponse (Prelude.Maybe Assignment)
getAssignmentResponse_assignment = Lens.lens (\GetAssignmentResponse' {assignment} -> assignment) (\s@GetAssignmentResponse' {} a -> s {assignment = a} :: GetAssignmentResponse)

-- | The HIT associated with this assignment. The response includes one HIT
-- element.
getAssignmentResponse_hit :: Lens.Lens' GetAssignmentResponse (Prelude.Maybe HIT)
getAssignmentResponse_hit = Lens.lens (\GetAssignmentResponse' {hit} -> hit) (\s@GetAssignmentResponse' {} a -> s {hit = a} :: GetAssignmentResponse)

-- | The response's http status code.
getAssignmentResponse_httpStatus :: Lens.Lens' GetAssignmentResponse Prelude.Int
getAssignmentResponse_httpStatus = Lens.lens (\GetAssignmentResponse' {httpStatus} -> httpStatus) (\s@GetAssignmentResponse' {} a -> s {httpStatus = a} :: GetAssignmentResponse)

instance Prelude.NFData GetAssignmentResponse where
  rnf GetAssignmentResponse' {..} =
    Prelude.rnf assignment
      `Prelude.seq` Prelude.rnf hit
      `Prelude.seq` Prelude.rnf httpStatus
