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
-- Module      : Network.AWS.MechanicalTurk.GetAssignment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetAssignment@ operation retrieves the details of the specified
-- Assignment.
module Network.AWS.MechanicalTurk.GetAssignment
  ( -- * Creating a Request
    GetAssignment (..),
    newGetAssignment,

    -- * Request Lenses
    getAssignment_assignmentId,

    -- * Destructuring the Response
    GetAssignmentResponse (..),
    newGetAssignmentResponse,

    -- * Response Lenses
    getAssignmentResponse_hit,
    getAssignmentResponse_assignment,
    getAssignmentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAssignment' smart constructor.
data GetAssignment = GetAssignment'
  { -- | The ID of the Assignment to be retrieved.
    assignmentId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetAssignment
newGetAssignment pAssignmentId_ =
  GetAssignment' {assignmentId = pAssignmentId_}

-- | The ID of the Assignment to be retrieved.
getAssignment_assignmentId :: Lens.Lens' GetAssignment Core.Text
getAssignment_assignmentId = Lens.lens (\GetAssignment' {assignmentId} -> assignmentId) (\s@GetAssignment' {} a -> s {assignmentId = a} :: GetAssignment)

instance Core.AWSRequest GetAssignment where
  type
    AWSResponse GetAssignment =
      GetAssignmentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssignmentResponse'
            Core.<$> (x Core..?> "HIT")
            Core.<*> (x Core..?> "Assignment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAssignment

instance Core.NFData GetAssignment

instance Core.ToHeaders GetAssignment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.GetAssignment" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAssignment where
  toJSON GetAssignment' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("AssignmentId" Core..= assignmentId)]
      )

instance Core.ToPath GetAssignment where
  toPath = Core.const "/"

instance Core.ToQuery GetAssignment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAssignmentResponse' smart constructor.
data GetAssignmentResponse = GetAssignmentResponse'
  { -- | The HIT associated with this assignment. The response includes one HIT
    -- element.
    hit :: Core.Maybe HIT,
    -- | The assignment. The response includes one Assignment element.
    assignment :: Core.Maybe Assignment,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAssignmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hit', 'getAssignmentResponse_hit' - The HIT associated with this assignment. The response includes one HIT
-- element.
--
-- 'assignment', 'getAssignmentResponse_assignment' - The assignment. The response includes one Assignment element.
--
-- 'httpStatus', 'getAssignmentResponse_httpStatus' - The response's http status code.
newGetAssignmentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAssignmentResponse
newGetAssignmentResponse pHttpStatus_ =
  GetAssignmentResponse'
    { hit = Core.Nothing,
      assignment = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The HIT associated with this assignment. The response includes one HIT
-- element.
getAssignmentResponse_hit :: Lens.Lens' GetAssignmentResponse (Core.Maybe HIT)
getAssignmentResponse_hit = Lens.lens (\GetAssignmentResponse' {hit} -> hit) (\s@GetAssignmentResponse' {} a -> s {hit = a} :: GetAssignmentResponse)

-- | The assignment. The response includes one Assignment element.
getAssignmentResponse_assignment :: Lens.Lens' GetAssignmentResponse (Core.Maybe Assignment)
getAssignmentResponse_assignment = Lens.lens (\GetAssignmentResponse' {assignment} -> assignment) (\s@GetAssignmentResponse' {} a -> s {assignment = a} :: GetAssignmentResponse)

-- | The response's http status code.
getAssignmentResponse_httpStatus :: Lens.Lens' GetAssignmentResponse Core.Int
getAssignmentResponse_httpStatus = Lens.lens (\GetAssignmentResponse' {httpStatus} -> httpStatus) (\s@GetAssignmentResponse' {} a -> s {httpStatus = a} :: GetAssignmentResponse)

instance Core.NFData GetAssignmentResponse
