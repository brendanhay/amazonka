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
-- Module      : Network.AWS.MechanicalTurk.RejectAssignment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @RejectAssignment@ operation rejects the results of a completed
-- assignment.
--
-- You can include an optional feedback message with the rejection, which
-- the Worker can see in the Status section of the web site. When you
-- include a feedback message with the rejection, it helps the Worker
-- understand why the assignment was rejected, and can improve the quality
-- of the results the Worker submits in the future.
--
-- Only the Requester who created the HIT can reject an assignment for the
-- HIT.
module Network.AWS.MechanicalTurk.RejectAssignment
  ( -- * Creating a Request
    RejectAssignment (..),
    newRejectAssignment,

    -- * Request Lenses
    rejectAssignment_assignmentId,
    rejectAssignment_requesterFeedback,

    -- * Destructuring the Response
    RejectAssignmentResponse (..),
    newRejectAssignmentResponse,

    -- * Response Lenses
    rejectAssignmentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRejectAssignment' smart constructor.
data RejectAssignment = RejectAssignment'
  { -- | The ID of the assignment. The assignment must correspond to a HIT
    -- created by the Requester.
    assignmentId :: Core.Text,
    -- | A message for the Worker, which the Worker can see in the Status section
    -- of the web site.
    requesterFeedback :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RejectAssignment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignmentId', 'rejectAssignment_assignmentId' - The ID of the assignment. The assignment must correspond to a HIT
-- created by the Requester.
--
-- 'requesterFeedback', 'rejectAssignment_requesterFeedback' - A message for the Worker, which the Worker can see in the Status section
-- of the web site.
newRejectAssignment ::
  -- | 'assignmentId'
  Core.Text ->
  -- | 'requesterFeedback'
  Core.Text ->
  RejectAssignment
newRejectAssignment
  pAssignmentId_
  pRequesterFeedback_ =
    RejectAssignment'
      { assignmentId = pAssignmentId_,
        requesterFeedback = pRequesterFeedback_
      }

-- | The ID of the assignment. The assignment must correspond to a HIT
-- created by the Requester.
rejectAssignment_assignmentId :: Lens.Lens' RejectAssignment Core.Text
rejectAssignment_assignmentId = Lens.lens (\RejectAssignment' {assignmentId} -> assignmentId) (\s@RejectAssignment' {} a -> s {assignmentId = a} :: RejectAssignment)

-- | A message for the Worker, which the Worker can see in the Status section
-- of the web site.
rejectAssignment_requesterFeedback :: Lens.Lens' RejectAssignment Core.Text
rejectAssignment_requesterFeedback = Lens.lens (\RejectAssignment' {requesterFeedback} -> requesterFeedback) (\s@RejectAssignment' {} a -> s {requesterFeedback = a} :: RejectAssignment)

instance Core.AWSRequest RejectAssignment where
  type
    AWSResponse RejectAssignment =
      RejectAssignmentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectAssignmentResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RejectAssignment

instance Core.NFData RejectAssignment

instance Core.ToHeaders RejectAssignment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.RejectAssignment" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RejectAssignment where
  toJSON RejectAssignment' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AssignmentId" Core..= assignmentId),
            Core.Just
              ("RequesterFeedback" Core..= requesterFeedback)
          ]
      )

instance Core.ToPath RejectAssignment where
  toPath = Core.const "/"

instance Core.ToQuery RejectAssignment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRejectAssignmentResponse' smart constructor.
data RejectAssignmentResponse = RejectAssignmentResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RejectAssignmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'rejectAssignmentResponse_httpStatus' - The response's http status code.
newRejectAssignmentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RejectAssignmentResponse
newRejectAssignmentResponse pHttpStatus_ =
  RejectAssignmentResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
rejectAssignmentResponse_httpStatus :: Lens.Lens' RejectAssignmentResponse Core.Int
rejectAssignmentResponse_httpStatus = Lens.lens (\RejectAssignmentResponse' {httpStatus} -> httpStatus) (\s@RejectAssignmentResponse' {} a -> s {httpStatus = a} :: RejectAssignmentResponse)

instance Core.NFData RejectAssignmentResponse
