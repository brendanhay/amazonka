{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRejectAssignment' smart constructor.
data RejectAssignment = RejectAssignment'
  { -- | The ID of the assignment. The assignment must correspond to a HIT
    -- created by the Requester.
    assignmentId :: Prelude.Text,
    -- | A message for the Worker, which the Worker can see in the Status section
    -- of the web site.
    requesterFeedback :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'requesterFeedback'
  Prelude.Text ->
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
rejectAssignment_assignmentId :: Lens.Lens' RejectAssignment Prelude.Text
rejectAssignment_assignmentId = Lens.lens (\RejectAssignment' {assignmentId} -> assignmentId) (\s@RejectAssignment' {} a -> s {assignmentId = a} :: RejectAssignment)

-- | A message for the Worker, which the Worker can see in the Status section
-- of the web site.
rejectAssignment_requesterFeedback :: Lens.Lens' RejectAssignment Prelude.Text
rejectAssignment_requesterFeedback = Lens.lens (\RejectAssignment' {requesterFeedback} -> requesterFeedback) (\s@RejectAssignment' {} a -> s {requesterFeedback = a} :: RejectAssignment)

instance Prelude.AWSRequest RejectAssignment where
  type Rs RejectAssignment = RejectAssignmentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectAssignmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RejectAssignment

instance Prelude.NFData RejectAssignment

instance Prelude.ToHeaders RejectAssignment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MTurkRequesterServiceV20170117.RejectAssignment" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RejectAssignment where
  toJSON RejectAssignment' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AssignmentId" Prelude..= assignmentId),
            Prelude.Just
              ("RequesterFeedback" Prelude..= requesterFeedback)
          ]
      )

instance Prelude.ToPath RejectAssignment where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RejectAssignment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectAssignmentResponse' smart constructor.
data RejectAssignmentResponse = RejectAssignmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  RejectAssignmentResponse
newRejectAssignmentResponse pHttpStatus_ =
  RejectAssignmentResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
rejectAssignmentResponse_httpStatus :: Lens.Lens' RejectAssignmentResponse Prelude.Int
rejectAssignmentResponse_httpStatus = Lens.lens (\RejectAssignmentResponse' {httpStatus} -> httpStatus) (\s@RejectAssignmentResponse' {} a -> s {httpStatus = a} :: RejectAssignmentResponse)

instance Prelude.NFData RejectAssignmentResponse
