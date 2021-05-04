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
-- Module      : Network.AWS.MechanicalTurk.ApproveAssignment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ApproveAssignment@ operation approves the results of a completed
-- assignment.
--
-- Approving an assignment initiates two payments from the Requester\'s
-- Amazon.com account
--
-- -   The Worker who submitted the results is paid the reward specified in
--     the HIT.
--
-- -   Amazon Mechanical Turk fees are debited.
--
-- If the Requester\'s account does not have adequate funds for these
-- payments, the call to ApproveAssignment returns an exception, and the
-- approval is not processed. You can include an optional feedback message
-- with the approval, which the Worker can see in the Status section of the
-- web site.
--
-- You can also call this operation for assignments that were previous
-- rejected and approve them by explicitly overriding the previous
-- rejection. This only works on rejected assignments that were submitted
-- within the previous 30 days and only if the assignment\'s related HIT
-- has not been deleted.
module Network.AWS.MechanicalTurk.ApproveAssignment
  ( -- * Creating a Request
    ApproveAssignment (..),
    newApproveAssignment,

    -- * Request Lenses
    approveAssignment_requesterFeedback,
    approveAssignment_overrideRejection,
    approveAssignment_assignmentId,

    -- * Destructuring the Response
    ApproveAssignmentResponse (..),
    newApproveAssignmentResponse,

    -- * Response Lenses
    approveAssignmentResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newApproveAssignment' smart constructor.
data ApproveAssignment = ApproveAssignment'
  { -- | A message for the Worker, which the Worker can see in the Status section
    -- of the web site.
    requesterFeedback :: Prelude.Maybe Prelude.Text,
    -- | A flag indicating that an assignment should be approved even if it was
    -- previously rejected. Defaults to @False@.
    overrideRejection :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the assignment. The assignment must correspond to a HIT
    -- created by the Requester.
    assignmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ApproveAssignment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requesterFeedback', 'approveAssignment_requesterFeedback' - A message for the Worker, which the Worker can see in the Status section
-- of the web site.
--
-- 'overrideRejection', 'approveAssignment_overrideRejection' - A flag indicating that an assignment should be approved even if it was
-- previously rejected. Defaults to @False@.
--
-- 'assignmentId', 'approveAssignment_assignmentId' - The ID of the assignment. The assignment must correspond to a HIT
-- created by the Requester.
newApproveAssignment ::
  -- | 'assignmentId'
  Prelude.Text ->
  ApproveAssignment
newApproveAssignment pAssignmentId_ =
  ApproveAssignment'
    { requesterFeedback =
        Prelude.Nothing,
      overrideRejection = Prelude.Nothing,
      assignmentId = pAssignmentId_
    }

-- | A message for the Worker, which the Worker can see in the Status section
-- of the web site.
approveAssignment_requesterFeedback :: Lens.Lens' ApproveAssignment (Prelude.Maybe Prelude.Text)
approveAssignment_requesterFeedback = Lens.lens (\ApproveAssignment' {requesterFeedback} -> requesterFeedback) (\s@ApproveAssignment' {} a -> s {requesterFeedback = a} :: ApproveAssignment)

-- | A flag indicating that an assignment should be approved even if it was
-- previously rejected. Defaults to @False@.
approveAssignment_overrideRejection :: Lens.Lens' ApproveAssignment (Prelude.Maybe Prelude.Bool)
approveAssignment_overrideRejection = Lens.lens (\ApproveAssignment' {overrideRejection} -> overrideRejection) (\s@ApproveAssignment' {} a -> s {overrideRejection = a} :: ApproveAssignment)

-- | The ID of the assignment. The assignment must correspond to a HIT
-- created by the Requester.
approveAssignment_assignmentId :: Lens.Lens' ApproveAssignment Prelude.Text
approveAssignment_assignmentId = Lens.lens (\ApproveAssignment' {assignmentId} -> assignmentId) (\s@ApproveAssignment' {} a -> s {assignmentId = a} :: ApproveAssignment)

instance Prelude.AWSRequest ApproveAssignment where
  type Rs ApproveAssignment = ApproveAssignmentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ApproveAssignmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ApproveAssignment

instance Prelude.NFData ApproveAssignment

instance Prelude.ToHeaders ApproveAssignment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MTurkRequesterServiceV20170117.ApproveAssignment" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ApproveAssignment where
  toJSON ApproveAssignment' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RequesterFeedback" Prelude..=)
              Prelude.<$> requesterFeedback,
            ("OverrideRejection" Prelude..=)
              Prelude.<$> overrideRejection,
            Prelude.Just
              ("AssignmentId" Prelude..= assignmentId)
          ]
      )

instance Prelude.ToPath ApproveAssignment where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ApproveAssignment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newApproveAssignmentResponse' smart constructor.
data ApproveAssignmentResponse = ApproveAssignmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ApproveAssignmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'approveAssignmentResponse_httpStatus' - The response's http status code.
newApproveAssignmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ApproveAssignmentResponse
newApproveAssignmentResponse pHttpStatus_ =
  ApproveAssignmentResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
approveAssignmentResponse_httpStatus :: Lens.Lens' ApproveAssignmentResponse Prelude.Int
approveAssignmentResponse_httpStatus = Lens.lens (\ApproveAssignmentResponse' {httpStatus} -> httpStatus) (\s@ApproveAssignmentResponse' {} a -> s {httpStatus = a} :: ApproveAssignmentResponse)

instance Prelude.NFData ApproveAssignmentResponse
