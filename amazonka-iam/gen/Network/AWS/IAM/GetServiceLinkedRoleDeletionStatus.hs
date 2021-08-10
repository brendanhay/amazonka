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
-- Module      : Network.AWS.IAM.GetServiceLinkedRoleDeletionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status of your service-linked role deletion. After you use
-- DeleteServiceLinkedRole to submit a service-linked role for deletion,
-- you can use the @DeletionTaskId@ parameter in
-- @GetServiceLinkedRoleDeletionStatus@ to check the status of the
-- deletion. If the deletion fails, this operation returns the reason that
-- it failed, if that information is returned by the service.
module Network.AWS.IAM.GetServiceLinkedRoleDeletionStatus
  ( -- * Creating a Request
    GetServiceLinkedRoleDeletionStatus (..),
    newGetServiceLinkedRoleDeletionStatus,

    -- * Request Lenses
    getServiceLinkedRoleDeletionStatus_deletionTaskId,

    -- * Destructuring the Response
    GetServiceLinkedRoleDeletionStatusResponse (..),
    newGetServiceLinkedRoleDeletionStatusResponse,

    -- * Response Lenses
    getServiceLinkedRoleDeletionStatusResponse_reason,
    getServiceLinkedRoleDeletionStatusResponse_httpStatus,
    getServiceLinkedRoleDeletionStatusResponse_status,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetServiceLinkedRoleDeletionStatus' smart constructor.
data GetServiceLinkedRoleDeletionStatus = GetServiceLinkedRoleDeletionStatus'
  { -- | The deletion task identifier. This identifier is returned by the
    -- DeleteServiceLinkedRole operation in the format
    -- @task\/aws-service-role\/\<service-principal-name>\/\<role-name>\/\<task-uuid>@.
    deletionTaskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceLinkedRoleDeletionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionTaskId', 'getServiceLinkedRoleDeletionStatus_deletionTaskId' - The deletion task identifier. This identifier is returned by the
-- DeleteServiceLinkedRole operation in the format
-- @task\/aws-service-role\/\<service-principal-name>\/\<role-name>\/\<task-uuid>@.
newGetServiceLinkedRoleDeletionStatus ::
  -- | 'deletionTaskId'
  Prelude.Text ->
  GetServiceLinkedRoleDeletionStatus
newGetServiceLinkedRoleDeletionStatus
  pDeletionTaskId_ =
    GetServiceLinkedRoleDeletionStatus'
      { deletionTaskId =
          pDeletionTaskId_
      }

-- | The deletion task identifier. This identifier is returned by the
-- DeleteServiceLinkedRole operation in the format
-- @task\/aws-service-role\/\<service-principal-name>\/\<role-name>\/\<task-uuid>@.
getServiceLinkedRoleDeletionStatus_deletionTaskId :: Lens.Lens' GetServiceLinkedRoleDeletionStatus Prelude.Text
getServiceLinkedRoleDeletionStatus_deletionTaskId = Lens.lens (\GetServiceLinkedRoleDeletionStatus' {deletionTaskId} -> deletionTaskId) (\s@GetServiceLinkedRoleDeletionStatus' {} a -> s {deletionTaskId = a} :: GetServiceLinkedRoleDeletionStatus)

instance
  Core.AWSRequest
    GetServiceLinkedRoleDeletionStatus
  where
  type
    AWSResponse GetServiceLinkedRoleDeletionStatus =
      GetServiceLinkedRoleDeletionStatusResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetServiceLinkedRoleDeletionStatusResult"
      ( \s h x ->
          GetServiceLinkedRoleDeletionStatusResponse'
            Prelude.<$> (x Core..@? "Reason")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Core..@ "Status")
      )

instance
  Prelude.Hashable
    GetServiceLinkedRoleDeletionStatus

instance
  Prelude.NFData
    GetServiceLinkedRoleDeletionStatus

instance
  Core.ToHeaders
    GetServiceLinkedRoleDeletionStatus
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    GetServiceLinkedRoleDeletionStatus
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetServiceLinkedRoleDeletionStatus
  where
  toQuery GetServiceLinkedRoleDeletionStatus' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "GetServiceLinkedRoleDeletionStatus" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "DeletionTaskId" Core.=: deletionTaskId
      ]

-- | /See:/ 'newGetServiceLinkedRoleDeletionStatusResponse' smart constructor.
data GetServiceLinkedRoleDeletionStatusResponse = GetServiceLinkedRoleDeletionStatusResponse'
  { -- | An object that contains details about the reason the deletion failed.
    reason :: Prelude.Maybe DeletionTaskFailureReasonType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the deletion.
    status :: DeletionTaskStatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceLinkedRoleDeletionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'getServiceLinkedRoleDeletionStatusResponse_reason' - An object that contains details about the reason the deletion failed.
--
-- 'httpStatus', 'getServiceLinkedRoleDeletionStatusResponse_httpStatus' - The response's http status code.
--
-- 'status', 'getServiceLinkedRoleDeletionStatusResponse_status' - The status of the deletion.
newGetServiceLinkedRoleDeletionStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'status'
  DeletionTaskStatusType ->
  GetServiceLinkedRoleDeletionStatusResponse
newGetServiceLinkedRoleDeletionStatusResponse
  pHttpStatus_
  pStatus_ =
    GetServiceLinkedRoleDeletionStatusResponse'
      { reason =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        status = pStatus_
      }

-- | An object that contains details about the reason the deletion failed.
getServiceLinkedRoleDeletionStatusResponse_reason :: Lens.Lens' GetServiceLinkedRoleDeletionStatusResponse (Prelude.Maybe DeletionTaskFailureReasonType)
getServiceLinkedRoleDeletionStatusResponse_reason = Lens.lens (\GetServiceLinkedRoleDeletionStatusResponse' {reason} -> reason) (\s@GetServiceLinkedRoleDeletionStatusResponse' {} a -> s {reason = a} :: GetServiceLinkedRoleDeletionStatusResponse)

-- | The response's http status code.
getServiceLinkedRoleDeletionStatusResponse_httpStatus :: Lens.Lens' GetServiceLinkedRoleDeletionStatusResponse Prelude.Int
getServiceLinkedRoleDeletionStatusResponse_httpStatus = Lens.lens (\GetServiceLinkedRoleDeletionStatusResponse' {httpStatus} -> httpStatus) (\s@GetServiceLinkedRoleDeletionStatusResponse' {} a -> s {httpStatus = a} :: GetServiceLinkedRoleDeletionStatusResponse)

-- | The status of the deletion.
getServiceLinkedRoleDeletionStatusResponse_status :: Lens.Lens' GetServiceLinkedRoleDeletionStatusResponse DeletionTaskStatusType
getServiceLinkedRoleDeletionStatusResponse_status = Lens.lens (\GetServiceLinkedRoleDeletionStatusResponse' {status} -> status) (\s@GetServiceLinkedRoleDeletionStatusResponse' {} a -> s {status = a} :: GetServiceLinkedRoleDeletionStatusResponse)

instance
  Prelude.NFData
    GetServiceLinkedRoleDeletionStatusResponse
