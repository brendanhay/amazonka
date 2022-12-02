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
-- Module      : Amazonka.IAM.GetServiceLinkedRoleDeletionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.IAM.GetServiceLinkedRoleDeletionStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetServiceLinkedRoleDeletionStatusResult"
      ( \s h x ->
          GetServiceLinkedRoleDeletionStatusResponse'
            Prelude.<$> (x Data..@? "Reason")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..@ "Status")
      )

instance
  Prelude.Hashable
    GetServiceLinkedRoleDeletionStatus
  where
  hashWithSalt
    _salt
    GetServiceLinkedRoleDeletionStatus' {..} =
      _salt `Prelude.hashWithSalt` deletionTaskId

instance
  Prelude.NFData
    GetServiceLinkedRoleDeletionStatus
  where
  rnf GetServiceLinkedRoleDeletionStatus' {..} =
    Prelude.rnf deletionTaskId

instance
  Data.ToHeaders
    GetServiceLinkedRoleDeletionStatus
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetServiceLinkedRoleDeletionStatus
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetServiceLinkedRoleDeletionStatus
  where
  toQuery GetServiceLinkedRoleDeletionStatus' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetServiceLinkedRoleDeletionStatus" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "DeletionTaskId" Data.=: deletionTaskId
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
  where
  rnf GetServiceLinkedRoleDeletionStatusResponse' {..} =
    Prelude.rnf reason
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf status
