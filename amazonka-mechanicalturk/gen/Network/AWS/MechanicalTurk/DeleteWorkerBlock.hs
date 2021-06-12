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
-- Module      : Network.AWS.MechanicalTurk.DeleteWorkerBlock
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteWorkerBlock@ operation allows you to reinstate a blocked
-- Worker to work on your HITs. This operation reverses the effects of the
-- CreateWorkerBlock operation. You need the Worker ID to use this
-- operation. If the Worker ID is missing or invalid, this operation fails
-- and returns the message “WorkerId is invalid.” If the specified Worker
-- is not blocked, this operation returns successfully.
module Network.AWS.MechanicalTurk.DeleteWorkerBlock
  ( -- * Creating a Request
    DeleteWorkerBlock (..),
    newDeleteWorkerBlock,

    -- * Request Lenses
    deleteWorkerBlock_reason,
    deleteWorkerBlock_workerId,

    -- * Destructuring the Response
    DeleteWorkerBlockResponse (..),
    newDeleteWorkerBlockResponse,

    -- * Response Lenses
    deleteWorkerBlockResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteWorkerBlock' smart constructor.
data DeleteWorkerBlock = DeleteWorkerBlock'
  { -- | A message that explains the reason for unblocking the Worker. The Worker
    -- does not see this message.
    reason :: Core.Maybe Core.Text,
    -- | The ID of the Worker to unblock.
    workerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteWorkerBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'deleteWorkerBlock_reason' - A message that explains the reason for unblocking the Worker. The Worker
-- does not see this message.
--
-- 'workerId', 'deleteWorkerBlock_workerId' - The ID of the Worker to unblock.
newDeleteWorkerBlock ::
  -- | 'workerId'
  Core.Text ->
  DeleteWorkerBlock
newDeleteWorkerBlock pWorkerId_ =
  DeleteWorkerBlock'
    { reason = Core.Nothing,
      workerId = pWorkerId_
    }

-- | A message that explains the reason for unblocking the Worker. The Worker
-- does not see this message.
deleteWorkerBlock_reason :: Lens.Lens' DeleteWorkerBlock (Core.Maybe Core.Text)
deleteWorkerBlock_reason = Lens.lens (\DeleteWorkerBlock' {reason} -> reason) (\s@DeleteWorkerBlock' {} a -> s {reason = a} :: DeleteWorkerBlock)

-- | The ID of the Worker to unblock.
deleteWorkerBlock_workerId :: Lens.Lens' DeleteWorkerBlock Core.Text
deleteWorkerBlock_workerId = Lens.lens (\DeleteWorkerBlock' {workerId} -> workerId) (\s@DeleteWorkerBlock' {} a -> s {workerId = a} :: DeleteWorkerBlock)

instance Core.AWSRequest DeleteWorkerBlock where
  type
    AWSResponse DeleteWorkerBlock =
      DeleteWorkerBlockResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkerBlockResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteWorkerBlock

instance Core.NFData DeleteWorkerBlock

instance Core.ToHeaders DeleteWorkerBlock where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.DeleteWorkerBlock" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteWorkerBlock where
  toJSON DeleteWorkerBlock' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Reason" Core..=) Core.<$> reason,
            Core.Just ("WorkerId" Core..= workerId)
          ]
      )

instance Core.ToPath DeleteWorkerBlock where
  toPath = Core.const "/"

instance Core.ToQuery DeleteWorkerBlock where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteWorkerBlockResponse' smart constructor.
data DeleteWorkerBlockResponse = DeleteWorkerBlockResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteWorkerBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkerBlockResponse_httpStatus' - The response's http status code.
newDeleteWorkerBlockResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteWorkerBlockResponse
newDeleteWorkerBlockResponse pHttpStatus_ =
  DeleteWorkerBlockResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteWorkerBlockResponse_httpStatus :: Lens.Lens' DeleteWorkerBlockResponse Core.Int
deleteWorkerBlockResponse_httpStatus = Lens.lens (\DeleteWorkerBlockResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkerBlockResponse' {} a -> s {httpStatus = a} :: DeleteWorkerBlockResponse)

instance Core.NFData DeleteWorkerBlockResponse
