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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteWorkerBlock' smart constructor.
data DeleteWorkerBlock = DeleteWorkerBlock'
  { -- | A message that explains the reason for unblocking the Worker. The Worker
    -- does not see this message.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Worker to unblock.
    workerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteWorkerBlock
newDeleteWorkerBlock pWorkerId_ =
  DeleteWorkerBlock'
    { reason = Prelude.Nothing,
      workerId = pWorkerId_
    }

-- | A message that explains the reason for unblocking the Worker. The Worker
-- does not see this message.
deleteWorkerBlock_reason :: Lens.Lens' DeleteWorkerBlock (Prelude.Maybe Prelude.Text)
deleteWorkerBlock_reason = Lens.lens (\DeleteWorkerBlock' {reason} -> reason) (\s@DeleteWorkerBlock' {} a -> s {reason = a} :: DeleteWorkerBlock)

-- | The ID of the Worker to unblock.
deleteWorkerBlock_workerId :: Lens.Lens' DeleteWorkerBlock Prelude.Text
deleteWorkerBlock_workerId = Lens.lens (\DeleteWorkerBlock' {workerId} -> workerId) (\s@DeleteWorkerBlock' {} a -> s {workerId = a} :: DeleteWorkerBlock)

instance Prelude.AWSRequest DeleteWorkerBlock where
  type Rs DeleteWorkerBlock = DeleteWorkerBlockResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkerBlockResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWorkerBlock

instance Prelude.NFData DeleteWorkerBlock

instance Prelude.ToHeaders DeleteWorkerBlock where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MTurkRequesterServiceV20170117.DeleteWorkerBlock" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteWorkerBlock where
  toJSON DeleteWorkerBlock' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Reason" Prelude..=) Prelude.<$> reason,
            Prelude.Just ("WorkerId" Prelude..= workerId)
          ]
      )

instance Prelude.ToPath DeleteWorkerBlock where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteWorkerBlock where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkerBlockResponse' smart constructor.
data DeleteWorkerBlockResponse = DeleteWorkerBlockResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteWorkerBlockResponse
newDeleteWorkerBlockResponse pHttpStatus_ =
  DeleteWorkerBlockResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteWorkerBlockResponse_httpStatus :: Lens.Lens' DeleteWorkerBlockResponse Prelude.Int
deleteWorkerBlockResponse_httpStatus = Lens.lens (\DeleteWorkerBlockResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkerBlockResponse' {} a -> s {httpStatus = a} :: DeleteWorkerBlockResponse)

instance Prelude.NFData DeleteWorkerBlockResponse
