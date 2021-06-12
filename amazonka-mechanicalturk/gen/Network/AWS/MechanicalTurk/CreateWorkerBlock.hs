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
-- Module      : Network.AWS.MechanicalTurk.CreateWorkerBlock
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateWorkerBlock@ operation allows you to prevent a Worker from
-- working on your HITs. For example, you can block a Worker who is
-- producing poor quality work. You can block up to 100,000 Workers.
module Network.AWS.MechanicalTurk.CreateWorkerBlock
  ( -- * Creating a Request
    CreateWorkerBlock (..),
    newCreateWorkerBlock,

    -- * Request Lenses
    createWorkerBlock_workerId,
    createWorkerBlock_reason,

    -- * Destructuring the Response
    CreateWorkerBlockResponse (..),
    newCreateWorkerBlockResponse,

    -- * Response Lenses
    createWorkerBlockResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateWorkerBlock' smart constructor.
data CreateWorkerBlock = CreateWorkerBlock'
  { -- | The ID of the Worker to block.
    workerId :: Core.Text,
    -- | A message explaining the reason for blocking the Worker. This parameter
    -- enables you to keep track of your Workers. The Worker does not see this
    -- message.
    reason :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateWorkerBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workerId', 'createWorkerBlock_workerId' - The ID of the Worker to block.
--
-- 'reason', 'createWorkerBlock_reason' - A message explaining the reason for blocking the Worker. This parameter
-- enables you to keep track of your Workers. The Worker does not see this
-- message.
newCreateWorkerBlock ::
  -- | 'workerId'
  Core.Text ->
  -- | 'reason'
  Core.Text ->
  CreateWorkerBlock
newCreateWorkerBlock pWorkerId_ pReason_ =
  CreateWorkerBlock'
    { workerId = pWorkerId_,
      reason = pReason_
    }

-- | The ID of the Worker to block.
createWorkerBlock_workerId :: Lens.Lens' CreateWorkerBlock Core.Text
createWorkerBlock_workerId = Lens.lens (\CreateWorkerBlock' {workerId} -> workerId) (\s@CreateWorkerBlock' {} a -> s {workerId = a} :: CreateWorkerBlock)

-- | A message explaining the reason for blocking the Worker. This parameter
-- enables you to keep track of your Workers. The Worker does not see this
-- message.
createWorkerBlock_reason :: Lens.Lens' CreateWorkerBlock Core.Text
createWorkerBlock_reason = Lens.lens (\CreateWorkerBlock' {reason} -> reason) (\s@CreateWorkerBlock' {} a -> s {reason = a} :: CreateWorkerBlock)

instance Core.AWSRequest CreateWorkerBlock where
  type
    AWSResponse CreateWorkerBlock =
      CreateWorkerBlockResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateWorkerBlockResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateWorkerBlock

instance Core.NFData CreateWorkerBlock

instance Core.ToHeaders CreateWorkerBlock where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.CreateWorkerBlock" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateWorkerBlock where
  toJSON CreateWorkerBlock' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WorkerId" Core..= workerId),
            Core.Just ("Reason" Core..= reason)
          ]
      )

instance Core.ToPath CreateWorkerBlock where
  toPath = Core.const "/"

instance Core.ToQuery CreateWorkerBlock where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateWorkerBlockResponse' smart constructor.
data CreateWorkerBlockResponse = CreateWorkerBlockResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateWorkerBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createWorkerBlockResponse_httpStatus' - The response's http status code.
newCreateWorkerBlockResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateWorkerBlockResponse
newCreateWorkerBlockResponse pHttpStatus_ =
  CreateWorkerBlockResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createWorkerBlockResponse_httpStatus :: Lens.Lens' CreateWorkerBlockResponse Core.Int
createWorkerBlockResponse_httpStatus = Lens.lens (\CreateWorkerBlockResponse' {httpStatus} -> httpStatus) (\s@CreateWorkerBlockResponse' {} a -> s {httpStatus = a} :: CreateWorkerBlockResponse)

instance Core.NFData CreateWorkerBlockResponse
