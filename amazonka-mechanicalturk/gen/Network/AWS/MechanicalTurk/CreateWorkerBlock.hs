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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateWorkerBlock' smart constructor.
data CreateWorkerBlock = CreateWorkerBlock'
  { -- | The ID of the Worker to block.
    workerId :: Prelude.Text,
    -- | A message explaining the reason for blocking the Worker. This parameter
    -- enables you to keep track of your Workers. The Worker does not see this
    -- message.
    reason :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'reason'
  Prelude.Text ->
  CreateWorkerBlock
newCreateWorkerBlock pWorkerId_ pReason_ =
  CreateWorkerBlock'
    { workerId = pWorkerId_,
      reason = pReason_
    }

-- | The ID of the Worker to block.
createWorkerBlock_workerId :: Lens.Lens' CreateWorkerBlock Prelude.Text
createWorkerBlock_workerId = Lens.lens (\CreateWorkerBlock' {workerId} -> workerId) (\s@CreateWorkerBlock' {} a -> s {workerId = a} :: CreateWorkerBlock)

-- | A message explaining the reason for blocking the Worker. This parameter
-- enables you to keep track of your Workers. The Worker does not see this
-- message.
createWorkerBlock_reason :: Lens.Lens' CreateWorkerBlock Prelude.Text
createWorkerBlock_reason = Lens.lens (\CreateWorkerBlock' {reason} -> reason) (\s@CreateWorkerBlock' {} a -> s {reason = a} :: CreateWorkerBlock)

instance Prelude.AWSRequest CreateWorkerBlock where
  type Rs CreateWorkerBlock = CreateWorkerBlockResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateWorkerBlockResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorkerBlock

instance Prelude.NFData CreateWorkerBlock

instance Prelude.ToHeaders CreateWorkerBlock where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MTurkRequesterServiceV20170117.CreateWorkerBlock" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateWorkerBlock where
  toJSON CreateWorkerBlock' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("WorkerId" Prelude..= workerId),
            Prelude.Just ("Reason" Prelude..= reason)
          ]
      )

instance Prelude.ToPath CreateWorkerBlock where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateWorkerBlock where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkerBlockResponse' smart constructor.
data CreateWorkerBlockResponse = CreateWorkerBlockResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateWorkerBlockResponse
newCreateWorkerBlockResponse pHttpStatus_ =
  CreateWorkerBlockResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createWorkerBlockResponse_httpStatus :: Lens.Lens' CreateWorkerBlockResponse Prelude.Int
createWorkerBlockResponse_httpStatus = Lens.lens (\CreateWorkerBlockResponse' {httpStatus} -> httpStatus) (\s@CreateWorkerBlockResponse' {} a -> s {httpStatus = a} :: CreateWorkerBlockResponse)

instance Prelude.NFData CreateWorkerBlockResponse
