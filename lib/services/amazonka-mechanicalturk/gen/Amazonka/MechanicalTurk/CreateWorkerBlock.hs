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
-- Module      : Amazonka.MechanicalTurk.CreateWorkerBlock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateWorkerBlock@ operation allows you to prevent a Worker from
-- working on your HITs. For example, you can block a Worker who is
-- producing poor quality work. You can block up to 100,000 Workers.
module Amazonka.MechanicalTurk.CreateWorkerBlock
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWorkerBlock' smart constructor.
data CreateWorkerBlock = CreateWorkerBlock'
  { -- | The ID of the Worker to block.
    workerId :: Prelude.Text,
    -- | A message explaining the reason for blocking the Worker. This parameter
    -- enables you to keep track of your Workers. The Worker does not see this
    -- message.
    reason :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest CreateWorkerBlock where
  type
    AWSResponse CreateWorkerBlock =
      CreateWorkerBlockResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateWorkerBlockResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorkerBlock where
  hashWithSalt _salt CreateWorkerBlock' {..} =
    _salt
      `Prelude.hashWithSalt` workerId
      `Prelude.hashWithSalt` reason

instance Prelude.NFData CreateWorkerBlock where
  rnf CreateWorkerBlock' {..} =
    Prelude.rnf workerId
      `Prelude.seq` Prelude.rnf reason

instance Data.ToHeaders CreateWorkerBlock where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.CreateWorkerBlock" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorkerBlock where
  toJSON CreateWorkerBlock' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("WorkerId" Data..= workerId),
            Prelude.Just ("Reason" Data..= reason)
          ]
      )

instance Data.ToPath CreateWorkerBlock where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateWorkerBlock where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkerBlockResponse' smart constructor.
data CreateWorkerBlockResponse = CreateWorkerBlockResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateWorkerBlockResponse where
  rnf CreateWorkerBlockResponse' {..} =
    Prelude.rnf httpStatus
