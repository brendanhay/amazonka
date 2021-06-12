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
-- Module      : Network.AWS.DeviceFarm.DeleteRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the run, given the run ARN.
--
-- Deleting this resource does not stop an in-progress run.
module Network.AWS.DeviceFarm.DeleteRun
  ( -- * Creating a Request
    DeleteRun (..),
    newDeleteRun,

    -- * Request Lenses
    deleteRun_arn,

    -- * Destructuring the Response
    DeleteRunResponse (..),
    newDeleteRunResponse,

    -- * Response Lenses
    deleteRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the delete run operation.
--
-- /See:/ 'newDeleteRun' smart constructor.
data DeleteRun = DeleteRun'
  { -- | The Amazon Resource Name (ARN) for the run to delete.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteRun_arn' - The Amazon Resource Name (ARN) for the run to delete.
newDeleteRun ::
  -- | 'arn'
  Core.Text ->
  DeleteRun
newDeleteRun pArn_ = DeleteRun' {arn = pArn_}

-- | The Amazon Resource Name (ARN) for the run to delete.
deleteRun_arn :: Lens.Lens' DeleteRun Core.Text
deleteRun_arn = Lens.lens (\DeleteRun' {arn} -> arn) (\s@DeleteRun' {} a -> s {arn = a} :: DeleteRun)

instance Core.AWSRequest DeleteRun where
  type AWSResponse DeleteRun = DeleteRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRunResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteRun

instance Core.NFData DeleteRun

instance Core.ToHeaders DeleteRun where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DeviceFarm_20150623.DeleteRun" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteRun where
  toJSON DeleteRun' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath DeleteRun where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRun where
  toQuery = Core.const Core.mempty

-- | Represents the result of a delete run request.
--
-- /See:/ 'newDeleteRunResponse' smart constructor.
data DeleteRunResponse = DeleteRunResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRunResponse_httpStatus' - The response's http status code.
newDeleteRunResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteRunResponse
newDeleteRunResponse pHttpStatus_ =
  DeleteRunResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteRunResponse_httpStatus :: Lens.Lens' DeleteRunResponse Core.Int
deleteRunResponse_httpStatus = Lens.lens (\DeleteRunResponse' {httpStatus} -> httpStatus) (\s@DeleteRunResponse' {} a -> s {httpStatus = a} :: DeleteRunResponse)

instance Core.NFData DeleteRunResponse
