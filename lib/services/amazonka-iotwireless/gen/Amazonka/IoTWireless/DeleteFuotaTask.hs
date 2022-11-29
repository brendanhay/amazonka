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
-- Module      : Amazonka.IoTWireless.DeleteFuotaTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a FUOTA task.
module Amazonka.IoTWireless.DeleteFuotaTask
  ( -- * Creating a Request
    DeleteFuotaTask (..),
    newDeleteFuotaTask,

    -- * Request Lenses
    deleteFuotaTask_id,

    -- * Destructuring the Response
    DeleteFuotaTaskResponse (..),
    newDeleteFuotaTaskResponse,

    -- * Response Lenses
    deleteFuotaTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFuotaTask' smart constructor.
data DeleteFuotaTask = DeleteFuotaTask'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFuotaTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteFuotaTask_id' - Undocumented member.
newDeleteFuotaTask ::
  -- | 'id'
  Prelude.Text ->
  DeleteFuotaTask
newDeleteFuotaTask pId_ = DeleteFuotaTask' {id = pId_}

-- | Undocumented member.
deleteFuotaTask_id :: Lens.Lens' DeleteFuotaTask Prelude.Text
deleteFuotaTask_id = Lens.lens (\DeleteFuotaTask' {id} -> id) (\s@DeleteFuotaTask' {} a -> s {id = a} :: DeleteFuotaTask)

instance Core.AWSRequest DeleteFuotaTask where
  type
    AWSResponse DeleteFuotaTask =
      DeleteFuotaTaskResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFuotaTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFuotaTask where
  hashWithSalt _salt DeleteFuotaTask' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteFuotaTask where
  rnf DeleteFuotaTask' {..} = Prelude.rnf id

instance Core.ToHeaders DeleteFuotaTask where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteFuotaTask where
  toPath DeleteFuotaTask' {..} =
    Prelude.mconcat ["/fuota-tasks/", Core.toBS id]

instance Core.ToQuery DeleteFuotaTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFuotaTaskResponse' smart constructor.
data DeleteFuotaTaskResponse = DeleteFuotaTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFuotaTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFuotaTaskResponse_httpStatus' - The response's http status code.
newDeleteFuotaTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFuotaTaskResponse
newDeleteFuotaTaskResponse pHttpStatus_ =
  DeleteFuotaTaskResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteFuotaTaskResponse_httpStatus :: Lens.Lens' DeleteFuotaTaskResponse Prelude.Int
deleteFuotaTaskResponse_httpStatus = Lens.lens (\DeleteFuotaTaskResponse' {httpStatus} -> httpStatus) (\s@DeleteFuotaTaskResponse' {} a -> s {httpStatus = a} :: DeleteFuotaTaskResponse)

instance Prelude.NFData DeleteFuotaTaskResponse where
  rnf DeleteFuotaTaskResponse' {..} =
    Prelude.rnf httpStatus
