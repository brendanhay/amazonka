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
-- Module      : Amazonka.IoTWireless.DeleteWirelessGatewayTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a wireless gateway task.
module Amazonka.IoTWireless.DeleteWirelessGatewayTask
  ( -- * Creating a Request
    DeleteWirelessGatewayTask (..),
    newDeleteWirelessGatewayTask,

    -- * Request Lenses
    deleteWirelessGatewayTask_id,

    -- * Destructuring the Response
    DeleteWirelessGatewayTaskResponse (..),
    newDeleteWirelessGatewayTaskResponse,

    -- * Response Lenses
    deleteWirelessGatewayTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWirelessGatewayTask' smart constructor.
data DeleteWirelessGatewayTask = DeleteWirelessGatewayTask'
  { -- | The ID of the resource to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWirelessGatewayTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteWirelessGatewayTask_id' - The ID of the resource to delete.
newDeleteWirelessGatewayTask ::
  -- | 'id'
  Prelude.Text ->
  DeleteWirelessGatewayTask
newDeleteWirelessGatewayTask pId_ =
  DeleteWirelessGatewayTask' {id = pId_}

-- | The ID of the resource to delete.
deleteWirelessGatewayTask_id :: Lens.Lens' DeleteWirelessGatewayTask Prelude.Text
deleteWirelessGatewayTask_id = Lens.lens (\DeleteWirelessGatewayTask' {id} -> id) (\s@DeleteWirelessGatewayTask' {} a -> s {id = a} :: DeleteWirelessGatewayTask)

instance Core.AWSRequest DeleteWirelessGatewayTask where
  type
    AWSResponse DeleteWirelessGatewayTask =
      DeleteWirelessGatewayTaskResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWirelessGatewayTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWirelessGatewayTask where
  hashWithSalt _salt DeleteWirelessGatewayTask' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteWirelessGatewayTask where
  rnf DeleteWirelessGatewayTask' {..} = Prelude.rnf id

instance Core.ToHeaders DeleteWirelessGatewayTask where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteWirelessGatewayTask where
  toPath DeleteWirelessGatewayTask' {..} =
    Prelude.mconcat
      ["/wireless-gateways/", Core.toBS id, "/tasks"]

instance Core.ToQuery DeleteWirelessGatewayTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWirelessGatewayTaskResponse' smart constructor.
data DeleteWirelessGatewayTaskResponse = DeleteWirelessGatewayTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWirelessGatewayTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWirelessGatewayTaskResponse_httpStatus' - The response's http status code.
newDeleteWirelessGatewayTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWirelessGatewayTaskResponse
newDeleteWirelessGatewayTaskResponse pHttpStatus_ =
  DeleteWirelessGatewayTaskResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteWirelessGatewayTaskResponse_httpStatus :: Lens.Lens' DeleteWirelessGatewayTaskResponse Prelude.Int
deleteWirelessGatewayTaskResponse_httpStatus = Lens.lens (\DeleteWirelessGatewayTaskResponse' {httpStatus} -> httpStatus) (\s@DeleteWirelessGatewayTaskResponse' {} a -> s {httpStatus = a} :: DeleteWirelessGatewayTaskResponse)

instance
  Prelude.NFData
    DeleteWirelessGatewayTaskResponse
  where
  rnf DeleteWirelessGatewayTaskResponse' {..} =
    Prelude.rnf httpStatus
