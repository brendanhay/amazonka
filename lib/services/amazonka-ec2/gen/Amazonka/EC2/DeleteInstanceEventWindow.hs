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
-- Module      : Amazonka.EC2.DeleteInstanceEventWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified event window.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/event-windows.html Define event windows for scheduled events>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.DeleteInstanceEventWindow
  ( -- * Creating a Request
    DeleteInstanceEventWindow (..),
    newDeleteInstanceEventWindow,

    -- * Request Lenses
    deleteInstanceEventWindow_dryRun,
    deleteInstanceEventWindow_forceDelete,
    deleteInstanceEventWindow_instanceEventWindowId,

    -- * Destructuring the Response
    DeleteInstanceEventWindowResponse (..),
    newDeleteInstanceEventWindowResponse,

    -- * Response Lenses
    deleteInstanceEventWindowResponse_instanceEventWindowState,
    deleteInstanceEventWindowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteInstanceEventWindow' smart constructor.
data DeleteInstanceEventWindow = DeleteInstanceEventWindow'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Specify @true@ to force delete the event window. Use the force delete
    -- parameter if the event window is currently associated with targets.
    forceDelete :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the event window.
    instanceEventWindowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstanceEventWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteInstanceEventWindow_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'forceDelete', 'deleteInstanceEventWindow_forceDelete' - Specify @true@ to force delete the event window. Use the force delete
-- parameter if the event window is currently associated with targets.
--
-- 'instanceEventWindowId', 'deleteInstanceEventWindow_instanceEventWindowId' - The ID of the event window.
newDeleteInstanceEventWindow ::
  -- | 'instanceEventWindowId'
  Prelude.Text ->
  DeleteInstanceEventWindow
newDeleteInstanceEventWindow pInstanceEventWindowId_ =
  DeleteInstanceEventWindow'
    { dryRun =
        Prelude.Nothing,
      forceDelete = Prelude.Nothing,
      instanceEventWindowId = pInstanceEventWindowId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteInstanceEventWindow_dryRun :: Lens.Lens' DeleteInstanceEventWindow (Prelude.Maybe Prelude.Bool)
deleteInstanceEventWindow_dryRun = Lens.lens (\DeleteInstanceEventWindow' {dryRun} -> dryRun) (\s@DeleteInstanceEventWindow' {} a -> s {dryRun = a} :: DeleteInstanceEventWindow)

-- | Specify @true@ to force delete the event window. Use the force delete
-- parameter if the event window is currently associated with targets.
deleteInstanceEventWindow_forceDelete :: Lens.Lens' DeleteInstanceEventWindow (Prelude.Maybe Prelude.Bool)
deleteInstanceEventWindow_forceDelete = Lens.lens (\DeleteInstanceEventWindow' {forceDelete} -> forceDelete) (\s@DeleteInstanceEventWindow' {} a -> s {forceDelete = a} :: DeleteInstanceEventWindow)

-- | The ID of the event window.
deleteInstanceEventWindow_instanceEventWindowId :: Lens.Lens' DeleteInstanceEventWindow Prelude.Text
deleteInstanceEventWindow_instanceEventWindowId = Lens.lens (\DeleteInstanceEventWindow' {instanceEventWindowId} -> instanceEventWindowId) (\s@DeleteInstanceEventWindow' {} a -> s {instanceEventWindowId = a} :: DeleteInstanceEventWindow)

instance Core.AWSRequest DeleteInstanceEventWindow where
  type
    AWSResponse DeleteInstanceEventWindow =
      DeleteInstanceEventWindowResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteInstanceEventWindowResponse'
            Prelude.<$> (x Data..@? "instanceEventWindowState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteInstanceEventWindow where
  hashWithSalt _salt DeleteInstanceEventWindow' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` forceDelete
      `Prelude.hashWithSalt` instanceEventWindowId

instance Prelude.NFData DeleteInstanceEventWindow where
  rnf DeleteInstanceEventWindow' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf forceDelete
      `Prelude.seq` Prelude.rnf instanceEventWindowId

instance Data.ToHeaders DeleteInstanceEventWindow where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteInstanceEventWindow where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteInstanceEventWindow where
  toQuery DeleteInstanceEventWindow' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteInstanceEventWindow" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "ForceDelete" Data.=: forceDelete,
        "InstanceEventWindowId"
          Data.=: instanceEventWindowId
      ]

-- | /See:/ 'newDeleteInstanceEventWindowResponse' smart constructor.
data DeleteInstanceEventWindowResponse = DeleteInstanceEventWindowResponse'
  { -- | The state of the event window.
    instanceEventWindowState :: Prelude.Maybe InstanceEventWindowStateChange,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstanceEventWindowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceEventWindowState', 'deleteInstanceEventWindowResponse_instanceEventWindowState' - The state of the event window.
--
-- 'httpStatus', 'deleteInstanceEventWindowResponse_httpStatus' - The response's http status code.
newDeleteInstanceEventWindowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteInstanceEventWindowResponse
newDeleteInstanceEventWindowResponse pHttpStatus_ =
  DeleteInstanceEventWindowResponse'
    { instanceEventWindowState =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state of the event window.
deleteInstanceEventWindowResponse_instanceEventWindowState :: Lens.Lens' DeleteInstanceEventWindowResponse (Prelude.Maybe InstanceEventWindowStateChange)
deleteInstanceEventWindowResponse_instanceEventWindowState = Lens.lens (\DeleteInstanceEventWindowResponse' {instanceEventWindowState} -> instanceEventWindowState) (\s@DeleteInstanceEventWindowResponse' {} a -> s {instanceEventWindowState = a} :: DeleteInstanceEventWindowResponse)

-- | The response's http status code.
deleteInstanceEventWindowResponse_httpStatus :: Lens.Lens' DeleteInstanceEventWindowResponse Prelude.Int
deleteInstanceEventWindowResponse_httpStatus = Lens.lens (\DeleteInstanceEventWindowResponse' {httpStatus} -> httpStatus) (\s@DeleteInstanceEventWindowResponse' {} a -> s {httpStatus = a} :: DeleteInstanceEventWindowResponse)

instance
  Prelude.NFData
    DeleteInstanceEventWindowResponse
  where
  rnf DeleteInstanceEventWindowResponse' {..} =
    Prelude.rnf instanceEventWindowState
      `Prelude.seq` Prelude.rnf httpStatus
