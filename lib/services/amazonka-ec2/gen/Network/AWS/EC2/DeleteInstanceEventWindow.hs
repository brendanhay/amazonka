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
-- Module      : Network.AWS.EC2.DeleteInstanceEventWindow
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.EC2.DeleteInstanceEventWindow
  ( -- * Creating a Request
    DeleteInstanceEventWindow (..),
    newDeleteInstanceEventWindow,

    -- * Request Lenses
    deleteInstanceEventWindow_forceDelete,
    deleteInstanceEventWindow_dryRun,
    deleteInstanceEventWindow_instanceEventWindowId,

    -- * Destructuring the Response
    DeleteInstanceEventWindowResponse (..),
    newDeleteInstanceEventWindowResponse,

    -- * Response Lenses
    deleteInstanceEventWindowResponse_instanceEventWindowState,
    deleteInstanceEventWindowResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteInstanceEventWindow' smart constructor.
data DeleteInstanceEventWindow = DeleteInstanceEventWindow'
  { -- | Specify @true@ to force delete the event window. Use the force delete
    -- parameter if the event window is currently associated with targets.
    forceDelete :: Prelude.Maybe Prelude.Bool,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
-- 'forceDelete', 'deleteInstanceEventWindow_forceDelete' - Specify @true@ to force delete the event window. Use the force delete
-- parameter if the event window is currently associated with targets.
--
-- 'dryRun', 'deleteInstanceEventWindow_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceEventWindowId', 'deleteInstanceEventWindow_instanceEventWindowId' - The ID of the event window.
newDeleteInstanceEventWindow ::
  -- | 'instanceEventWindowId'
  Prelude.Text ->
  DeleteInstanceEventWindow
newDeleteInstanceEventWindow pInstanceEventWindowId_ =
  DeleteInstanceEventWindow'
    { forceDelete =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      instanceEventWindowId = pInstanceEventWindowId_
    }

-- | Specify @true@ to force delete the event window. Use the force delete
-- parameter if the event window is currently associated with targets.
deleteInstanceEventWindow_forceDelete :: Lens.Lens' DeleteInstanceEventWindow (Prelude.Maybe Prelude.Bool)
deleteInstanceEventWindow_forceDelete = Lens.lens (\DeleteInstanceEventWindow' {forceDelete} -> forceDelete) (\s@DeleteInstanceEventWindow' {} a -> s {forceDelete = a} :: DeleteInstanceEventWindow)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteInstanceEventWindow_dryRun :: Lens.Lens' DeleteInstanceEventWindow (Prelude.Maybe Prelude.Bool)
deleteInstanceEventWindow_dryRun = Lens.lens (\DeleteInstanceEventWindow' {dryRun} -> dryRun) (\s@DeleteInstanceEventWindow' {} a -> s {dryRun = a} :: DeleteInstanceEventWindow)

-- | The ID of the event window.
deleteInstanceEventWindow_instanceEventWindowId :: Lens.Lens' DeleteInstanceEventWindow Prelude.Text
deleteInstanceEventWindow_instanceEventWindowId = Lens.lens (\DeleteInstanceEventWindow' {instanceEventWindowId} -> instanceEventWindowId) (\s@DeleteInstanceEventWindow' {} a -> s {instanceEventWindowId = a} :: DeleteInstanceEventWindow)

instance Core.AWSRequest DeleteInstanceEventWindow where
  type
    AWSResponse DeleteInstanceEventWindow =
      DeleteInstanceEventWindowResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteInstanceEventWindowResponse'
            Prelude.<$> (x Core..@? "instanceEventWindowState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteInstanceEventWindow

instance Prelude.NFData DeleteInstanceEventWindow

instance Core.ToHeaders DeleteInstanceEventWindow where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteInstanceEventWindow where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteInstanceEventWindow where
  toQuery DeleteInstanceEventWindow' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteInstanceEventWindow" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "ForceDelete" Core.=: forceDelete,
        "DryRun" Core.=: dryRun,
        "InstanceEventWindowId"
          Core.=: instanceEventWindowId
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
