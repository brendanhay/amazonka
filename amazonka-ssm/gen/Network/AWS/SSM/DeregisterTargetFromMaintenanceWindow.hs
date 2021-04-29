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
-- Module      : Network.AWS.SSM.DeregisterTargetFromMaintenanceWindow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a target from a maintenance window.
module Network.AWS.SSM.DeregisterTargetFromMaintenanceWindow
  ( -- * Creating a Request
    DeregisterTargetFromMaintenanceWindow (..),
    newDeregisterTargetFromMaintenanceWindow,

    -- * Request Lenses
    deregisterTargetFromMaintenanceWindow_safe,
    deregisterTargetFromMaintenanceWindow_windowId,
    deregisterTargetFromMaintenanceWindow_windowTargetId,

    -- * Destructuring the Response
    DeregisterTargetFromMaintenanceWindowResponse (..),
    newDeregisterTargetFromMaintenanceWindowResponse,

    -- * Response Lenses
    deregisterTargetFromMaintenanceWindowResponse_windowTargetId,
    deregisterTargetFromMaintenanceWindowResponse_windowId,
    deregisterTargetFromMaintenanceWindowResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDeregisterTargetFromMaintenanceWindow' smart constructor.
data DeregisterTargetFromMaintenanceWindow = DeregisterTargetFromMaintenanceWindow'
  { -- | The system checks if the target is being referenced by a task. If the
    -- target is being referenced, the system returns an error and does not
    -- deregister the target from the maintenance window.
    safe :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the maintenance window the target should be removed from.
    windowId :: Prelude.Text,
    -- | The ID of the target definition to remove.
    windowTargetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterTargetFromMaintenanceWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'safe', 'deregisterTargetFromMaintenanceWindow_safe' - The system checks if the target is being referenced by a task. If the
-- target is being referenced, the system returns an error and does not
-- deregister the target from the maintenance window.
--
-- 'windowId', 'deregisterTargetFromMaintenanceWindow_windowId' - The ID of the maintenance window the target should be removed from.
--
-- 'windowTargetId', 'deregisterTargetFromMaintenanceWindow_windowTargetId' - The ID of the target definition to remove.
newDeregisterTargetFromMaintenanceWindow ::
  -- | 'windowId'
  Prelude.Text ->
  -- | 'windowTargetId'
  Prelude.Text ->
  DeregisterTargetFromMaintenanceWindow
newDeregisterTargetFromMaintenanceWindow
  pWindowId_
  pWindowTargetId_ =
    DeregisterTargetFromMaintenanceWindow'
      { safe =
          Prelude.Nothing,
        windowId = pWindowId_,
        windowTargetId = pWindowTargetId_
      }

-- | The system checks if the target is being referenced by a task. If the
-- target is being referenced, the system returns an error and does not
-- deregister the target from the maintenance window.
deregisterTargetFromMaintenanceWindow_safe :: Lens.Lens' DeregisterTargetFromMaintenanceWindow (Prelude.Maybe Prelude.Bool)
deregisterTargetFromMaintenanceWindow_safe = Lens.lens (\DeregisterTargetFromMaintenanceWindow' {safe} -> safe) (\s@DeregisterTargetFromMaintenanceWindow' {} a -> s {safe = a} :: DeregisterTargetFromMaintenanceWindow)

-- | The ID of the maintenance window the target should be removed from.
deregisterTargetFromMaintenanceWindow_windowId :: Lens.Lens' DeregisterTargetFromMaintenanceWindow Prelude.Text
deregisterTargetFromMaintenanceWindow_windowId = Lens.lens (\DeregisterTargetFromMaintenanceWindow' {windowId} -> windowId) (\s@DeregisterTargetFromMaintenanceWindow' {} a -> s {windowId = a} :: DeregisterTargetFromMaintenanceWindow)

-- | The ID of the target definition to remove.
deregisterTargetFromMaintenanceWindow_windowTargetId :: Lens.Lens' DeregisterTargetFromMaintenanceWindow Prelude.Text
deregisterTargetFromMaintenanceWindow_windowTargetId = Lens.lens (\DeregisterTargetFromMaintenanceWindow' {windowTargetId} -> windowTargetId) (\s@DeregisterTargetFromMaintenanceWindow' {} a -> s {windowTargetId = a} :: DeregisterTargetFromMaintenanceWindow)

instance
  Prelude.AWSRequest
    DeregisterTargetFromMaintenanceWindow
  where
  type
    Rs DeregisterTargetFromMaintenanceWindow =
      DeregisterTargetFromMaintenanceWindowResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterTargetFromMaintenanceWindowResponse'
            Prelude.<$> (x Prelude..?> "WindowTargetId")
              Prelude.<*> (x Prelude..?> "WindowId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterTargetFromMaintenanceWindow

instance
  Prelude.NFData
    DeregisterTargetFromMaintenanceWindow

instance
  Prelude.ToHeaders
    DeregisterTargetFromMaintenanceWindow
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.DeregisterTargetFromMaintenanceWindow" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DeregisterTargetFromMaintenanceWindow
  where
  toJSON DeregisterTargetFromMaintenanceWindow' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Safe" Prelude..=) Prelude.<$> safe,
            Prelude.Just ("WindowId" Prelude..= windowId),
            Prelude.Just
              ("WindowTargetId" Prelude..= windowTargetId)
          ]
      )

instance
  Prelude.ToPath
    DeregisterTargetFromMaintenanceWindow
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeregisterTargetFromMaintenanceWindow
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterTargetFromMaintenanceWindowResponse' smart constructor.
data DeregisterTargetFromMaintenanceWindowResponse = DeregisterTargetFromMaintenanceWindowResponse'
  { -- | The ID of the removed target definition.
    windowTargetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window the target was removed from.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterTargetFromMaintenanceWindowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowTargetId', 'deregisterTargetFromMaintenanceWindowResponse_windowTargetId' - The ID of the removed target definition.
--
-- 'windowId', 'deregisterTargetFromMaintenanceWindowResponse_windowId' - The ID of the maintenance window the target was removed from.
--
-- 'httpStatus', 'deregisterTargetFromMaintenanceWindowResponse_httpStatus' - The response's http status code.
newDeregisterTargetFromMaintenanceWindowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterTargetFromMaintenanceWindowResponse
newDeregisterTargetFromMaintenanceWindowResponse
  pHttpStatus_ =
    DeregisterTargetFromMaintenanceWindowResponse'
      { windowTargetId =
          Prelude.Nothing,
        windowId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the removed target definition.
deregisterTargetFromMaintenanceWindowResponse_windowTargetId :: Lens.Lens' DeregisterTargetFromMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
deregisterTargetFromMaintenanceWindowResponse_windowTargetId = Lens.lens (\DeregisterTargetFromMaintenanceWindowResponse' {windowTargetId} -> windowTargetId) (\s@DeregisterTargetFromMaintenanceWindowResponse' {} a -> s {windowTargetId = a} :: DeregisterTargetFromMaintenanceWindowResponse)

-- | The ID of the maintenance window the target was removed from.
deregisterTargetFromMaintenanceWindowResponse_windowId :: Lens.Lens' DeregisterTargetFromMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
deregisterTargetFromMaintenanceWindowResponse_windowId = Lens.lens (\DeregisterTargetFromMaintenanceWindowResponse' {windowId} -> windowId) (\s@DeregisterTargetFromMaintenanceWindowResponse' {} a -> s {windowId = a} :: DeregisterTargetFromMaintenanceWindowResponse)

-- | The response's http status code.
deregisterTargetFromMaintenanceWindowResponse_httpStatus :: Lens.Lens' DeregisterTargetFromMaintenanceWindowResponse Prelude.Int
deregisterTargetFromMaintenanceWindowResponse_httpStatus = Lens.lens (\DeregisterTargetFromMaintenanceWindowResponse' {httpStatus} -> httpStatus) (\s@DeregisterTargetFromMaintenanceWindowResponse' {} a -> s {httpStatus = a} :: DeregisterTargetFromMaintenanceWindowResponse)

instance
  Prelude.NFData
    DeregisterTargetFromMaintenanceWindowResponse
