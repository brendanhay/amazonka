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
-- Module      : Amazonka.SSM.DeregisterTargetFromMaintenanceWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a target from a maintenance window.
module Amazonka.SSM.DeregisterTargetFromMaintenanceWindow
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
    deregisterTargetFromMaintenanceWindowResponse_windowId,
    deregisterTargetFromMaintenanceWindowResponse_windowTargetId,
    deregisterTargetFromMaintenanceWindowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDeregisterTargetFromMaintenanceWindow' smart constructor.
data DeregisterTargetFromMaintenanceWindow = DeregisterTargetFromMaintenanceWindow'
  { -- | The system checks if the target is being referenced by a task. If the
    -- target is being referenced, the system returns an error and doesn\'t
    -- deregister the target from the maintenance window.
    safe :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the maintenance window the target should be removed from.
    windowId :: Prelude.Text,
    -- | The ID of the target definition to remove.
    windowTargetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterTargetFromMaintenanceWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'safe', 'deregisterTargetFromMaintenanceWindow_safe' - The system checks if the target is being referenced by a task. If the
-- target is being referenced, the system returns an error and doesn\'t
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
-- target is being referenced, the system returns an error and doesn\'t
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
  Core.AWSRequest
    DeregisterTargetFromMaintenanceWindow
  where
  type
    AWSResponse
      DeregisterTargetFromMaintenanceWindow =
      DeregisterTargetFromMaintenanceWindowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterTargetFromMaintenanceWindowResponse'
            Prelude.<$> (x Data..?> "WindowId")
            Prelude.<*> (x Data..?> "WindowTargetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterTargetFromMaintenanceWindow
  where
  hashWithSalt
    _salt
    DeregisterTargetFromMaintenanceWindow' {..} =
      _salt
        `Prelude.hashWithSalt` safe
        `Prelude.hashWithSalt` windowId
        `Prelude.hashWithSalt` windowTargetId

instance
  Prelude.NFData
    DeregisterTargetFromMaintenanceWindow
  where
  rnf DeregisterTargetFromMaintenanceWindow' {..} =
    Prelude.rnf safe
      `Prelude.seq` Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf windowTargetId

instance
  Data.ToHeaders
    DeregisterTargetFromMaintenanceWindow
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DeregisterTargetFromMaintenanceWindow" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DeregisterTargetFromMaintenanceWindow
  where
  toJSON DeregisterTargetFromMaintenanceWindow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Safe" Data..=) Prelude.<$> safe,
            Prelude.Just ("WindowId" Data..= windowId),
            Prelude.Just
              ("WindowTargetId" Data..= windowTargetId)
          ]
      )

instance
  Data.ToPath
    DeregisterTargetFromMaintenanceWindow
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeregisterTargetFromMaintenanceWindow
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterTargetFromMaintenanceWindowResponse' smart constructor.
data DeregisterTargetFromMaintenanceWindowResponse = DeregisterTargetFromMaintenanceWindowResponse'
  { -- | The ID of the maintenance window the target was removed from.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the removed target definition.
    windowTargetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterTargetFromMaintenanceWindowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowId', 'deregisterTargetFromMaintenanceWindowResponse_windowId' - The ID of the maintenance window the target was removed from.
--
-- 'windowTargetId', 'deregisterTargetFromMaintenanceWindowResponse_windowTargetId' - The ID of the removed target definition.
--
-- 'httpStatus', 'deregisterTargetFromMaintenanceWindowResponse_httpStatus' - The response's http status code.
newDeregisterTargetFromMaintenanceWindowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterTargetFromMaintenanceWindowResponse
newDeregisterTargetFromMaintenanceWindowResponse
  pHttpStatus_ =
    DeregisterTargetFromMaintenanceWindowResponse'
      { windowId =
          Prelude.Nothing,
        windowTargetId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the maintenance window the target was removed from.
deregisterTargetFromMaintenanceWindowResponse_windowId :: Lens.Lens' DeregisterTargetFromMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
deregisterTargetFromMaintenanceWindowResponse_windowId = Lens.lens (\DeregisterTargetFromMaintenanceWindowResponse' {windowId} -> windowId) (\s@DeregisterTargetFromMaintenanceWindowResponse' {} a -> s {windowId = a} :: DeregisterTargetFromMaintenanceWindowResponse)

-- | The ID of the removed target definition.
deregisterTargetFromMaintenanceWindowResponse_windowTargetId :: Lens.Lens' DeregisterTargetFromMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
deregisterTargetFromMaintenanceWindowResponse_windowTargetId = Lens.lens (\DeregisterTargetFromMaintenanceWindowResponse' {windowTargetId} -> windowTargetId) (\s@DeregisterTargetFromMaintenanceWindowResponse' {} a -> s {windowTargetId = a} :: DeregisterTargetFromMaintenanceWindowResponse)

-- | The response's http status code.
deregisterTargetFromMaintenanceWindowResponse_httpStatus :: Lens.Lens' DeregisterTargetFromMaintenanceWindowResponse Prelude.Int
deregisterTargetFromMaintenanceWindowResponse_httpStatus = Lens.lens (\DeregisterTargetFromMaintenanceWindowResponse' {httpStatus} -> httpStatus) (\s@DeregisterTargetFromMaintenanceWindowResponse' {} a -> s {httpStatus = a} :: DeregisterTargetFromMaintenanceWindowResponse)

instance
  Prelude.NFData
    DeregisterTargetFromMaintenanceWindowResponse
  where
  rnf
    DeregisterTargetFromMaintenanceWindowResponse' {..} =
      Prelude.rnf windowId
        `Prelude.seq` Prelude.rnf windowTargetId
        `Prelude.seq` Prelude.rnf httpStatus
