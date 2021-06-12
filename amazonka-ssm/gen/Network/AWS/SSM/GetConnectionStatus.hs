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
-- Module      : Network.AWS.SSM.GetConnectionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Session Manager connection status for an instance to
-- determine whether it is running and ready to receive Session Manager
-- connections.
module Network.AWS.SSM.GetConnectionStatus
  ( -- * Creating a Request
    GetConnectionStatus (..),
    newGetConnectionStatus,

    -- * Request Lenses
    getConnectionStatus_target,

    -- * Destructuring the Response
    GetConnectionStatusResponse (..),
    newGetConnectionStatusResponse,

    -- * Response Lenses
    getConnectionStatusResponse_status,
    getConnectionStatusResponse_target,
    getConnectionStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetConnectionStatus' smart constructor.
data GetConnectionStatus = GetConnectionStatus'
  { -- | The ID of the instance.
    target :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConnectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'target', 'getConnectionStatus_target' - The ID of the instance.
newGetConnectionStatus ::
  -- | 'target'
  Core.Text ->
  GetConnectionStatus
newGetConnectionStatus pTarget_ =
  GetConnectionStatus' {target = pTarget_}

-- | The ID of the instance.
getConnectionStatus_target :: Lens.Lens' GetConnectionStatus Core.Text
getConnectionStatus_target = Lens.lens (\GetConnectionStatus' {target} -> target) (\s@GetConnectionStatus' {} a -> s {target = a} :: GetConnectionStatus)

instance Core.AWSRequest GetConnectionStatus where
  type
    AWSResponse GetConnectionStatus =
      GetConnectionStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectionStatusResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "Target")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetConnectionStatus

instance Core.NFData GetConnectionStatus

instance Core.ToHeaders GetConnectionStatus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.GetConnectionStatus" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetConnectionStatus where
  toJSON GetConnectionStatus' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Target" Core..= target)]
      )

instance Core.ToPath GetConnectionStatus where
  toPath = Core.const "/"

instance Core.ToQuery GetConnectionStatus where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetConnectionStatusResponse' smart constructor.
data GetConnectionStatusResponse = GetConnectionStatusResponse'
  { -- | The status of the connection to the instance. For example, \'Connected\'
    -- or \'Not Connected\'.
    status :: Core.Maybe ConnectionStatus,
    -- | The ID of the instance to check connection status.
    target :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConnectionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getConnectionStatusResponse_status' - The status of the connection to the instance. For example, \'Connected\'
-- or \'Not Connected\'.
--
-- 'target', 'getConnectionStatusResponse_target' - The ID of the instance to check connection status.
--
-- 'httpStatus', 'getConnectionStatusResponse_httpStatus' - The response's http status code.
newGetConnectionStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetConnectionStatusResponse
newGetConnectionStatusResponse pHttpStatus_ =
  GetConnectionStatusResponse'
    { status = Core.Nothing,
      target = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the connection to the instance. For example, \'Connected\'
-- or \'Not Connected\'.
getConnectionStatusResponse_status :: Lens.Lens' GetConnectionStatusResponse (Core.Maybe ConnectionStatus)
getConnectionStatusResponse_status = Lens.lens (\GetConnectionStatusResponse' {status} -> status) (\s@GetConnectionStatusResponse' {} a -> s {status = a} :: GetConnectionStatusResponse)

-- | The ID of the instance to check connection status.
getConnectionStatusResponse_target :: Lens.Lens' GetConnectionStatusResponse (Core.Maybe Core.Text)
getConnectionStatusResponse_target = Lens.lens (\GetConnectionStatusResponse' {target} -> target) (\s@GetConnectionStatusResponse' {} a -> s {target = a} :: GetConnectionStatusResponse)

-- | The response's http status code.
getConnectionStatusResponse_httpStatus :: Lens.Lens' GetConnectionStatusResponse Core.Int
getConnectionStatusResponse_httpStatus = Lens.lens (\GetConnectionStatusResponse' {httpStatus} -> httpStatus) (\s@GetConnectionStatusResponse' {} a -> s {httpStatus = a} :: GetConnectionStatusResponse)

instance Core.NFData GetConnectionStatusResponse
