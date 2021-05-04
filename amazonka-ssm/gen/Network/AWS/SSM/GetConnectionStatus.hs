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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetConnectionStatus' smart constructor.
data GetConnectionStatus = GetConnectionStatus'
  { -- | The ID of the instance.
    target :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetConnectionStatus
newGetConnectionStatus pTarget_ =
  GetConnectionStatus' {target = pTarget_}

-- | The ID of the instance.
getConnectionStatus_target :: Lens.Lens' GetConnectionStatus Prelude.Text
getConnectionStatus_target = Lens.lens (\GetConnectionStatus' {target} -> target) (\s@GetConnectionStatus' {} a -> s {target = a} :: GetConnectionStatus)

instance Prelude.AWSRequest GetConnectionStatus where
  type
    Rs GetConnectionStatus =
      GetConnectionStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectionStatusResponse'
            Prelude.<$> (x Prelude..?> "Status")
            Prelude.<*> (x Prelude..?> "Target")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnectionStatus

instance Prelude.NFData GetConnectionStatus

instance Prelude.ToHeaders GetConnectionStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.GetConnectionStatus" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetConnectionStatus where
  toJSON GetConnectionStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Target" Prelude..= target)]
      )

instance Prelude.ToPath GetConnectionStatus where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetConnectionStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConnectionStatusResponse' smart constructor.
data GetConnectionStatusResponse = GetConnectionStatusResponse'
  { -- | The status of the connection to the instance. For example, \'Connected\'
    -- or \'Not Connected\'.
    status :: Prelude.Maybe ConnectionStatus,
    -- | The ID of the instance to check connection status.
    target :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetConnectionStatusResponse
newGetConnectionStatusResponse pHttpStatus_ =
  GetConnectionStatusResponse'
    { status =
        Prelude.Nothing,
      target = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the connection to the instance. For example, \'Connected\'
-- or \'Not Connected\'.
getConnectionStatusResponse_status :: Lens.Lens' GetConnectionStatusResponse (Prelude.Maybe ConnectionStatus)
getConnectionStatusResponse_status = Lens.lens (\GetConnectionStatusResponse' {status} -> status) (\s@GetConnectionStatusResponse' {} a -> s {status = a} :: GetConnectionStatusResponse)

-- | The ID of the instance to check connection status.
getConnectionStatusResponse_target :: Lens.Lens' GetConnectionStatusResponse (Prelude.Maybe Prelude.Text)
getConnectionStatusResponse_target = Lens.lens (\GetConnectionStatusResponse' {target} -> target) (\s@GetConnectionStatusResponse' {} a -> s {target = a} :: GetConnectionStatusResponse)

-- | The response's http status code.
getConnectionStatusResponse_httpStatus :: Lens.Lens' GetConnectionStatusResponse Prelude.Int
getConnectionStatusResponse_httpStatus = Lens.lens (\GetConnectionStatusResponse' {httpStatus} -> httpStatus) (\s@GetConnectionStatusResponse' {} a -> s {httpStatus = a} :: GetConnectionStatusResponse)

instance Prelude.NFData GetConnectionStatusResponse
