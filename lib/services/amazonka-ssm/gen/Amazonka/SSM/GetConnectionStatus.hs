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
-- Module      : Amazonka.SSM.GetConnectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Session Manager connection status for a managed node to
-- determine whether it is running and ready to receive Session Manager
-- connections.
module Amazonka.SSM.GetConnectionStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetConnectionStatus' smart constructor.
data GetConnectionStatus = GetConnectionStatus'
  { -- | The managed node ID.
    target :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'target', 'getConnectionStatus_target' - The managed node ID.
newGetConnectionStatus ::
  -- | 'target'
  Prelude.Text ->
  GetConnectionStatus
newGetConnectionStatus pTarget_ =
  GetConnectionStatus' {target = pTarget_}

-- | The managed node ID.
getConnectionStatus_target :: Lens.Lens' GetConnectionStatus Prelude.Text
getConnectionStatus_target = Lens.lens (\GetConnectionStatus' {target} -> target) (\s@GetConnectionStatus' {} a -> s {target = a} :: GetConnectionStatus)

instance Core.AWSRequest GetConnectionStatus where
  type
    AWSResponse GetConnectionStatus =
      GetConnectionStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectionStatusResponse'
            Prelude.<$> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "Target")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnectionStatus where
  hashWithSalt _salt GetConnectionStatus' {..} =
    _salt `Prelude.hashWithSalt` target

instance Prelude.NFData GetConnectionStatus where
  rnf GetConnectionStatus' {..} = Prelude.rnf target

instance Data.ToHeaders GetConnectionStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.GetConnectionStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetConnectionStatus where
  toJSON GetConnectionStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Target" Data..= target)]
      )

instance Data.ToPath GetConnectionStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetConnectionStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConnectionStatusResponse' smart constructor.
data GetConnectionStatusResponse = GetConnectionStatusResponse'
  { -- | The status of the connection to the managed node. For example,
    -- \'Connected\' or \'Not Connected\'.
    status :: Prelude.Maybe ConnectionStatus,
    -- | The ID of the managed node to check connection status.
    target :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getConnectionStatusResponse_status' - The status of the connection to the managed node. For example,
-- \'Connected\' or \'Not Connected\'.
--
-- 'target', 'getConnectionStatusResponse_target' - The ID of the managed node to check connection status.
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

-- | The status of the connection to the managed node. For example,
-- \'Connected\' or \'Not Connected\'.
getConnectionStatusResponse_status :: Lens.Lens' GetConnectionStatusResponse (Prelude.Maybe ConnectionStatus)
getConnectionStatusResponse_status = Lens.lens (\GetConnectionStatusResponse' {status} -> status) (\s@GetConnectionStatusResponse' {} a -> s {status = a} :: GetConnectionStatusResponse)

-- | The ID of the managed node to check connection status.
getConnectionStatusResponse_target :: Lens.Lens' GetConnectionStatusResponse (Prelude.Maybe Prelude.Text)
getConnectionStatusResponse_target = Lens.lens (\GetConnectionStatusResponse' {target} -> target) (\s@GetConnectionStatusResponse' {} a -> s {target = a} :: GetConnectionStatusResponse)

-- | The response's http status code.
getConnectionStatusResponse_httpStatus :: Lens.Lens' GetConnectionStatusResponse Prelude.Int
getConnectionStatusResponse_httpStatus = Lens.lens (\GetConnectionStatusResponse' {httpStatus} -> httpStatus) (\s@GetConnectionStatusResponse' {} a -> s {httpStatus = a} :: GetConnectionStatusResponse)

instance Prelude.NFData GetConnectionStatusResponse where
  rnf GetConnectionStatusResponse' {..} =
    Prelude.rnf status `Prelude.seq`
      Prelude.rnf target `Prelude.seq`
        Prelude.rnf httpStatus
