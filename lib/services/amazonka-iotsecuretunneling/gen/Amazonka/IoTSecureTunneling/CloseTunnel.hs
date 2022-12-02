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
-- Module      : Amazonka.IoTSecureTunneling.CloseTunnel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Closes a tunnel identified by the unique tunnel id. When a @CloseTunnel@
-- request is received, we close the WebSocket connections between the
-- client and proxy server so no data can be transmitted.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CloseTunnel>
-- action.
module Amazonka.IoTSecureTunneling.CloseTunnel
  ( -- * Creating a Request
    CloseTunnel (..),
    newCloseTunnel,

    -- * Request Lenses
    closeTunnel_delete,
    closeTunnel_tunnelId,

    -- * Destructuring the Response
    CloseTunnelResponse (..),
    newCloseTunnelResponse,

    -- * Response Lenses
    closeTunnelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSecureTunneling.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCloseTunnel' smart constructor.
data CloseTunnel = CloseTunnel'
  { -- | When set to true, IoT Secure Tunneling deletes the tunnel data
    -- immediately.
    delete' :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the tunnel to close.
    tunnelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloseTunnel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delete'', 'closeTunnel_delete' - When set to true, IoT Secure Tunneling deletes the tunnel data
-- immediately.
--
-- 'tunnelId', 'closeTunnel_tunnelId' - The ID of the tunnel to close.
newCloseTunnel ::
  -- | 'tunnelId'
  Prelude.Text ->
  CloseTunnel
newCloseTunnel pTunnelId_ =
  CloseTunnel'
    { delete' = Prelude.Nothing,
      tunnelId = pTunnelId_
    }

-- | When set to true, IoT Secure Tunneling deletes the tunnel data
-- immediately.
closeTunnel_delete :: Lens.Lens' CloseTunnel (Prelude.Maybe Prelude.Bool)
closeTunnel_delete = Lens.lens (\CloseTunnel' {delete'} -> delete') (\s@CloseTunnel' {} a -> s {delete' = a} :: CloseTunnel)

-- | The ID of the tunnel to close.
closeTunnel_tunnelId :: Lens.Lens' CloseTunnel Prelude.Text
closeTunnel_tunnelId = Lens.lens (\CloseTunnel' {tunnelId} -> tunnelId) (\s@CloseTunnel' {} a -> s {tunnelId = a} :: CloseTunnel)

instance Core.AWSRequest CloseTunnel where
  type AWSResponse CloseTunnel = CloseTunnelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CloseTunnelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CloseTunnel where
  hashWithSalt _salt CloseTunnel' {..} =
    _salt `Prelude.hashWithSalt` delete'
      `Prelude.hashWithSalt` tunnelId

instance Prelude.NFData CloseTunnel where
  rnf CloseTunnel' {..} =
    Prelude.rnf delete'
      `Prelude.seq` Prelude.rnf tunnelId

instance Data.ToHeaders CloseTunnel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTSecuredTunneling.CloseTunnel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CloseTunnel where
  toJSON CloseTunnel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("delete" Data..=) Prelude.<$> delete',
            Prelude.Just ("tunnelId" Data..= tunnelId)
          ]
      )

instance Data.ToPath CloseTunnel where
  toPath = Prelude.const "/"

instance Data.ToQuery CloseTunnel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCloseTunnelResponse' smart constructor.
data CloseTunnelResponse = CloseTunnelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloseTunnelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'closeTunnelResponse_httpStatus' - The response's http status code.
newCloseTunnelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CloseTunnelResponse
newCloseTunnelResponse pHttpStatus_ =
  CloseTunnelResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
closeTunnelResponse_httpStatus :: Lens.Lens' CloseTunnelResponse Prelude.Int
closeTunnelResponse_httpStatus = Lens.lens (\CloseTunnelResponse' {httpStatus} -> httpStatus) (\s@CloseTunnelResponse' {} a -> s {httpStatus = a} :: CloseTunnelResponse)

instance Prelude.NFData CloseTunnelResponse where
  rnf CloseTunnelResponse' {..} = Prelude.rnf httpStatus
