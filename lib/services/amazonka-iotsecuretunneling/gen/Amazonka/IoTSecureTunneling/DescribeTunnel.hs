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
-- Module      : Amazonka.IoTSecureTunneling.DescribeTunnel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a tunnel identified by the unique tunnel id.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeTunnel>
-- action.
module Amazonka.IoTSecureTunneling.DescribeTunnel
  ( -- * Creating a Request
    DescribeTunnel (..),
    newDescribeTunnel,

    -- * Request Lenses
    describeTunnel_tunnelId,

    -- * Destructuring the Response
    DescribeTunnelResponse (..),
    newDescribeTunnelResponse,

    -- * Response Lenses
    describeTunnelResponse_tunnel,
    describeTunnelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSecureTunneling.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTunnel' smart constructor.
data DescribeTunnel = DescribeTunnel'
  { -- | The tunnel to describe.
    tunnelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTunnel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tunnelId', 'describeTunnel_tunnelId' - The tunnel to describe.
newDescribeTunnel ::
  -- | 'tunnelId'
  Prelude.Text ->
  DescribeTunnel
newDescribeTunnel pTunnelId_ =
  DescribeTunnel' {tunnelId = pTunnelId_}

-- | The tunnel to describe.
describeTunnel_tunnelId :: Lens.Lens' DescribeTunnel Prelude.Text
describeTunnel_tunnelId = Lens.lens (\DescribeTunnel' {tunnelId} -> tunnelId) (\s@DescribeTunnel' {} a -> s {tunnelId = a} :: DescribeTunnel)

instance Core.AWSRequest DescribeTunnel where
  type
    AWSResponse DescribeTunnel =
      DescribeTunnelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTunnelResponse'
            Prelude.<$> (x Data..?> "tunnel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTunnel where
  hashWithSalt _salt DescribeTunnel' {..} =
    _salt `Prelude.hashWithSalt` tunnelId

instance Prelude.NFData DescribeTunnel where
  rnf DescribeTunnel' {..} = Prelude.rnf tunnelId

instance Data.ToHeaders DescribeTunnel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTSecuredTunneling.DescribeTunnel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTunnel where
  toJSON DescribeTunnel' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("tunnelId" Data..= tunnelId)]
      )

instance Data.ToPath DescribeTunnel where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTunnel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTunnelResponse' smart constructor.
data DescribeTunnelResponse = DescribeTunnelResponse'
  { -- | The tunnel being described.
    tunnel :: Prelude.Maybe Tunnel,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTunnelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tunnel', 'describeTunnelResponse_tunnel' - The tunnel being described.
--
-- 'httpStatus', 'describeTunnelResponse_httpStatus' - The response's http status code.
newDescribeTunnelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTunnelResponse
newDescribeTunnelResponse pHttpStatus_ =
  DescribeTunnelResponse'
    { tunnel = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tunnel being described.
describeTunnelResponse_tunnel :: Lens.Lens' DescribeTunnelResponse (Prelude.Maybe Tunnel)
describeTunnelResponse_tunnel = Lens.lens (\DescribeTunnelResponse' {tunnel} -> tunnel) (\s@DescribeTunnelResponse' {} a -> s {tunnel = a} :: DescribeTunnelResponse)

-- | The response's http status code.
describeTunnelResponse_httpStatus :: Lens.Lens' DescribeTunnelResponse Prelude.Int
describeTunnelResponse_httpStatus = Lens.lens (\DescribeTunnelResponse' {httpStatus} -> httpStatus) (\s@DescribeTunnelResponse' {} a -> s {httpStatus = a} :: DescribeTunnelResponse)

instance Prelude.NFData DescribeTunnelResponse where
  rnf DescribeTunnelResponse' {..} =
    Prelude.rnf tunnel
      `Prelude.seq` Prelude.rnf httpStatus
