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
-- Module      : Amazonka.BackupGateway.AssociateGatewayToServer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a backup gateway with your server. After you complete the
-- association process, you can back up and restore your VMs through the
-- gateway.
module Amazonka.BackupGateway.AssociateGatewayToServer
  ( -- * Creating a Request
    AssociateGatewayToServer (..),
    newAssociateGatewayToServer,

    -- * Request Lenses
    associateGatewayToServer_gatewayArn,
    associateGatewayToServer_serverArn,

    -- * Destructuring the Response
    AssociateGatewayToServerResponse (..),
    newAssociateGatewayToServerResponse,

    -- * Response Lenses
    associateGatewayToServerResponse_gatewayArn,
    associateGatewayToServerResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateGatewayToServer' smart constructor.
data AssociateGatewayToServer = AssociateGatewayToServer'
  { -- | The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@
    -- operation to return a list of gateways for your account and Amazon Web
    -- Services Region.
    gatewayArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the server that hosts your virtual
    -- machines.
    serverArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateGatewayToServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'associateGatewayToServer_gatewayArn' - The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
--
-- 'serverArn', 'associateGatewayToServer_serverArn' - The Amazon Resource Name (ARN) of the server that hosts your virtual
-- machines.
newAssociateGatewayToServer ::
  -- | 'gatewayArn'
  Prelude.Text ->
  -- | 'serverArn'
  Prelude.Text ->
  AssociateGatewayToServer
newAssociateGatewayToServer pGatewayArn_ pServerArn_ =
  AssociateGatewayToServer'
    { gatewayArn =
        pGatewayArn_,
      serverArn = pServerArn_
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
associateGatewayToServer_gatewayArn :: Lens.Lens' AssociateGatewayToServer Prelude.Text
associateGatewayToServer_gatewayArn = Lens.lens (\AssociateGatewayToServer' {gatewayArn} -> gatewayArn) (\s@AssociateGatewayToServer' {} a -> s {gatewayArn = a} :: AssociateGatewayToServer)

-- | The Amazon Resource Name (ARN) of the server that hosts your virtual
-- machines.
associateGatewayToServer_serverArn :: Lens.Lens' AssociateGatewayToServer Prelude.Text
associateGatewayToServer_serverArn = Lens.lens (\AssociateGatewayToServer' {serverArn} -> serverArn) (\s@AssociateGatewayToServer' {} a -> s {serverArn = a} :: AssociateGatewayToServer)

instance Core.AWSRequest AssociateGatewayToServer where
  type
    AWSResponse AssociateGatewayToServer =
      AssociateGatewayToServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateGatewayToServerResponse'
            Prelude.<$> (x Data..?> "GatewayArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateGatewayToServer where
  hashWithSalt _salt AssociateGatewayToServer' {..} =
    _salt
      `Prelude.hashWithSalt` gatewayArn
      `Prelude.hashWithSalt` serverArn

instance Prelude.NFData AssociateGatewayToServer where
  rnf AssociateGatewayToServer' {..} =
    Prelude.rnf gatewayArn `Prelude.seq`
      Prelude.rnf serverArn

instance Data.ToHeaders AssociateGatewayToServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.AssociateGatewayToServer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateGatewayToServer where
  toJSON AssociateGatewayToServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayArn" Data..= gatewayArn),
            Prelude.Just ("ServerArn" Data..= serverArn)
          ]
      )

instance Data.ToPath AssociateGatewayToServer where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateGatewayToServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateGatewayToServerResponse' smart constructor.
data AssociateGatewayToServerResponse = AssociateGatewayToServerResponse'
  { -- | The Amazon Resource Name (ARN) of a gateway.
    gatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateGatewayToServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'associateGatewayToServerResponse_gatewayArn' - The Amazon Resource Name (ARN) of a gateway.
--
-- 'httpStatus', 'associateGatewayToServerResponse_httpStatus' - The response's http status code.
newAssociateGatewayToServerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateGatewayToServerResponse
newAssociateGatewayToServerResponse pHttpStatus_ =
  AssociateGatewayToServerResponse'
    { gatewayArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of a gateway.
associateGatewayToServerResponse_gatewayArn :: Lens.Lens' AssociateGatewayToServerResponse (Prelude.Maybe Prelude.Text)
associateGatewayToServerResponse_gatewayArn = Lens.lens (\AssociateGatewayToServerResponse' {gatewayArn} -> gatewayArn) (\s@AssociateGatewayToServerResponse' {} a -> s {gatewayArn = a} :: AssociateGatewayToServerResponse)

-- | The response's http status code.
associateGatewayToServerResponse_httpStatus :: Lens.Lens' AssociateGatewayToServerResponse Prelude.Int
associateGatewayToServerResponse_httpStatus = Lens.lens (\AssociateGatewayToServerResponse' {httpStatus} -> httpStatus) (\s@AssociateGatewayToServerResponse' {} a -> s {httpStatus = a} :: AssociateGatewayToServerResponse)

instance
  Prelude.NFData
    AssociateGatewayToServerResponse
  where
  rnf AssociateGatewayToServerResponse' {..} =
    Prelude.rnf gatewayArn `Prelude.seq`
      Prelude.rnf httpStatus
