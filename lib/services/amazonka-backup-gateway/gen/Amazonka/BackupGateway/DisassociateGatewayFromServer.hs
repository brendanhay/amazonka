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
-- Module      : Amazonka.BackupGateway.DisassociateGatewayFromServer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a backup gateway from the specified server. After the
-- disassociation process finishes, the gateway can no longer access the
-- virtual machines on the server.
module Amazonka.BackupGateway.DisassociateGatewayFromServer
  ( -- * Creating a Request
    DisassociateGatewayFromServer (..),
    newDisassociateGatewayFromServer,

    -- * Request Lenses
    disassociateGatewayFromServer_gatewayArn,

    -- * Destructuring the Response
    DisassociateGatewayFromServerResponse (..),
    newDisassociateGatewayFromServerResponse,

    -- * Response Lenses
    disassociateGatewayFromServerResponse_gatewayArn,
    disassociateGatewayFromServerResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateGatewayFromServer' smart constructor.
data DisassociateGatewayFromServer = DisassociateGatewayFromServer'
  { -- | The Amazon Resource Name (ARN) of the gateway to disassociate.
    gatewayArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateGatewayFromServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'disassociateGatewayFromServer_gatewayArn' - The Amazon Resource Name (ARN) of the gateway to disassociate.
newDisassociateGatewayFromServer ::
  -- | 'gatewayArn'
  Prelude.Text ->
  DisassociateGatewayFromServer
newDisassociateGatewayFromServer pGatewayArn_ =
  DisassociateGatewayFromServer'
    { gatewayArn =
        pGatewayArn_
    }

-- | The Amazon Resource Name (ARN) of the gateway to disassociate.
disassociateGatewayFromServer_gatewayArn :: Lens.Lens' DisassociateGatewayFromServer Prelude.Text
disassociateGatewayFromServer_gatewayArn = Lens.lens (\DisassociateGatewayFromServer' {gatewayArn} -> gatewayArn) (\s@DisassociateGatewayFromServer' {} a -> s {gatewayArn = a} :: DisassociateGatewayFromServer)

instance
  Core.AWSRequest
    DisassociateGatewayFromServer
  where
  type
    AWSResponse DisassociateGatewayFromServer =
      DisassociateGatewayFromServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateGatewayFromServerResponse'
            Prelude.<$> (x Data..?> "GatewayArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateGatewayFromServer
  where
  hashWithSalt _salt DisassociateGatewayFromServer' {..} =
    _salt `Prelude.hashWithSalt` gatewayArn

instance Prelude.NFData DisassociateGatewayFromServer where
  rnf DisassociateGatewayFromServer' {..} =
    Prelude.rnf gatewayArn

instance Data.ToHeaders DisassociateGatewayFromServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.DisassociateGatewayFromServer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateGatewayFromServer where
  toJSON DisassociateGatewayFromServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayArn" Data..= gatewayArn)]
      )

instance Data.ToPath DisassociateGatewayFromServer where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateGatewayFromServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateGatewayFromServerResponse' smart constructor.
data DisassociateGatewayFromServerResponse = DisassociateGatewayFromServerResponse'
  { -- | The Amazon Resource Name (ARN) of the gateway you disassociated.
    gatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateGatewayFromServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'disassociateGatewayFromServerResponse_gatewayArn' - The Amazon Resource Name (ARN) of the gateway you disassociated.
--
-- 'httpStatus', 'disassociateGatewayFromServerResponse_httpStatus' - The response's http status code.
newDisassociateGatewayFromServerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateGatewayFromServerResponse
newDisassociateGatewayFromServerResponse pHttpStatus_ =
  DisassociateGatewayFromServerResponse'
    { gatewayArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the gateway you disassociated.
disassociateGatewayFromServerResponse_gatewayArn :: Lens.Lens' DisassociateGatewayFromServerResponse (Prelude.Maybe Prelude.Text)
disassociateGatewayFromServerResponse_gatewayArn = Lens.lens (\DisassociateGatewayFromServerResponse' {gatewayArn} -> gatewayArn) (\s@DisassociateGatewayFromServerResponse' {} a -> s {gatewayArn = a} :: DisassociateGatewayFromServerResponse)

-- | The response's http status code.
disassociateGatewayFromServerResponse_httpStatus :: Lens.Lens' DisassociateGatewayFromServerResponse Prelude.Int
disassociateGatewayFromServerResponse_httpStatus = Lens.lens (\DisassociateGatewayFromServerResponse' {httpStatus} -> httpStatus) (\s@DisassociateGatewayFromServerResponse' {} a -> s {httpStatus = a} :: DisassociateGatewayFromServerResponse)

instance
  Prelude.NFData
    DisassociateGatewayFromServerResponse
  where
  rnf DisassociateGatewayFromServerResponse' {..} =
    Prelude.rnf gatewayArn
      `Prelude.seq` Prelude.rnf httpStatus
