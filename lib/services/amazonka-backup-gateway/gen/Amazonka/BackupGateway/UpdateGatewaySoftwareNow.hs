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
-- Module      : Amazonka.BackupGateway.UpdateGatewaySoftwareNow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the gateway virtual machine (VM) software. The request
-- immediately triggers the software update.
--
-- When you make this request, you get a @200 OK@ success response
-- immediately. However, it might take some time for the update to
-- complete.
module Amazonka.BackupGateway.UpdateGatewaySoftwareNow
  ( -- * Creating a Request
    UpdateGatewaySoftwareNow (..),
    newUpdateGatewaySoftwareNow,

    -- * Request Lenses
    updateGatewaySoftwareNow_gatewayArn,

    -- * Destructuring the Response
    UpdateGatewaySoftwareNowResponse (..),
    newUpdateGatewaySoftwareNowResponse,

    -- * Response Lenses
    updateGatewaySoftwareNowResponse_gatewayArn,
    updateGatewaySoftwareNowResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGatewaySoftwareNow' smart constructor.
data UpdateGatewaySoftwareNow = UpdateGatewaySoftwareNow'
  { -- | The Amazon Resource Name (ARN) of the gateway to be updated.
    gatewayArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewaySoftwareNow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'updateGatewaySoftwareNow_gatewayArn' - The Amazon Resource Name (ARN) of the gateway to be updated.
newUpdateGatewaySoftwareNow ::
  -- | 'gatewayArn'
  Prelude.Text ->
  UpdateGatewaySoftwareNow
newUpdateGatewaySoftwareNow pGatewayArn_ =
  UpdateGatewaySoftwareNow'
    { gatewayArn =
        pGatewayArn_
    }

-- | The Amazon Resource Name (ARN) of the gateway to be updated.
updateGatewaySoftwareNow_gatewayArn :: Lens.Lens' UpdateGatewaySoftwareNow Prelude.Text
updateGatewaySoftwareNow_gatewayArn = Lens.lens (\UpdateGatewaySoftwareNow' {gatewayArn} -> gatewayArn) (\s@UpdateGatewaySoftwareNow' {} a -> s {gatewayArn = a} :: UpdateGatewaySoftwareNow)

instance Core.AWSRequest UpdateGatewaySoftwareNow where
  type
    AWSResponse UpdateGatewaySoftwareNow =
      UpdateGatewaySoftwareNowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGatewaySoftwareNowResponse'
            Prelude.<$> (x Data..?> "GatewayArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGatewaySoftwareNow where
  hashWithSalt _salt UpdateGatewaySoftwareNow' {..} =
    _salt `Prelude.hashWithSalt` gatewayArn

instance Prelude.NFData UpdateGatewaySoftwareNow where
  rnf UpdateGatewaySoftwareNow' {..} =
    Prelude.rnf gatewayArn

instance Data.ToHeaders UpdateGatewaySoftwareNow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.UpdateGatewaySoftwareNow" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGatewaySoftwareNow where
  toJSON UpdateGatewaySoftwareNow' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayArn" Data..= gatewayArn)]
      )

instance Data.ToPath UpdateGatewaySoftwareNow where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateGatewaySoftwareNow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGatewaySoftwareNowResponse' smart constructor.
data UpdateGatewaySoftwareNowResponse = UpdateGatewaySoftwareNowResponse'
  { -- | The Amazon Resource Name (ARN) of the gateway you updated.
    gatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewaySoftwareNowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'updateGatewaySoftwareNowResponse_gatewayArn' - The Amazon Resource Name (ARN) of the gateway you updated.
--
-- 'httpStatus', 'updateGatewaySoftwareNowResponse_httpStatus' - The response's http status code.
newUpdateGatewaySoftwareNowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGatewaySoftwareNowResponse
newUpdateGatewaySoftwareNowResponse pHttpStatus_ =
  UpdateGatewaySoftwareNowResponse'
    { gatewayArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the gateway you updated.
updateGatewaySoftwareNowResponse_gatewayArn :: Lens.Lens' UpdateGatewaySoftwareNowResponse (Prelude.Maybe Prelude.Text)
updateGatewaySoftwareNowResponse_gatewayArn = Lens.lens (\UpdateGatewaySoftwareNowResponse' {gatewayArn} -> gatewayArn) (\s@UpdateGatewaySoftwareNowResponse' {} a -> s {gatewayArn = a} :: UpdateGatewaySoftwareNowResponse)

-- | The response's http status code.
updateGatewaySoftwareNowResponse_httpStatus :: Lens.Lens' UpdateGatewaySoftwareNowResponse Prelude.Int
updateGatewaySoftwareNowResponse_httpStatus = Lens.lens (\UpdateGatewaySoftwareNowResponse' {httpStatus} -> httpStatus) (\s@UpdateGatewaySoftwareNowResponse' {} a -> s {httpStatus = a} :: UpdateGatewaySoftwareNowResponse)

instance
  Prelude.NFData
    UpdateGatewaySoftwareNowResponse
  where
  rnf UpdateGatewaySoftwareNowResponse' {..} =
    Prelude.rnf gatewayArn
      `Prelude.seq` Prelude.rnf httpStatus
