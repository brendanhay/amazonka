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
-- Module      : Amazonka.StorageGateway.UpdateGatewaySoftwareNow
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
-- complete. You can call DescribeGatewayInformation to verify the gateway
-- is in the @STATE_RUNNING@ state.
--
-- A software update forces a system restart of your gateway. You can
-- minimize the chance of any disruption to your applications by increasing
-- your iSCSI Initiators\' timeouts. For more information about increasing
-- iSCSI Initiator timeouts for Windows and Linux, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/ConfiguringiSCSIClientInitiatorWindowsClient.html#CustomizeWindowsiSCSISettings Customizing your Windows iSCSI settings>
-- and
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/ConfiguringiSCSIClientInitiatorRedHatClient.html#CustomizeLinuxiSCSISettings Customizing your Linux iSCSI settings>,
-- respectively.
module Amazonka.StorageGateway.UpdateGatewaySoftwareNow
  ( -- * Creating a Request
    UpdateGatewaySoftwareNow (..),
    newUpdateGatewaySoftwareNow,

    -- * Request Lenses
    updateGatewaySoftwareNow_gatewayARN,

    -- * Destructuring the Response
    UpdateGatewaySoftwareNowResponse (..),
    newUpdateGatewaySoftwareNowResponse,

    -- * Response Lenses
    updateGatewaySoftwareNowResponse_gatewayARN,
    updateGatewaySoftwareNowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway
-- to update.
--
-- /See:/ 'newUpdateGatewaySoftwareNow' smart constructor.
data UpdateGatewaySoftwareNow = UpdateGatewaySoftwareNow'
  { gatewayARN :: Prelude.Text
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
-- 'gatewayARN', 'updateGatewaySoftwareNow_gatewayARN' - Undocumented member.
newUpdateGatewaySoftwareNow ::
  -- | 'gatewayARN'
  Prelude.Text ->
  UpdateGatewaySoftwareNow
newUpdateGatewaySoftwareNow pGatewayARN_ =
  UpdateGatewaySoftwareNow'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
updateGatewaySoftwareNow_gatewayARN :: Lens.Lens' UpdateGatewaySoftwareNow Prelude.Text
updateGatewaySoftwareNow_gatewayARN = Lens.lens (\UpdateGatewaySoftwareNow' {gatewayARN} -> gatewayARN) (\s@UpdateGatewaySoftwareNow' {} a -> s {gatewayARN = a} :: UpdateGatewaySoftwareNow)

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
            Prelude.<$> (x Data..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGatewaySoftwareNow where
  hashWithSalt _salt UpdateGatewaySoftwareNow' {..} =
    _salt `Prelude.hashWithSalt` gatewayARN

instance Prelude.NFData UpdateGatewaySoftwareNow where
  rnf UpdateGatewaySoftwareNow' {..} =
    Prelude.rnf gatewayARN

instance Data.ToHeaders UpdateGatewaySoftwareNow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.UpdateGatewaySoftwareNow" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGatewaySoftwareNow where
  toJSON UpdateGatewaySoftwareNow' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Data..= gatewayARN)]
      )

instance Data.ToPath UpdateGatewaySoftwareNow where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateGatewaySoftwareNow where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway
-- that was updated.
--
-- /See:/ 'newUpdateGatewaySoftwareNowResponse' smart constructor.
data UpdateGatewaySoftwareNowResponse = UpdateGatewaySoftwareNowResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
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
-- 'gatewayARN', 'updateGatewaySoftwareNowResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'updateGatewaySoftwareNowResponse_httpStatus' - The response's http status code.
newUpdateGatewaySoftwareNowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGatewaySoftwareNowResponse
newUpdateGatewaySoftwareNowResponse pHttpStatus_ =
  UpdateGatewaySoftwareNowResponse'
    { gatewayARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateGatewaySoftwareNowResponse_gatewayARN :: Lens.Lens' UpdateGatewaySoftwareNowResponse (Prelude.Maybe Prelude.Text)
updateGatewaySoftwareNowResponse_gatewayARN = Lens.lens (\UpdateGatewaySoftwareNowResponse' {gatewayARN} -> gatewayARN) (\s@UpdateGatewaySoftwareNowResponse' {} a -> s {gatewayARN = a} :: UpdateGatewaySoftwareNowResponse)

-- | The response's http status code.
updateGatewaySoftwareNowResponse_httpStatus :: Lens.Lens' UpdateGatewaySoftwareNowResponse Prelude.Int
updateGatewaySoftwareNowResponse_httpStatus = Lens.lens (\UpdateGatewaySoftwareNowResponse' {httpStatus} -> httpStatus) (\s@UpdateGatewaySoftwareNowResponse' {} a -> s {httpStatus = a} :: UpdateGatewaySoftwareNowResponse)

instance
  Prelude.NFData
    UpdateGatewaySoftwareNowResponse
  where
  rnf UpdateGatewaySoftwareNowResponse' {..} =
    Prelude.rnf gatewayARN `Prelude.seq`
      Prelude.rnf httpStatus
