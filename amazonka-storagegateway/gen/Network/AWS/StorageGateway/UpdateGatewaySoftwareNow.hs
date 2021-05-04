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
-- Module      : Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway
-- to update.
--
-- /See:/ 'newUpdateGatewaySoftwareNow' smart constructor.
data UpdateGatewaySoftwareNow = UpdateGatewaySoftwareNow'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest UpdateGatewaySoftwareNow where
  type
    Rs UpdateGatewaySoftwareNow =
      UpdateGatewaySoftwareNowResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGatewaySoftwareNowResponse'
            Prelude.<$> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGatewaySoftwareNow

instance Prelude.NFData UpdateGatewaySoftwareNow

instance Prelude.ToHeaders UpdateGatewaySoftwareNow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.UpdateGatewaySoftwareNow" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateGatewaySoftwareNow where
  toJSON UpdateGatewaySoftwareNow' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Prelude..= gatewayARN)]
      )

instance Prelude.ToPath UpdateGatewaySoftwareNow where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateGatewaySoftwareNow where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
