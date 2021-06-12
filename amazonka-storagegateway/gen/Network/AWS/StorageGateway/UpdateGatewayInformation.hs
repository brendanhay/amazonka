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
-- Module      : Network.AWS.StorageGateway.UpdateGatewayInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a gateway\'s metadata, which includes the gateway\'s name and
-- time zone. To specify which gateway to update, use the Amazon Resource
-- Name (ARN) of the gateway in your request.
--
-- For gateways activated after September 2, 2015, the gateway\'s ARN
-- contains the gateway ID rather than the gateway name. However, changing
-- the name of the gateway has no effect on the gateway\'s ARN.
module Network.AWS.StorageGateway.UpdateGatewayInformation
  ( -- * Creating a Request
    UpdateGatewayInformation (..),
    newUpdateGatewayInformation,

    -- * Request Lenses
    updateGatewayInformation_gatewayName,
    updateGatewayInformation_gatewayTimezone,
    updateGatewayInformation_cloudWatchLogGroupARN,
    updateGatewayInformation_gatewayARN,

    -- * Destructuring the Response
    UpdateGatewayInformationResponse (..),
    newUpdateGatewayInformationResponse,

    -- * Response Lenses
    updateGatewayInformationResponse_gatewayName,
    updateGatewayInformationResponse_gatewayARN,
    updateGatewayInformationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newUpdateGatewayInformation' smart constructor.
data UpdateGatewayInformation = UpdateGatewayInformation'
  { gatewayName :: Core.Maybe Core.Text,
    -- | A value that indicates the time zone of the gateway.
    gatewayTimezone :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
    -- you want to use to monitor and log events in the gateway.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/WhatIsCloudWatchLogs.html What is Amazon CloudWatch Logs?>
    cloudWatchLogGroupARN :: Core.Maybe Core.Text,
    gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGatewayInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayName', 'updateGatewayInformation_gatewayName' - Undocumented member.
--
-- 'gatewayTimezone', 'updateGatewayInformation_gatewayTimezone' - A value that indicates the time zone of the gateway.
--
-- 'cloudWatchLogGroupARN', 'updateGatewayInformation_cloudWatchLogGroupARN' - The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
-- you want to use to monitor and log events in the gateway.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/WhatIsCloudWatchLogs.html What is Amazon CloudWatch Logs?>
--
-- 'gatewayARN', 'updateGatewayInformation_gatewayARN' - Undocumented member.
newUpdateGatewayInformation ::
  -- | 'gatewayARN'
  Core.Text ->
  UpdateGatewayInformation
newUpdateGatewayInformation pGatewayARN_ =
  UpdateGatewayInformation'
    { gatewayName =
        Core.Nothing,
      gatewayTimezone = Core.Nothing,
      cloudWatchLogGroupARN = Core.Nothing,
      gatewayARN = pGatewayARN_
    }

-- | Undocumented member.
updateGatewayInformation_gatewayName :: Lens.Lens' UpdateGatewayInformation (Core.Maybe Core.Text)
updateGatewayInformation_gatewayName = Lens.lens (\UpdateGatewayInformation' {gatewayName} -> gatewayName) (\s@UpdateGatewayInformation' {} a -> s {gatewayName = a} :: UpdateGatewayInformation)

-- | A value that indicates the time zone of the gateway.
updateGatewayInformation_gatewayTimezone :: Lens.Lens' UpdateGatewayInformation (Core.Maybe Core.Text)
updateGatewayInformation_gatewayTimezone = Lens.lens (\UpdateGatewayInformation' {gatewayTimezone} -> gatewayTimezone) (\s@UpdateGatewayInformation' {} a -> s {gatewayTimezone = a} :: UpdateGatewayInformation)

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that
-- you want to use to monitor and log events in the gateway.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/WhatIsCloudWatchLogs.html What is Amazon CloudWatch Logs?>
updateGatewayInformation_cloudWatchLogGroupARN :: Lens.Lens' UpdateGatewayInformation (Core.Maybe Core.Text)
updateGatewayInformation_cloudWatchLogGroupARN = Lens.lens (\UpdateGatewayInformation' {cloudWatchLogGroupARN} -> cloudWatchLogGroupARN) (\s@UpdateGatewayInformation' {} a -> s {cloudWatchLogGroupARN = a} :: UpdateGatewayInformation)

-- | Undocumented member.
updateGatewayInformation_gatewayARN :: Lens.Lens' UpdateGatewayInformation Core.Text
updateGatewayInformation_gatewayARN = Lens.lens (\UpdateGatewayInformation' {gatewayARN} -> gatewayARN) (\s@UpdateGatewayInformation' {} a -> s {gatewayARN = a} :: UpdateGatewayInformation)

instance Core.AWSRequest UpdateGatewayInformation where
  type
    AWSResponse UpdateGatewayInformation =
      UpdateGatewayInformationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGatewayInformationResponse'
            Core.<$> (x Core..?> "GatewayName")
            Core.<*> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateGatewayInformation

instance Core.NFData UpdateGatewayInformation

instance Core.ToHeaders UpdateGatewayInformation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.UpdateGatewayInformation" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateGatewayInformation where
  toJSON UpdateGatewayInformation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("GatewayName" Core..=) Core.<$> gatewayName,
            ("GatewayTimezone" Core..=) Core.<$> gatewayTimezone,
            ("CloudWatchLogGroupARN" Core..=)
              Core.<$> cloudWatchLogGroupARN,
            Core.Just ("GatewayARN" Core..= gatewayARN)
          ]
      )

instance Core.ToPath UpdateGatewayInformation where
  toPath = Core.const "/"

instance Core.ToQuery UpdateGatewayInformation where
  toQuery = Core.const Core.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway
-- that was updated.
--
-- /See:/ 'newUpdateGatewayInformationResponse' smart constructor.
data UpdateGatewayInformationResponse = UpdateGatewayInformationResponse'
  { -- | The name you configured for your gateway.
    gatewayName :: Core.Maybe Core.Text,
    gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGatewayInformationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayName', 'updateGatewayInformationResponse_gatewayName' - The name you configured for your gateway.
--
-- 'gatewayARN', 'updateGatewayInformationResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'updateGatewayInformationResponse_httpStatus' - The response's http status code.
newUpdateGatewayInformationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateGatewayInformationResponse
newUpdateGatewayInformationResponse pHttpStatus_ =
  UpdateGatewayInformationResponse'
    { gatewayName =
        Core.Nothing,
      gatewayARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name you configured for your gateway.
updateGatewayInformationResponse_gatewayName :: Lens.Lens' UpdateGatewayInformationResponse (Core.Maybe Core.Text)
updateGatewayInformationResponse_gatewayName = Lens.lens (\UpdateGatewayInformationResponse' {gatewayName} -> gatewayName) (\s@UpdateGatewayInformationResponse' {} a -> s {gatewayName = a} :: UpdateGatewayInformationResponse)

-- | Undocumented member.
updateGatewayInformationResponse_gatewayARN :: Lens.Lens' UpdateGatewayInformationResponse (Core.Maybe Core.Text)
updateGatewayInformationResponse_gatewayARN = Lens.lens (\UpdateGatewayInformationResponse' {gatewayARN} -> gatewayARN) (\s@UpdateGatewayInformationResponse' {} a -> s {gatewayARN = a} :: UpdateGatewayInformationResponse)

-- | The response's http status code.
updateGatewayInformationResponse_httpStatus :: Lens.Lens' UpdateGatewayInformationResponse Core.Int
updateGatewayInformationResponse_httpStatus = Lens.lens (\UpdateGatewayInformationResponse' {httpStatus} -> httpStatus) (\s@UpdateGatewayInformationResponse' {} a -> s {httpStatus = a} :: UpdateGatewayInformationResponse)

instance Core.NFData UpdateGatewayInformationResponse
