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
-- Module      : Network.AWS.Pinpoint.UpdateApnsSandboxChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs sandbox channel for an application or updates the
-- status and settings of the APNs sandbox channel for an application.
module Network.AWS.Pinpoint.UpdateApnsSandboxChannel
  ( -- * Creating a Request
    UpdateApnsSandboxChannel (..),
    newUpdateApnsSandboxChannel,

    -- * Request Lenses
    updateApnsSandboxChannel_applicationId,
    updateApnsSandboxChannel_aPNSSandboxChannelRequest,

    -- * Destructuring the Response
    UpdateApnsSandboxChannelResponse (..),
    newUpdateApnsSandboxChannelResponse,

    -- * Response Lenses
    updateApnsSandboxChannelResponse_httpStatus,
    updateApnsSandboxChannelResponse_aPNSSandboxChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateApnsSandboxChannel' smart constructor.
data UpdateApnsSandboxChannel = UpdateApnsSandboxChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    aPNSSandboxChannelRequest :: APNSSandboxChannelRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApnsSandboxChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateApnsSandboxChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'aPNSSandboxChannelRequest', 'updateApnsSandboxChannel_aPNSSandboxChannelRequest' - Undocumented member.
newUpdateApnsSandboxChannel ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'aPNSSandboxChannelRequest'
  APNSSandboxChannelRequest ->
  UpdateApnsSandboxChannel
newUpdateApnsSandboxChannel
  pApplicationId_
  pAPNSSandboxChannelRequest_ =
    UpdateApnsSandboxChannel'
      { applicationId =
          pApplicationId_,
        aPNSSandboxChannelRequest =
          pAPNSSandboxChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateApnsSandboxChannel_applicationId :: Lens.Lens' UpdateApnsSandboxChannel Core.Text
updateApnsSandboxChannel_applicationId = Lens.lens (\UpdateApnsSandboxChannel' {applicationId} -> applicationId) (\s@UpdateApnsSandboxChannel' {} a -> s {applicationId = a} :: UpdateApnsSandboxChannel)

-- | Undocumented member.
updateApnsSandboxChannel_aPNSSandboxChannelRequest :: Lens.Lens' UpdateApnsSandboxChannel APNSSandboxChannelRequest
updateApnsSandboxChannel_aPNSSandboxChannelRequest = Lens.lens (\UpdateApnsSandboxChannel' {aPNSSandboxChannelRequest} -> aPNSSandboxChannelRequest) (\s@UpdateApnsSandboxChannel' {} a -> s {aPNSSandboxChannelRequest = a} :: UpdateApnsSandboxChannel)

instance Core.AWSRequest UpdateApnsSandboxChannel where
  type
    AWSResponse UpdateApnsSandboxChannel =
      UpdateApnsSandboxChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApnsSandboxChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable UpdateApnsSandboxChannel

instance Core.NFData UpdateApnsSandboxChannel

instance Core.ToHeaders UpdateApnsSandboxChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateApnsSandboxChannel where
  toJSON UpdateApnsSandboxChannel' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "APNSSandboxChannelRequest"
                  Core..= aPNSSandboxChannelRequest
              )
          ]
      )

instance Core.ToPath UpdateApnsSandboxChannel where
  toPath UpdateApnsSandboxChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns_sandbox"
      ]

instance Core.ToQuery UpdateApnsSandboxChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateApnsSandboxChannelResponse' smart constructor.
data UpdateApnsSandboxChannelResponse = UpdateApnsSandboxChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    aPNSSandboxChannelResponse :: APNSSandboxChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApnsSandboxChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApnsSandboxChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSSandboxChannelResponse', 'updateApnsSandboxChannelResponse_aPNSSandboxChannelResponse' - Undocumented member.
newUpdateApnsSandboxChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'aPNSSandboxChannelResponse'
  APNSSandboxChannelResponse ->
  UpdateApnsSandboxChannelResponse
newUpdateApnsSandboxChannelResponse
  pHttpStatus_
  pAPNSSandboxChannelResponse_ =
    UpdateApnsSandboxChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSSandboxChannelResponse =
          pAPNSSandboxChannelResponse_
      }

-- | The response's http status code.
updateApnsSandboxChannelResponse_httpStatus :: Lens.Lens' UpdateApnsSandboxChannelResponse Core.Int
updateApnsSandboxChannelResponse_httpStatus = Lens.lens (\UpdateApnsSandboxChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateApnsSandboxChannelResponse' {} a -> s {httpStatus = a} :: UpdateApnsSandboxChannelResponse)

-- | Undocumented member.
updateApnsSandboxChannelResponse_aPNSSandboxChannelResponse :: Lens.Lens' UpdateApnsSandboxChannelResponse APNSSandboxChannelResponse
updateApnsSandboxChannelResponse_aPNSSandboxChannelResponse = Lens.lens (\UpdateApnsSandboxChannelResponse' {aPNSSandboxChannelResponse} -> aPNSSandboxChannelResponse) (\s@UpdateApnsSandboxChannelResponse' {} a -> s {aPNSSandboxChannelResponse = a} :: UpdateApnsSandboxChannelResponse)

instance Core.NFData UpdateApnsSandboxChannelResponse
