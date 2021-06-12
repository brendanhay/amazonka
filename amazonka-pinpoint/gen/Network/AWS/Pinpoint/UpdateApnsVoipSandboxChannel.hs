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
-- Module      : Network.AWS.Pinpoint.UpdateApnsVoipSandboxChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs VoIP sandbox channel for an application or updates the
-- status and settings of the APNs VoIP sandbox channel for an application.
module Network.AWS.Pinpoint.UpdateApnsVoipSandboxChannel
  ( -- * Creating a Request
    UpdateApnsVoipSandboxChannel (..),
    newUpdateApnsVoipSandboxChannel,

    -- * Request Lenses
    updateApnsVoipSandboxChannel_applicationId,
    updateApnsVoipSandboxChannel_aPNSVoipSandboxChannelRequest,

    -- * Destructuring the Response
    UpdateApnsVoipSandboxChannelResponse (..),
    newUpdateApnsVoipSandboxChannelResponse,

    -- * Response Lenses
    updateApnsVoipSandboxChannelResponse_httpStatus,
    updateApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateApnsVoipSandboxChannel' smart constructor.
data UpdateApnsVoipSandboxChannel = UpdateApnsVoipSandboxChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    aPNSVoipSandboxChannelRequest :: APNSVoipSandboxChannelRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApnsVoipSandboxChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateApnsVoipSandboxChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'aPNSVoipSandboxChannelRequest', 'updateApnsVoipSandboxChannel_aPNSVoipSandboxChannelRequest' - Undocumented member.
newUpdateApnsVoipSandboxChannel ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'aPNSVoipSandboxChannelRequest'
  APNSVoipSandboxChannelRequest ->
  UpdateApnsVoipSandboxChannel
newUpdateApnsVoipSandboxChannel
  pApplicationId_
  pAPNSVoipSandboxChannelRequest_ =
    UpdateApnsVoipSandboxChannel'
      { applicationId =
          pApplicationId_,
        aPNSVoipSandboxChannelRequest =
          pAPNSVoipSandboxChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateApnsVoipSandboxChannel_applicationId :: Lens.Lens' UpdateApnsVoipSandboxChannel Core.Text
updateApnsVoipSandboxChannel_applicationId = Lens.lens (\UpdateApnsVoipSandboxChannel' {applicationId} -> applicationId) (\s@UpdateApnsVoipSandboxChannel' {} a -> s {applicationId = a} :: UpdateApnsVoipSandboxChannel)

-- | Undocumented member.
updateApnsVoipSandboxChannel_aPNSVoipSandboxChannelRequest :: Lens.Lens' UpdateApnsVoipSandboxChannel APNSVoipSandboxChannelRequest
updateApnsVoipSandboxChannel_aPNSVoipSandboxChannelRequest = Lens.lens (\UpdateApnsVoipSandboxChannel' {aPNSVoipSandboxChannelRequest} -> aPNSVoipSandboxChannelRequest) (\s@UpdateApnsVoipSandboxChannel' {} a -> s {aPNSVoipSandboxChannelRequest = a} :: UpdateApnsVoipSandboxChannel)

instance Core.AWSRequest UpdateApnsVoipSandboxChannel where
  type
    AWSResponse UpdateApnsVoipSandboxChannel =
      UpdateApnsVoipSandboxChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApnsVoipSandboxChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable UpdateApnsVoipSandboxChannel

instance Core.NFData UpdateApnsVoipSandboxChannel

instance Core.ToHeaders UpdateApnsVoipSandboxChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateApnsVoipSandboxChannel where
  toJSON UpdateApnsVoipSandboxChannel' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "APNSVoipSandboxChannelRequest"
                  Core..= aPNSVoipSandboxChannelRequest
              )
          ]
      )

instance Core.ToPath UpdateApnsVoipSandboxChannel where
  toPath UpdateApnsVoipSandboxChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns_voip_sandbox"
      ]

instance Core.ToQuery UpdateApnsVoipSandboxChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateApnsVoipSandboxChannelResponse' smart constructor.
data UpdateApnsVoipSandboxChannelResponse = UpdateApnsVoipSandboxChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    aPNSVoipSandboxChannelResponse :: APNSVoipSandboxChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApnsVoipSandboxChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApnsVoipSandboxChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSVoipSandboxChannelResponse', 'updateApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse' - Undocumented member.
newUpdateApnsVoipSandboxChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'aPNSVoipSandboxChannelResponse'
  APNSVoipSandboxChannelResponse ->
  UpdateApnsVoipSandboxChannelResponse
newUpdateApnsVoipSandboxChannelResponse
  pHttpStatus_
  pAPNSVoipSandboxChannelResponse_ =
    UpdateApnsVoipSandboxChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSVoipSandboxChannelResponse =
          pAPNSVoipSandboxChannelResponse_
      }

-- | The response's http status code.
updateApnsVoipSandboxChannelResponse_httpStatus :: Lens.Lens' UpdateApnsVoipSandboxChannelResponse Core.Int
updateApnsVoipSandboxChannelResponse_httpStatus = Lens.lens (\UpdateApnsVoipSandboxChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateApnsVoipSandboxChannelResponse' {} a -> s {httpStatus = a} :: UpdateApnsVoipSandboxChannelResponse)

-- | Undocumented member.
updateApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse :: Lens.Lens' UpdateApnsVoipSandboxChannelResponse APNSVoipSandboxChannelResponse
updateApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse = Lens.lens (\UpdateApnsVoipSandboxChannelResponse' {aPNSVoipSandboxChannelResponse} -> aPNSVoipSandboxChannelResponse) (\s@UpdateApnsVoipSandboxChannelResponse' {} a -> s {aPNSVoipSandboxChannelResponse = a} :: UpdateApnsVoipSandboxChannelResponse)

instance
  Core.NFData
    UpdateApnsVoipSandboxChannelResponse
