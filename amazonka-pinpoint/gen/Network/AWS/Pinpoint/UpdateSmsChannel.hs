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
-- Module      : Network.AWS.Pinpoint.UpdateSmsChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the SMS channel for an application or updates the status and
-- settings of the SMS channel for an application.
module Network.AWS.Pinpoint.UpdateSmsChannel
  ( -- * Creating a Request
    UpdateSmsChannel (..),
    newUpdateSmsChannel,

    -- * Request Lenses
    updateSmsChannel_applicationId,
    updateSmsChannel_sMSChannelRequest,

    -- * Destructuring the Response
    UpdateSmsChannelResponse (..),
    newUpdateSmsChannelResponse,

    -- * Response Lenses
    updateSmsChannelResponse_httpStatus,
    updateSmsChannelResponse_sMSChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSmsChannel' smart constructor.
data UpdateSmsChannel = UpdateSmsChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    sMSChannelRequest :: SMSChannelRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSmsChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateSmsChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'sMSChannelRequest', 'updateSmsChannel_sMSChannelRequest' - Undocumented member.
newUpdateSmsChannel ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'sMSChannelRequest'
  SMSChannelRequest ->
  UpdateSmsChannel
newUpdateSmsChannel
  pApplicationId_
  pSMSChannelRequest_ =
    UpdateSmsChannel'
      { applicationId = pApplicationId_,
        sMSChannelRequest = pSMSChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateSmsChannel_applicationId :: Lens.Lens' UpdateSmsChannel Core.Text
updateSmsChannel_applicationId = Lens.lens (\UpdateSmsChannel' {applicationId} -> applicationId) (\s@UpdateSmsChannel' {} a -> s {applicationId = a} :: UpdateSmsChannel)

-- | Undocumented member.
updateSmsChannel_sMSChannelRequest :: Lens.Lens' UpdateSmsChannel SMSChannelRequest
updateSmsChannel_sMSChannelRequest = Lens.lens (\UpdateSmsChannel' {sMSChannelRequest} -> sMSChannelRequest) (\s@UpdateSmsChannel' {} a -> s {sMSChannelRequest = a} :: UpdateSmsChannel)

instance Core.AWSRequest UpdateSmsChannel where
  type
    AWSResponse UpdateSmsChannel =
      UpdateSmsChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSmsChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable UpdateSmsChannel

instance Core.NFData UpdateSmsChannel

instance Core.ToHeaders UpdateSmsChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateSmsChannel where
  toJSON UpdateSmsChannel' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("SMSChannelRequest" Core..= sMSChannelRequest)
          ]
      )

instance Core.ToPath UpdateSmsChannel where
  toPath UpdateSmsChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/sms"
      ]

instance Core.ToQuery UpdateSmsChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateSmsChannelResponse' smart constructor.
data UpdateSmsChannelResponse = UpdateSmsChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    sMSChannelResponse :: SMSChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSmsChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSmsChannelResponse_httpStatus' - The response's http status code.
--
-- 'sMSChannelResponse', 'updateSmsChannelResponse_sMSChannelResponse' - Undocumented member.
newUpdateSmsChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'sMSChannelResponse'
  SMSChannelResponse ->
  UpdateSmsChannelResponse
newUpdateSmsChannelResponse
  pHttpStatus_
  pSMSChannelResponse_ =
    UpdateSmsChannelResponse'
      { httpStatus =
          pHttpStatus_,
        sMSChannelResponse = pSMSChannelResponse_
      }

-- | The response's http status code.
updateSmsChannelResponse_httpStatus :: Lens.Lens' UpdateSmsChannelResponse Core.Int
updateSmsChannelResponse_httpStatus = Lens.lens (\UpdateSmsChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateSmsChannelResponse' {} a -> s {httpStatus = a} :: UpdateSmsChannelResponse)

-- | Undocumented member.
updateSmsChannelResponse_sMSChannelResponse :: Lens.Lens' UpdateSmsChannelResponse SMSChannelResponse
updateSmsChannelResponse_sMSChannelResponse = Lens.lens (\UpdateSmsChannelResponse' {sMSChannelResponse} -> sMSChannelResponse) (\s@UpdateSmsChannelResponse' {} a -> s {sMSChannelResponse = a} :: UpdateSmsChannelResponse)

instance Core.NFData UpdateSmsChannelResponse
