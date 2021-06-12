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
-- Module      : Network.AWS.Pinpoint.UpdateApnsChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs channel for an application or updates the status and
-- settings of the APNs channel for an application.
module Network.AWS.Pinpoint.UpdateApnsChannel
  ( -- * Creating a Request
    UpdateApnsChannel (..),
    newUpdateApnsChannel,

    -- * Request Lenses
    updateApnsChannel_applicationId,
    updateApnsChannel_aPNSChannelRequest,

    -- * Destructuring the Response
    UpdateApnsChannelResponse (..),
    newUpdateApnsChannelResponse,

    -- * Response Lenses
    updateApnsChannelResponse_httpStatus,
    updateApnsChannelResponse_aPNSChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateApnsChannel' smart constructor.
data UpdateApnsChannel = UpdateApnsChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    aPNSChannelRequest :: APNSChannelRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApnsChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateApnsChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'aPNSChannelRequest', 'updateApnsChannel_aPNSChannelRequest' - Undocumented member.
newUpdateApnsChannel ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'aPNSChannelRequest'
  APNSChannelRequest ->
  UpdateApnsChannel
newUpdateApnsChannel
  pApplicationId_
  pAPNSChannelRequest_ =
    UpdateApnsChannel'
      { applicationId = pApplicationId_,
        aPNSChannelRequest = pAPNSChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateApnsChannel_applicationId :: Lens.Lens' UpdateApnsChannel Core.Text
updateApnsChannel_applicationId = Lens.lens (\UpdateApnsChannel' {applicationId} -> applicationId) (\s@UpdateApnsChannel' {} a -> s {applicationId = a} :: UpdateApnsChannel)

-- | Undocumented member.
updateApnsChannel_aPNSChannelRequest :: Lens.Lens' UpdateApnsChannel APNSChannelRequest
updateApnsChannel_aPNSChannelRequest = Lens.lens (\UpdateApnsChannel' {aPNSChannelRequest} -> aPNSChannelRequest) (\s@UpdateApnsChannel' {} a -> s {aPNSChannelRequest = a} :: UpdateApnsChannel)

instance Core.AWSRequest UpdateApnsChannel where
  type
    AWSResponse UpdateApnsChannel =
      UpdateApnsChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApnsChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable UpdateApnsChannel

instance Core.NFData UpdateApnsChannel

instance Core.ToHeaders UpdateApnsChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateApnsChannel where
  toJSON UpdateApnsChannel' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("APNSChannelRequest" Core..= aPNSChannelRequest)
          ]
      )

instance Core.ToPath UpdateApnsChannel where
  toPath UpdateApnsChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns"
      ]

instance Core.ToQuery UpdateApnsChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateApnsChannelResponse' smart constructor.
data UpdateApnsChannelResponse = UpdateApnsChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    aPNSChannelResponse :: APNSChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApnsChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApnsChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSChannelResponse', 'updateApnsChannelResponse_aPNSChannelResponse' - Undocumented member.
newUpdateApnsChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'aPNSChannelResponse'
  APNSChannelResponse ->
  UpdateApnsChannelResponse
newUpdateApnsChannelResponse
  pHttpStatus_
  pAPNSChannelResponse_ =
    UpdateApnsChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSChannelResponse = pAPNSChannelResponse_
      }

-- | The response's http status code.
updateApnsChannelResponse_httpStatus :: Lens.Lens' UpdateApnsChannelResponse Core.Int
updateApnsChannelResponse_httpStatus = Lens.lens (\UpdateApnsChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateApnsChannelResponse' {} a -> s {httpStatus = a} :: UpdateApnsChannelResponse)

-- | Undocumented member.
updateApnsChannelResponse_aPNSChannelResponse :: Lens.Lens' UpdateApnsChannelResponse APNSChannelResponse
updateApnsChannelResponse_aPNSChannelResponse = Lens.lens (\UpdateApnsChannelResponse' {aPNSChannelResponse} -> aPNSChannelResponse) (\s@UpdateApnsChannelResponse' {} a -> s {aPNSChannelResponse = a} :: UpdateApnsChannelResponse)

instance Core.NFData UpdateApnsChannelResponse
