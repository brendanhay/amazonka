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
-- Module      : Network.AWS.Pinpoint.UpdateEmailChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the email channel for an application or updates the status and
-- settings of the email channel for an application.
module Network.AWS.Pinpoint.UpdateEmailChannel
  ( -- * Creating a Request
    UpdateEmailChannel (..),
    newUpdateEmailChannel,

    -- * Request Lenses
    updateEmailChannel_applicationId,
    updateEmailChannel_emailChannelRequest,

    -- * Destructuring the Response
    UpdateEmailChannelResponse (..),
    newUpdateEmailChannelResponse,

    -- * Response Lenses
    updateEmailChannelResponse_httpStatus,
    updateEmailChannelResponse_emailChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateEmailChannel' smart constructor.
data UpdateEmailChannel = UpdateEmailChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    emailChannelRequest :: EmailChannelRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateEmailChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateEmailChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'emailChannelRequest', 'updateEmailChannel_emailChannelRequest' - Undocumented member.
newUpdateEmailChannel ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'emailChannelRequest'
  EmailChannelRequest ->
  UpdateEmailChannel
newUpdateEmailChannel
  pApplicationId_
  pEmailChannelRequest_ =
    UpdateEmailChannel'
      { applicationId =
          pApplicationId_,
        emailChannelRequest = pEmailChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateEmailChannel_applicationId :: Lens.Lens' UpdateEmailChannel Core.Text
updateEmailChannel_applicationId = Lens.lens (\UpdateEmailChannel' {applicationId} -> applicationId) (\s@UpdateEmailChannel' {} a -> s {applicationId = a} :: UpdateEmailChannel)

-- | Undocumented member.
updateEmailChannel_emailChannelRequest :: Lens.Lens' UpdateEmailChannel EmailChannelRequest
updateEmailChannel_emailChannelRequest = Lens.lens (\UpdateEmailChannel' {emailChannelRequest} -> emailChannelRequest) (\s@UpdateEmailChannel' {} a -> s {emailChannelRequest = a} :: UpdateEmailChannel)

instance Core.AWSRequest UpdateEmailChannel where
  type
    AWSResponse UpdateEmailChannel =
      UpdateEmailChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEmailChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable UpdateEmailChannel

instance Core.NFData UpdateEmailChannel

instance Core.ToHeaders UpdateEmailChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateEmailChannel where
  toJSON UpdateEmailChannel' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("EmailChannelRequest" Core..= emailChannelRequest)
          ]
      )

instance Core.ToPath UpdateEmailChannel where
  toPath UpdateEmailChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/email"
      ]

instance Core.ToQuery UpdateEmailChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateEmailChannelResponse' smart constructor.
data UpdateEmailChannelResponse = UpdateEmailChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    emailChannelResponse :: EmailChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateEmailChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEmailChannelResponse_httpStatus' - The response's http status code.
--
-- 'emailChannelResponse', 'updateEmailChannelResponse_emailChannelResponse' - Undocumented member.
newUpdateEmailChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'emailChannelResponse'
  EmailChannelResponse ->
  UpdateEmailChannelResponse
newUpdateEmailChannelResponse
  pHttpStatus_
  pEmailChannelResponse_ =
    UpdateEmailChannelResponse'
      { httpStatus =
          pHttpStatus_,
        emailChannelResponse = pEmailChannelResponse_
      }

-- | The response's http status code.
updateEmailChannelResponse_httpStatus :: Lens.Lens' UpdateEmailChannelResponse Core.Int
updateEmailChannelResponse_httpStatus = Lens.lens (\UpdateEmailChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateEmailChannelResponse' {} a -> s {httpStatus = a} :: UpdateEmailChannelResponse)

-- | Undocumented member.
updateEmailChannelResponse_emailChannelResponse :: Lens.Lens' UpdateEmailChannelResponse EmailChannelResponse
updateEmailChannelResponse_emailChannelResponse = Lens.lens (\UpdateEmailChannelResponse' {emailChannelResponse} -> emailChannelResponse) (\s@UpdateEmailChannelResponse' {} a -> s {emailChannelResponse = a} :: UpdateEmailChannelResponse)

instance Core.NFData UpdateEmailChannelResponse
