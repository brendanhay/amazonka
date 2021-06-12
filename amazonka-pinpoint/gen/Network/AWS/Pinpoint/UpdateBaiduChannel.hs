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
-- Module      : Network.AWS.Pinpoint.UpdateBaiduChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the Baidu channel for an application or updates the status and
-- settings of the Baidu channel for an application.
module Network.AWS.Pinpoint.UpdateBaiduChannel
  ( -- * Creating a Request
    UpdateBaiduChannel (..),
    newUpdateBaiduChannel,

    -- * Request Lenses
    updateBaiduChannel_applicationId,
    updateBaiduChannel_baiduChannelRequest,

    -- * Destructuring the Response
    UpdateBaiduChannelResponse (..),
    newUpdateBaiduChannelResponse,

    -- * Response Lenses
    updateBaiduChannelResponse_httpStatus,
    updateBaiduChannelResponse_baiduChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateBaiduChannel' smart constructor.
data UpdateBaiduChannel = UpdateBaiduChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    baiduChannelRequest :: BaiduChannelRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateBaiduChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateBaiduChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'baiduChannelRequest', 'updateBaiduChannel_baiduChannelRequest' - Undocumented member.
newUpdateBaiduChannel ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'baiduChannelRequest'
  BaiduChannelRequest ->
  UpdateBaiduChannel
newUpdateBaiduChannel
  pApplicationId_
  pBaiduChannelRequest_ =
    UpdateBaiduChannel'
      { applicationId =
          pApplicationId_,
        baiduChannelRequest = pBaiduChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateBaiduChannel_applicationId :: Lens.Lens' UpdateBaiduChannel Core.Text
updateBaiduChannel_applicationId = Lens.lens (\UpdateBaiduChannel' {applicationId} -> applicationId) (\s@UpdateBaiduChannel' {} a -> s {applicationId = a} :: UpdateBaiduChannel)

-- | Undocumented member.
updateBaiduChannel_baiduChannelRequest :: Lens.Lens' UpdateBaiduChannel BaiduChannelRequest
updateBaiduChannel_baiduChannelRequest = Lens.lens (\UpdateBaiduChannel' {baiduChannelRequest} -> baiduChannelRequest) (\s@UpdateBaiduChannel' {} a -> s {baiduChannelRequest = a} :: UpdateBaiduChannel)

instance Core.AWSRequest UpdateBaiduChannel where
  type
    AWSResponse UpdateBaiduChannel =
      UpdateBaiduChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBaiduChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable UpdateBaiduChannel

instance Core.NFData UpdateBaiduChannel

instance Core.ToHeaders UpdateBaiduChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateBaiduChannel where
  toJSON UpdateBaiduChannel' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("BaiduChannelRequest" Core..= baiduChannelRequest)
          ]
      )

instance Core.ToPath UpdateBaiduChannel where
  toPath UpdateBaiduChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/baidu"
      ]

instance Core.ToQuery UpdateBaiduChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateBaiduChannelResponse' smart constructor.
data UpdateBaiduChannelResponse = UpdateBaiduChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    baiduChannelResponse :: BaiduChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateBaiduChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateBaiduChannelResponse_httpStatus' - The response's http status code.
--
-- 'baiduChannelResponse', 'updateBaiduChannelResponse_baiduChannelResponse' - Undocumented member.
newUpdateBaiduChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'baiduChannelResponse'
  BaiduChannelResponse ->
  UpdateBaiduChannelResponse
newUpdateBaiduChannelResponse
  pHttpStatus_
  pBaiduChannelResponse_ =
    UpdateBaiduChannelResponse'
      { httpStatus =
          pHttpStatus_,
        baiduChannelResponse = pBaiduChannelResponse_
      }

-- | The response's http status code.
updateBaiduChannelResponse_httpStatus :: Lens.Lens' UpdateBaiduChannelResponse Core.Int
updateBaiduChannelResponse_httpStatus = Lens.lens (\UpdateBaiduChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateBaiduChannelResponse' {} a -> s {httpStatus = a} :: UpdateBaiduChannelResponse)

-- | Undocumented member.
updateBaiduChannelResponse_baiduChannelResponse :: Lens.Lens' UpdateBaiduChannelResponse BaiduChannelResponse
updateBaiduChannelResponse_baiduChannelResponse = Lens.lens (\UpdateBaiduChannelResponse' {baiduChannelResponse} -> baiduChannelResponse) (\s@UpdateBaiduChannelResponse' {} a -> s {baiduChannelResponse = a} :: UpdateBaiduChannelResponse)

instance Core.NFData UpdateBaiduChannelResponse
