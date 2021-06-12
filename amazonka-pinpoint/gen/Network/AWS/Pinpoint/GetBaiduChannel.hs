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
-- Module      : Network.AWS.Pinpoint.GetBaiduChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the Baidu channel
-- for an application.
module Network.AWS.Pinpoint.GetBaiduChannel
  ( -- * Creating a Request
    GetBaiduChannel (..),
    newGetBaiduChannel,

    -- * Request Lenses
    getBaiduChannel_applicationId,

    -- * Destructuring the Response
    GetBaiduChannelResponse (..),
    newGetBaiduChannelResponse,

    -- * Response Lenses
    getBaiduChannelResponse_httpStatus,
    getBaiduChannelResponse_baiduChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBaiduChannel' smart constructor.
data GetBaiduChannel = GetBaiduChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBaiduChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getBaiduChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetBaiduChannel ::
  -- | 'applicationId'
  Core.Text ->
  GetBaiduChannel
newGetBaiduChannel pApplicationId_ =
  GetBaiduChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getBaiduChannel_applicationId :: Lens.Lens' GetBaiduChannel Core.Text
getBaiduChannel_applicationId = Lens.lens (\GetBaiduChannel' {applicationId} -> applicationId) (\s@GetBaiduChannel' {} a -> s {applicationId = a} :: GetBaiduChannel)

instance Core.AWSRequest GetBaiduChannel where
  type
    AWSResponse GetBaiduChannel =
      GetBaiduChannelResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBaiduChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetBaiduChannel

instance Core.NFData GetBaiduChannel

instance Core.ToHeaders GetBaiduChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetBaiduChannel where
  toPath GetBaiduChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/baidu"
      ]

instance Core.ToQuery GetBaiduChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetBaiduChannelResponse' smart constructor.
data GetBaiduChannelResponse = GetBaiduChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    baiduChannelResponse :: BaiduChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBaiduChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getBaiduChannelResponse_httpStatus' - The response's http status code.
--
-- 'baiduChannelResponse', 'getBaiduChannelResponse_baiduChannelResponse' - Undocumented member.
newGetBaiduChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'baiduChannelResponse'
  BaiduChannelResponse ->
  GetBaiduChannelResponse
newGetBaiduChannelResponse
  pHttpStatus_
  pBaiduChannelResponse_ =
    GetBaiduChannelResponse'
      { httpStatus = pHttpStatus_,
        baiduChannelResponse = pBaiduChannelResponse_
      }

-- | The response's http status code.
getBaiduChannelResponse_httpStatus :: Lens.Lens' GetBaiduChannelResponse Core.Int
getBaiduChannelResponse_httpStatus = Lens.lens (\GetBaiduChannelResponse' {httpStatus} -> httpStatus) (\s@GetBaiduChannelResponse' {} a -> s {httpStatus = a} :: GetBaiduChannelResponse)

-- | Undocumented member.
getBaiduChannelResponse_baiduChannelResponse :: Lens.Lens' GetBaiduChannelResponse BaiduChannelResponse
getBaiduChannelResponse_baiduChannelResponse = Lens.lens (\GetBaiduChannelResponse' {baiduChannelResponse} -> baiduChannelResponse) (\s@GetBaiduChannelResponse' {} a -> s {baiduChannelResponse = a} :: GetBaiduChannelResponse)

instance Core.NFData GetBaiduChannelResponse
