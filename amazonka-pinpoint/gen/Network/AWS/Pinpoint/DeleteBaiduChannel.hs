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
-- Module      : Network.AWS.Pinpoint.DeleteBaiduChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the Baidu channel for an application and deletes any existing
-- settings for the channel.
module Network.AWS.Pinpoint.DeleteBaiduChannel
  ( -- * Creating a Request
    DeleteBaiduChannel (..),
    newDeleteBaiduChannel,

    -- * Request Lenses
    deleteBaiduChannel_applicationId,

    -- * Destructuring the Response
    DeleteBaiduChannelResponse (..),
    newDeleteBaiduChannelResponse,

    -- * Response Lenses
    deleteBaiduChannelResponse_httpStatus,
    deleteBaiduChannelResponse_baiduChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBaiduChannel' smart constructor.
data DeleteBaiduChannel = DeleteBaiduChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteBaiduChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteBaiduChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteBaiduChannel ::
  -- | 'applicationId'
  Core.Text ->
  DeleteBaiduChannel
newDeleteBaiduChannel pApplicationId_ =
  DeleteBaiduChannel'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteBaiduChannel_applicationId :: Lens.Lens' DeleteBaiduChannel Core.Text
deleteBaiduChannel_applicationId = Lens.lens (\DeleteBaiduChannel' {applicationId} -> applicationId) (\s@DeleteBaiduChannel' {} a -> s {applicationId = a} :: DeleteBaiduChannel)

instance Core.AWSRequest DeleteBaiduChannel where
  type
    AWSResponse DeleteBaiduChannel =
      DeleteBaiduChannelResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBaiduChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable DeleteBaiduChannel

instance Core.NFData DeleteBaiduChannel

instance Core.ToHeaders DeleteBaiduChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteBaiduChannel where
  toPath DeleteBaiduChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/baidu"
      ]

instance Core.ToQuery DeleteBaiduChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteBaiduChannelResponse' smart constructor.
data DeleteBaiduChannelResponse = DeleteBaiduChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    baiduChannelResponse :: BaiduChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteBaiduChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteBaiduChannelResponse_httpStatus' - The response's http status code.
--
-- 'baiduChannelResponse', 'deleteBaiduChannelResponse_baiduChannelResponse' - Undocumented member.
newDeleteBaiduChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'baiduChannelResponse'
  BaiduChannelResponse ->
  DeleteBaiduChannelResponse
newDeleteBaiduChannelResponse
  pHttpStatus_
  pBaiduChannelResponse_ =
    DeleteBaiduChannelResponse'
      { httpStatus =
          pHttpStatus_,
        baiduChannelResponse = pBaiduChannelResponse_
      }

-- | The response's http status code.
deleteBaiduChannelResponse_httpStatus :: Lens.Lens' DeleteBaiduChannelResponse Core.Int
deleteBaiduChannelResponse_httpStatus = Lens.lens (\DeleteBaiduChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteBaiduChannelResponse' {} a -> s {httpStatus = a} :: DeleteBaiduChannelResponse)

-- | Undocumented member.
deleteBaiduChannelResponse_baiduChannelResponse :: Lens.Lens' DeleteBaiduChannelResponse BaiduChannelResponse
deleteBaiduChannelResponse_baiduChannelResponse = Lens.lens (\DeleteBaiduChannelResponse' {baiduChannelResponse} -> baiduChannelResponse) (\s@DeleteBaiduChannelResponse' {} a -> s {baiduChannelResponse = a} :: DeleteBaiduChannelResponse)

instance Core.NFData DeleteBaiduChannelResponse
