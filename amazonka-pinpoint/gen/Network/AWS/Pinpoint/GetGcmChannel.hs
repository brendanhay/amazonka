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
-- Module      : Network.AWS.Pinpoint.GetGcmChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the GCM channel
-- for an application.
module Network.AWS.Pinpoint.GetGcmChannel
  ( -- * Creating a Request
    GetGcmChannel (..),
    newGetGcmChannel,

    -- * Request Lenses
    getGcmChannel_applicationId,

    -- * Destructuring the Response
    GetGcmChannelResponse (..),
    newGetGcmChannelResponse,

    -- * Response Lenses
    getGcmChannelResponse_httpStatus,
    getGcmChannelResponse_gCMChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetGcmChannel' smart constructor.
data GetGcmChannel = GetGcmChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGcmChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getGcmChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetGcmChannel ::
  -- | 'applicationId'
  Core.Text ->
  GetGcmChannel
newGetGcmChannel pApplicationId_ =
  GetGcmChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getGcmChannel_applicationId :: Lens.Lens' GetGcmChannel Core.Text
getGcmChannel_applicationId = Lens.lens (\GetGcmChannel' {applicationId} -> applicationId) (\s@GetGcmChannel' {} a -> s {applicationId = a} :: GetGcmChannel)

instance Core.AWSRequest GetGcmChannel where
  type
    AWSResponse GetGcmChannel =
      GetGcmChannelResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGcmChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetGcmChannel

instance Core.NFData GetGcmChannel

instance Core.ToHeaders GetGcmChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetGcmChannel where
  toPath GetGcmChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/gcm"
      ]

instance Core.ToQuery GetGcmChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetGcmChannelResponse' smart constructor.
data GetGcmChannelResponse = GetGcmChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    gCMChannelResponse :: GCMChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGcmChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getGcmChannelResponse_httpStatus' - The response's http status code.
--
-- 'gCMChannelResponse', 'getGcmChannelResponse_gCMChannelResponse' - Undocumented member.
newGetGcmChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'gCMChannelResponse'
  GCMChannelResponse ->
  GetGcmChannelResponse
newGetGcmChannelResponse
  pHttpStatus_
  pGCMChannelResponse_ =
    GetGcmChannelResponse'
      { httpStatus = pHttpStatus_,
        gCMChannelResponse = pGCMChannelResponse_
      }

-- | The response's http status code.
getGcmChannelResponse_httpStatus :: Lens.Lens' GetGcmChannelResponse Core.Int
getGcmChannelResponse_httpStatus = Lens.lens (\GetGcmChannelResponse' {httpStatus} -> httpStatus) (\s@GetGcmChannelResponse' {} a -> s {httpStatus = a} :: GetGcmChannelResponse)

-- | Undocumented member.
getGcmChannelResponse_gCMChannelResponse :: Lens.Lens' GetGcmChannelResponse GCMChannelResponse
getGcmChannelResponse_gCMChannelResponse = Lens.lens (\GetGcmChannelResponse' {gCMChannelResponse} -> gCMChannelResponse) (\s@GetGcmChannelResponse' {} a -> s {gCMChannelResponse = a} :: GetGcmChannelResponse)

instance Core.NFData GetGcmChannelResponse
