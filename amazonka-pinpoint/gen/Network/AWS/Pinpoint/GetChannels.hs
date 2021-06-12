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
-- Module      : Network.AWS.Pinpoint.GetChannels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the history and status of each channel for
-- an application.
module Network.AWS.Pinpoint.GetChannels
  ( -- * Creating a Request
    GetChannels (..),
    newGetChannels,

    -- * Request Lenses
    getChannels_applicationId,

    -- * Destructuring the Response
    GetChannelsResponse (..),
    newGetChannelsResponse,

    -- * Response Lenses
    getChannelsResponse_httpStatus,
    getChannelsResponse_channelsResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetChannels' smart constructor.
data GetChannels = GetChannels'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetChannels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getChannels_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetChannels ::
  -- | 'applicationId'
  Core.Text ->
  GetChannels
newGetChannels pApplicationId_ =
  GetChannels' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getChannels_applicationId :: Lens.Lens' GetChannels Core.Text
getChannels_applicationId = Lens.lens (\GetChannels' {applicationId} -> applicationId) (\s@GetChannels' {} a -> s {applicationId = a} :: GetChannels)

instance Core.AWSRequest GetChannels where
  type AWSResponse GetChannels = GetChannelsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChannelsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetChannels

instance Core.NFData GetChannels

instance Core.ToHeaders GetChannels where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetChannels where
  toPath GetChannels' {..} =
    Core.mconcat
      ["/v1/apps/", Core.toBS applicationId, "/channels"]

instance Core.ToQuery GetChannels where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetChannelsResponse' smart constructor.
data GetChannelsResponse = GetChannelsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    channelsResponse :: ChannelsResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetChannelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getChannelsResponse_httpStatus' - The response's http status code.
--
-- 'channelsResponse', 'getChannelsResponse_channelsResponse' - Undocumented member.
newGetChannelsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'channelsResponse'
  ChannelsResponse ->
  GetChannelsResponse
newGetChannelsResponse
  pHttpStatus_
  pChannelsResponse_ =
    GetChannelsResponse'
      { httpStatus = pHttpStatus_,
        channelsResponse = pChannelsResponse_
      }

-- | The response's http status code.
getChannelsResponse_httpStatus :: Lens.Lens' GetChannelsResponse Core.Int
getChannelsResponse_httpStatus = Lens.lens (\GetChannelsResponse' {httpStatus} -> httpStatus) (\s@GetChannelsResponse' {} a -> s {httpStatus = a} :: GetChannelsResponse)

-- | Undocumented member.
getChannelsResponse_channelsResponse :: Lens.Lens' GetChannelsResponse ChannelsResponse
getChannelsResponse_channelsResponse = Lens.lens (\GetChannelsResponse' {channelsResponse} -> channelsResponse) (\s@GetChannelsResponse' {} a -> s {channelsResponse = a} :: GetChannelsResponse)

instance Core.NFData GetChannelsResponse
