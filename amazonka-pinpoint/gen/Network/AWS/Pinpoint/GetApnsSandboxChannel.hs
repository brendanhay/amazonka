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
-- Module      : Network.AWS.Pinpoint.GetApnsSandboxChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the APNs sandbox
-- channel for an application.
module Network.AWS.Pinpoint.GetApnsSandboxChannel
  ( -- * Creating a Request
    GetApnsSandboxChannel (..),
    newGetApnsSandboxChannel,

    -- * Request Lenses
    getApnsSandboxChannel_applicationId,

    -- * Destructuring the Response
    GetApnsSandboxChannelResponse (..),
    newGetApnsSandboxChannelResponse,

    -- * Response Lenses
    getApnsSandboxChannelResponse_httpStatus,
    getApnsSandboxChannelResponse_aPNSSandboxChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetApnsSandboxChannel' smart constructor.
data GetApnsSandboxChannel = GetApnsSandboxChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetApnsSandboxChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getApnsSandboxChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetApnsSandboxChannel ::
  -- | 'applicationId'
  Core.Text ->
  GetApnsSandboxChannel
newGetApnsSandboxChannel pApplicationId_ =
  GetApnsSandboxChannel'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getApnsSandboxChannel_applicationId :: Lens.Lens' GetApnsSandboxChannel Core.Text
getApnsSandboxChannel_applicationId = Lens.lens (\GetApnsSandboxChannel' {applicationId} -> applicationId) (\s@GetApnsSandboxChannel' {} a -> s {applicationId = a} :: GetApnsSandboxChannel)

instance Core.AWSRequest GetApnsSandboxChannel where
  type
    AWSResponse GetApnsSandboxChannel =
      GetApnsSandboxChannelResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApnsSandboxChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetApnsSandboxChannel

instance Core.NFData GetApnsSandboxChannel

instance Core.ToHeaders GetApnsSandboxChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetApnsSandboxChannel where
  toPath GetApnsSandboxChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns_sandbox"
      ]

instance Core.ToQuery GetApnsSandboxChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetApnsSandboxChannelResponse' smart constructor.
data GetApnsSandboxChannelResponse = GetApnsSandboxChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    aPNSSandboxChannelResponse :: APNSSandboxChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetApnsSandboxChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getApnsSandboxChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSSandboxChannelResponse', 'getApnsSandboxChannelResponse_aPNSSandboxChannelResponse' - Undocumented member.
newGetApnsSandboxChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'aPNSSandboxChannelResponse'
  APNSSandboxChannelResponse ->
  GetApnsSandboxChannelResponse
newGetApnsSandboxChannelResponse
  pHttpStatus_
  pAPNSSandboxChannelResponse_ =
    GetApnsSandboxChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSSandboxChannelResponse =
          pAPNSSandboxChannelResponse_
      }

-- | The response's http status code.
getApnsSandboxChannelResponse_httpStatus :: Lens.Lens' GetApnsSandboxChannelResponse Core.Int
getApnsSandboxChannelResponse_httpStatus = Lens.lens (\GetApnsSandboxChannelResponse' {httpStatus} -> httpStatus) (\s@GetApnsSandboxChannelResponse' {} a -> s {httpStatus = a} :: GetApnsSandboxChannelResponse)

-- | Undocumented member.
getApnsSandboxChannelResponse_aPNSSandboxChannelResponse :: Lens.Lens' GetApnsSandboxChannelResponse APNSSandboxChannelResponse
getApnsSandboxChannelResponse_aPNSSandboxChannelResponse = Lens.lens (\GetApnsSandboxChannelResponse' {aPNSSandboxChannelResponse} -> aPNSSandboxChannelResponse) (\s@GetApnsSandboxChannelResponse' {} a -> s {aPNSSandboxChannelResponse = a} :: GetApnsSandboxChannelResponse)

instance Core.NFData GetApnsSandboxChannelResponse
