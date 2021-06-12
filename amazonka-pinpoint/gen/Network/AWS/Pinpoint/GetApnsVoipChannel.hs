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
-- Module      : Network.AWS.Pinpoint.GetApnsVoipChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the APNs VoIP
-- channel for an application.
module Network.AWS.Pinpoint.GetApnsVoipChannel
  ( -- * Creating a Request
    GetApnsVoipChannel (..),
    newGetApnsVoipChannel,

    -- * Request Lenses
    getApnsVoipChannel_applicationId,

    -- * Destructuring the Response
    GetApnsVoipChannelResponse (..),
    newGetApnsVoipChannelResponse,

    -- * Response Lenses
    getApnsVoipChannelResponse_httpStatus,
    getApnsVoipChannelResponse_aPNSVoipChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetApnsVoipChannel' smart constructor.
data GetApnsVoipChannel = GetApnsVoipChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetApnsVoipChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getApnsVoipChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetApnsVoipChannel ::
  -- | 'applicationId'
  Core.Text ->
  GetApnsVoipChannel
newGetApnsVoipChannel pApplicationId_ =
  GetApnsVoipChannel'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getApnsVoipChannel_applicationId :: Lens.Lens' GetApnsVoipChannel Core.Text
getApnsVoipChannel_applicationId = Lens.lens (\GetApnsVoipChannel' {applicationId} -> applicationId) (\s@GetApnsVoipChannel' {} a -> s {applicationId = a} :: GetApnsVoipChannel)

instance Core.AWSRequest GetApnsVoipChannel where
  type
    AWSResponse GetApnsVoipChannel =
      GetApnsVoipChannelResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApnsVoipChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetApnsVoipChannel

instance Core.NFData GetApnsVoipChannel

instance Core.ToHeaders GetApnsVoipChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetApnsVoipChannel where
  toPath GetApnsVoipChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns_voip"
      ]

instance Core.ToQuery GetApnsVoipChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetApnsVoipChannelResponse' smart constructor.
data GetApnsVoipChannelResponse = GetApnsVoipChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    aPNSVoipChannelResponse :: APNSVoipChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetApnsVoipChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getApnsVoipChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSVoipChannelResponse', 'getApnsVoipChannelResponse_aPNSVoipChannelResponse' - Undocumented member.
newGetApnsVoipChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'aPNSVoipChannelResponse'
  APNSVoipChannelResponse ->
  GetApnsVoipChannelResponse
newGetApnsVoipChannelResponse
  pHttpStatus_
  pAPNSVoipChannelResponse_ =
    GetApnsVoipChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSVoipChannelResponse =
          pAPNSVoipChannelResponse_
      }

-- | The response's http status code.
getApnsVoipChannelResponse_httpStatus :: Lens.Lens' GetApnsVoipChannelResponse Core.Int
getApnsVoipChannelResponse_httpStatus = Lens.lens (\GetApnsVoipChannelResponse' {httpStatus} -> httpStatus) (\s@GetApnsVoipChannelResponse' {} a -> s {httpStatus = a} :: GetApnsVoipChannelResponse)

-- | Undocumented member.
getApnsVoipChannelResponse_aPNSVoipChannelResponse :: Lens.Lens' GetApnsVoipChannelResponse APNSVoipChannelResponse
getApnsVoipChannelResponse_aPNSVoipChannelResponse = Lens.lens (\GetApnsVoipChannelResponse' {aPNSVoipChannelResponse} -> aPNSVoipChannelResponse) (\s@GetApnsVoipChannelResponse' {} a -> s {aPNSVoipChannelResponse = a} :: GetApnsVoipChannelResponse)

instance Core.NFData GetApnsVoipChannelResponse
