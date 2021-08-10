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
-- Module      : Network.AWS.Pinpoint.GetApnsChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the APNs channel
-- for an application.
module Network.AWS.Pinpoint.GetApnsChannel
  ( -- * Creating a Request
    GetApnsChannel (..),
    newGetApnsChannel,

    -- * Request Lenses
    getApnsChannel_applicationId,

    -- * Destructuring the Response
    GetApnsChannelResponse (..),
    newGetApnsChannelResponse,

    -- * Response Lenses
    getApnsChannelResponse_httpStatus,
    getApnsChannelResponse_aPNSChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetApnsChannel' smart constructor.
data GetApnsChannel = GetApnsChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApnsChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getApnsChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetApnsChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  GetApnsChannel
newGetApnsChannel pApplicationId_ =
  GetApnsChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getApnsChannel_applicationId :: Lens.Lens' GetApnsChannel Prelude.Text
getApnsChannel_applicationId = Lens.lens (\GetApnsChannel' {applicationId} -> applicationId) (\s@GetApnsChannel' {} a -> s {applicationId = a} :: GetApnsChannel)

instance Core.AWSRequest GetApnsChannel where
  type
    AWSResponse GetApnsChannel =
      GetApnsChannelResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApnsChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetApnsChannel

instance Prelude.NFData GetApnsChannel

instance Core.ToHeaders GetApnsChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetApnsChannel where
  toPath GetApnsChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns"
      ]

instance Core.ToQuery GetApnsChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApnsChannelResponse' smart constructor.
data GetApnsChannelResponse = GetApnsChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    aPNSChannelResponse :: APNSChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApnsChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getApnsChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSChannelResponse', 'getApnsChannelResponse_aPNSChannelResponse' - Undocumented member.
newGetApnsChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'aPNSChannelResponse'
  APNSChannelResponse ->
  GetApnsChannelResponse
newGetApnsChannelResponse
  pHttpStatus_
  pAPNSChannelResponse_ =
    GetApnsChannelResponse'
      { httpStatus = pHttpStatus_,
        aPNSChannelResponse = pAPNSChannelResponse_
      }

-- | The response's http status code.
getApnsChannelResponse_httpStatus :: Lens.Lens' GetApnsChannelResponse Prelude.Int
getApnsChannelResponse_httpStatus = Lens.lens (\GetApnsChannelResponse' {httpStatus} -> httpStatus) (\s@GetApnsChannelResponse' {} a -> s {httpStatus = a} :: GetApnsChannelResponse)

-- | Undocumented member.
getApnsChannelResponse_aPNSChannelResponse :: Lens.Lens' GetApnsChannelResponse APNSChannelResponse
getApnsChannelResponse_aPNSChannelResponse = Lens.lens (\GetApnsChannelResponse' {aPNSChannelResponse} -> aPNSChannelResponse) (\s@GetApnsChannelResponse' {} a -> s {aPNSChannelResponse = a} :: GetApnsChannelResponse)

instance Prelude.NFData GetApnsChannelResponse
