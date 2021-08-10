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
-- Module      : Network.AWS.Pinpoint.GetSmsChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the SMS channel
-- for an application.
module Network.AWS.Pinpoint.GetSmsChannel
  ( -- * Creating a Request
    GetSmsChannel (..),
    newGetSmsChannel,

    -- * Request Lenses
    getSmsChannel_applicationId,

    -- * Destructuring the Response
    GetSmsChannelResponse (..),
    newGetSmsChannelResponse,

    -- * Response Lenses
    getSmsChannelResponse_httpStatus,
    getSmsChannelResponse_sMSChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSmsChannel' smart constructor.
data GetSmsChannel = GetSmsChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSmsChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getSmsChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetSmsChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  GetSmsChannel
newGetSmsChannel pApplicationId_ =
  GetSmsChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getSmsChannel_applicationId :: Lens.Lens' GetSmsChannel Prelude.Text
getSmsChannel_applicationId = Lens.lens (\GetSmsChannel' {applicationId} -> applicationId) (\s@GetSmsChannel' {} a -> s {applicationId = a} :: GetSmsChannel)

instance Core.AWSRequest GetSmsChannel where
  type
    AWSResponse GetSmsChannel =
      GetSmsChannelResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSmsChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetSmsChannel

instance Prelude.NFData GetSmsChannel

instance Core.ToHeaders GetSmsChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetSmsChannel where
  toPath GetSmsChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/sms"
      ]

instance Core.ToQuery GetSmsChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSmsChannelResponse' smart constructor.
data GetSmsChannelResponse = GetSmsChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    sMSChannelResponse :: SMSChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSmsChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSmsChannelResponse_httpStatus' - The response's http status code.
--
-- 'sMSChannelResponse', 'getSmsChannelResponse_sMSChannelResponse' - Undocumented member.
newGetSmsChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'sMSChannelResponse'
  SMSChannelResponse ->
  GetSmsChannelResponse
newGetSmsChannelResponse
  pHttpStatus_
  pSMSChannelResponse_ =
    GetSmsChannelResponse'
      { httpStatus = pHttpStatus_,
        sMSChannelResponse = pSMSChannelResponse_
      }

-- | The response's http status code.
getSmsChannelResponse_httpStatus :: Lens.Lens' GetSmsChannelResponse Prelude.Int
getSmsChannelResponse_httpStatus = Lens.lens (\GetSmsChannelResponse' {httpStatus} -> httpStatus) (\s@GetSmsChannelResponse' {} a -> s {httpStatus = a} :: GetSmsChannelResponse)

-- | Undocumented member.
getSmsChannelResponse_sMSChannelResponse :: Lens.Lens' GetSmsChannelResponse SMSChannelResponse
getSmsChannelResponse_sMSChannelResponse = Lens.lens (\GetSmsChannelResponse' {sMSChannelResponse} -> sMSChannelResponse) (\s@GetSmsChannelResponse' {} a -> s {sMSChannelResponse = a} :: GetSmsChannelResponse)

instance Prelude.NFData GetSmsChannelResponse
