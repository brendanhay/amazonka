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
-- Module      : Network.AWS.Pinpoint.GetEmailChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the email channel
-- for an application.
module Network.AWS.Pinpoint.GetEmailChannel
  ( -- * Creating a Request
    GetEmailChannel (..),
    newGetEmailChannel,

    -- * Request Lenses
    getEmailChannel_applicationId,

    -- * Destructuring the Response
    GetEmailChannelResponse (..),
    newGetEmailChannelResponse,

    -- * Response Lenses
    getEmailChannelResponse_httpStatus,
    getEmailChannelResponse_emailChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetEmailChannel' smart constructor.
data GetEmailChannel = GetEmailChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEmailChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getEmailChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetEmailChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  GetEmailChannel
newGetEmailChannel pApplicationId_ =
  GetEmailChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getEmailChannel_applicationId :: Lens.Lens' GetEmailChannel Prelude.Text
getEmailChannel_applicationId = Lens.lens (\GetEmailChannel' {applicationId} -> applicationId) (\s@GetEmailChannel' {} a -> s {applicationId = a} :: GetEmailChannel)

instance Core.AWSRequest GetEmailChannel where
  type
    AWSResponse GetEmailChannel =
      GetEmailChannelResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEmailChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetEmailChannel

instance Prelude.NFData GetEmailChannel

instance Core.ToHeaders GetEmailChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetEmailChannel where
  toPath GetEmailChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/email"
      ]

instance Core.ToQuery GetEmailChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEmailChannelResponse' smart constructor.
data GetEmailChannelResponse = GetEmailChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    emailChannelResponse :: EmailChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEmailChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getEmailChannelResponse_httpStatus' - The response's http status code.
--
-- 'emailChannelResponse', 'getEmailChannelResponse_emailChannelResponse' - Undocumented member.
newGetEmailChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'emailChannelResponse'
  EmailChannelResponse ->
  GetEmailChannelResponse
newGetEmailChannelResponse
  pHttpStatus_
  pEmailChannelResponse_ =
    GetEmailChannelResponse'
      { httpStatus = pHttpStatus_,
        emailChannelResponse = pEmailChannelResponse_
      }

-- | The response's http status code.
getEmailChannelResponse_httpStatus :: Lens.Lens' GetEmailChannelResponse Prelude.Int
getEmailChannelResponse_httpStatus = Lens.lens (\GetEmailChannelResponse' {httpStatus} -> httpStatus) (\s@GetEmailChannelResponse' {} a -> s {httpStatus = a} :: GetEmailChannelResponse)

-- | Undocumented member.
getEmailChannelResponse_emailChannelResponse :: Lens.Lens' GetEmailChannelResponse EmailChannelResponse
getEmailChannelResponse_emailChannelResponse = Lens.lens (\GetEmailChannelResponse' {emailChannelResponse} -> emailChannelResponse) (\s@GetEmailChannelResponse' {} a -> s {emailChannelResponse = a} :: GetEmailChannelResponse)

instance Prelude.NFData GetEmailChannelResponse
