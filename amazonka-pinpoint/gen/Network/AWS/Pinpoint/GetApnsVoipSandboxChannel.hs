{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.GetApnsVoipSandboxChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the APNs VoIP
-- sandbox channel for an application.
module Network.AWS.Pinpoint.GetApnsVoipSandboxChannel
  ( -- * Creating a Request
    GetApnsVoipSandboxChannel (..),
    newGetApnsVoipSandboxChannel,

    -- * Request Lenses
    getApnsVoipSandboxChannel_applicationId,

    -- * Destructuring the Response
    GetApnsVoipSandboxChannelResponse (..),
    newGetApnsVoipSandboxChannelResponse,

    -- * Response Lenses
    getApnsVoipSandboxChannelResponse_httpStatus,
    getApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetApnsVoipSandboxChannel' smart constructor.
data GetApnsVoipSandboxChannel = GetApnsVoipSandboxChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetApnsVoipSandboxChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getApnsVoipSandboxChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetApnsVoipSandboxChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  GetApnsVoipSandboxChannel
newGetApnsVoipSandboxChannel pApplicationId_ =
  GetApnsVoipSandboxChannel'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getApnsVoipSandboxChannel_applicationId :: Lens.Lens' GetApnsVoipSandboxChannel Prelude.Text
getApnsVoipSandboxChannel_applicationId = Lens.lens (\GetApnsVoipSandboxChannel' {applicationId} -> applicationId) (\s@GetApnsVoipSandboxChannel' {} a -> s {applicationId = a} :: GetApnsVoipSandboxChannel)

instance Prelude.AWSRequest GetApnsVoipSandboxChannel where
  type
    Rs GetApnsVoipSandboxChannel =
      GetApnsVoipSandboxChannelResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApnsVoipSandboxChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable GetApnsVoipSandboxChannel

instance Prelude.NFData GetApnsVoipSandboxChannel

instance Prelude.ToHeaders GetApnsVoipSandboxChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetApnsVoipSandboxChannel where
  toPath GetApnsVoipSandboxChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Prelude.toBS applicationId,
        "/channels/apns_voip_sandbox"
      ]

instance Prelude.ToQuery GetApnsVoipSandboxChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApnsVoipSandboxChannelResponse' smart constructor.
data GetApnsVoipSandboxChannelResponse = GetApnsVoipSandboxChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    aPNSVoipSandboxChannelResponse :: APNSVoipSandboxChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetApnsVoipSandboxChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getApnsVoipSandboxChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSVoipSandboxChannelResponse', 'getApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse' - Undocumented member.
newGetApnsVoipSandboxChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'aPNSVoipSandboxChannelResponse'
  APNSVoipSandboxChannelResponse ->
  GetApnsVoipSandboxChannelResponse
newGetApnsVoipSandboxChannelResponse
  pHttpStatus_
  pAPNSVoipSandboxChannelResponse_ =
    GetApnsVoipSandboxChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSVoipSandboxChannelResponse =
          pAPNSVoipSandboxChannelResponse_
      }

-- | The response's http status code.
getApnsVoipSandboxChannelResponse_httpStatus :: Lens.Lens' GetApnsVoipSandboxChannelResponse Prelude.Int
getApnsVoipSandboxChannelResponse_httpStatus = Lens.lens (\GetApnsVoipSandboxChannelResponse' {httpStatus} -> httpStatus) (\s@GetApnsVoipSandboxChannelResponse' {} a -> s {httpStatus = a} :: GetApnsVoipSandboxChannelResponse)

-- | Undocumented member.
getApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse :: Lens.Lens' GetApnsVoipSandboxChannelResponse APNSVoipSandboxChannelResponse
getApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse = Lens.lens (\GetApnsVoipSandboxChannelResponse' {aPNSVoipSandboxChannelResponse} -> aPNSVoipSandboxChannelResponse) (\s@GetApnsVoipSandboxChannelResponse' {} a -> s {aPNSVoipSandboxChannelResponse = a} :: GetApnsVoipSandboxChannelResponse)

instance
  Prelude.NFData
    GetApnsVoipSandboxChannelResponse
