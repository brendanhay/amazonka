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
-- Module      : Amazonka.Pinpoint.GetApnsVoipSandboxChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the APNs VoIP
-- sandbox channel for an application.
module Amazonka.Pinpoint.GetApnsVoipSandboxChannel
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetApnsVoipSandboxChannel' smart constructor.
data GetApnsVoipSandboxChannel = GetApnsVoipSandboxChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest GetApnsVoipSandboxChannel where
  type
    AWSResponse GetApnsVoipSandboxChannel =
      GetApnsVoipSandboxChannelResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApnsVoipSandboxChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetApnsVoipSandboxChannel where
  hashWithSalt _salt GetApnsVoipSandboxChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetApnsVoipSandboxChannel where
  rnf GetApnsVoipSandboxChannel' {..} =
    Prelude.rnf applicationId

instance Core.ToHeaders GetApnsVoipSandboxChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetApnsVoipSandboxChannel where
  toPath GetApnsVoipSandboxChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns_voip_sandbox"
      ]

instance Core.ToQuery GetApnsVoipSandboxChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApnsVoipSandboxChannelResponse' smart constructor.
data GetApnsVoipSandboxChannelResponse = GetApnsVoipSandboxChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    aPNSVoipSandboxChannelResponse :: APNSVoipSandboxChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf GetApnsVoipSandboxChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf aPNSVoipSandboxChannelResponse
