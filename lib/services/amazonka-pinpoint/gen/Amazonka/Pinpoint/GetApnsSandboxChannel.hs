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
-- Module      : Amazonka.Pinpoint.GetApnsSandboxChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the APNs sandbox
-- channel for an application.
module Amazonka.Pinpoint.GetApnsSandboxChannel
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetApnsSandboxChannel' smart constructor.
data GetApnsSandboxChannel = GetApnsSandboxChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetApnsSandboxChannel
newGetApnsSandboxChannel pApplicationId_ =
  GetApnsSandboxChannel'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getApnsSandboxChannel_applicationId :: Lens.Lens' GetApnsSandboxChannel Prelude.Text
getApnsSandboxChannel_applicationId = Lens.lens (\GetApnsSandboxChannel' {applicationId} -> applicationId) (\s@GetApnsSandboxChannel' {} a -> s {applicationId = a} :: GetApnsSandboxChannel)

instance Core.AWSRequest GetApnsSandboxChannel where
  type
    AWSResponse GetApnsSandboxChannel =
      GetApnsSandboxChannelResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApnsSandboxChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetApnsSandboxChannel where
  hashWithSalt _salt GetApnsSandboxChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetApnsSandboxChannel where
  rnf GetApnsSandboxChannel' {..} =
    Prelude.rnf applicationId

instance Data.ToHeaders GetApnsSandboxChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetApnsSandboxChannel where
  toPath GetApnsSandboxChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/channels/apns_sandbox"
      ]

instance Data.ToQuery GetApnsSandboxChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApnsSandboxChannelResponse' smart constructor.
data GetApnsSandboxChannelResponse = GetApnsSandboxChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    aPNSSandboxChannelResponse :: APNSSandboxChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
getApnsSandboxChannelResponse_httpStatus :: Lens.Lens' GetApnsSandboxChannelResponse Prelude.Int
getApnsSandboxChannelResponse_httpStatus = Lens.lens (\GetApnsSandboxChannelResponse' {httpStatus} -> httpStatus) (\s@GetApnsSandboxChannelResponse' {} a -> s {httpStatus = a} :: GetApnsSandboxChannelResponse)

-- | Undocumented member.
getApnsSandboxChannelResponse_aPNSSandboxChannelResponse :: Lens.Lens' GetApnsSandboxChannelResponse APNSSandboxChannelResponse
getApnsSandboxChannelResponse_aPNSSandboxChannelResponse = Lens.lens (\GetApnsSandboxChannelResponse' {aPNSSandboxChannelResponse} -> aPNSSandboxChannelResponse) (\s@GetApnsSandboxChannelResponse' {} a -> s {aPNSSandboxChannelResponse = a} :: GetApnsSandboxChannelResponse)

instance Prelude.NFData GetApnsSandboxChannelResponse where
  rnf GetApnsSandboxChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf aPNSSandboxChannelResponse
