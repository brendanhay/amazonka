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
-- Module      : Amazonka.Pinpoint.GetGcmChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the GCM channel
-- for an application.
module Amazonka.Pinpoint.GetGcmChannel
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGcmChannel' smart constructor.
data GetGcmChannel = GetGcmChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetGcmChannel
newGetGcmChannel pApplicationId_ =
  GetGcmChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getGcmChannel_applicationId :: Lens.Lens' GetGcmChannel Prelude.Text
getGcmChannel_applicationId = Lens.lens (\GetGcmChannel' {applicationId} -> applicationId) (\s@GetGcmChannel' {} a -> s {applicationId = a} :: GetGcmChannel)

instance Core.AWSRequest GetGcmChannel where
  type
    AWSResponse GetGcmChannel =
      GetGcmChannelResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGcmChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetGcmChannel where
  hashWithSalt _salt GetGcmChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetGcmChannel where
  rnf GetGcmChannel' {..} = Prelude.rnf applicationId

instance Data.ToHeaders GetGcmChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetGcmChannel where
  toPath GetGcmChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/channels/gcm"
      ]

instance Data.ToQuery GetGcmChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGcmChannelResponse' smart constructor.
data GetGcmChannelResponse = GetGcmChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    gCMChannelResponse :: GCMChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
getGcmChannelResponse_httpStatus :: Lens.Lens' GetGcmChannelResponse Prelude.Int
getGcmChannelResponse_httpStatus = Lens.lens (\GetGcmChannelResponse' {httpStatus} -> httpStatus) (\s@GetGcmChannelResponse' {} a -> s {httpStatus = a} :: GetGcmChannelResponse)

-- | Undocumented member.
getGcmChannelResponse_gCMChannelResponse :: Lens.Lens' GetGcmChannelResponse GCMChannelResponse
getGcmChannelResponse_gCMChannelResponse = Lens.lens (\GetGcmChannelResponse' {gCMChannelResponse} -> gCMChannelResponse) (\s@GetGcmChannelResponse' {} a -> s {gCMChannelResponse = a} :: GetGcmChannelResponse)

instance Prelude.NFData GetGcmChannelResponse where
  rnf GetGcmChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf gCMChannelResponse
