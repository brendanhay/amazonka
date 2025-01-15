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
-- Module      : Amazonka.Pinpoint.GetVoiceChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the voice channel
-- for an application.
module Amazonka.Pinpoint.GetVoiceChannel
  ( -- * Creating a Request
    GetVoiceChannel (..),
    newGetVoiceChannel,

    -- * Request Lenses
    getVoiceChannel_applicationId,

    -- * Destructuring the Response
    GetVoiceChannelResponse (..),
    newGetVoiceChannelResponse,

    -- * Response Lenses
    getVoiceChannelResponse_httpStatus,
    getVoiceChannelResponse_voiceChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVoiceChannel' smart constructor.
data GetVoiceChannel = GetVoiceChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getVoiceChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetVoiceChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  GetVoiceChannel
newGetVoiceChannel pApplicationId_ =
  GetVoiceChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getVoiceChannel_applicationId :: Lens.Lens' GetVoiceChannel Prelude.Text
getVoiceChannel_applicationId = Lens.lens (\GetVoiceChannel' {applicationId} -> applicationId) (\s@GetVoiceChannel' {} a -> s {applicationId = a} :: GetVoiceChannel)

instance Core.AWSRequest GetVoiceChannel where
  type
    AWSResponse GetVoiceChannel =
      GetVoiceChannelResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVoiceChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetVoiceChannel where
  hashWithSalt _salt GetVoiceChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetVoiceChannel where
  rnf GetVoiceChannel' {..} = Prelude.rnf applicationId

instance Data.ToHeaders GetVoiceChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetVoiceChannel where
  toPath GetVoiceChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/channels/voice"
      ]

instance Data.ToQuery GetVoiceChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVoiceChannelResponse' smart constructor.
data GetVoiceChannelResponse = GetVoiceChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    voiceChannelResponse :: VoiceChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getVoiceChannelResponse_httpStatus' - The response's http status code.
--
-- 'voiceChannelResponse', 'getVoiceChannelResponse_voiceChannelResponse' - Undocumented member.
newGetVoiceChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'voiceChannelResponse'
  VoiceChannelResponse ->
  GetVoiceChannelResponse
newGetVoiceChannelResponse
  pHttpStatus_
  pVoiceChannelResponse_ =
    GetVoiceChannelResponse'
      { httpStatus = pHttpStatus_,
        voiceChannelResponse = pVoiceChannelResponse_
      }

-- | The response's http status code.
getVoiceChannelResponse_httpStatus :: Lens.Lens' GetVoiceChannelResponse Prelude.Int
getVoiceChannelResponse_httpStatus = Lens.lens (\GetVoiceChannelResponse' {httpStatus} -> httpStatus) (\s@GetVoiceChannelResponse' {} a -> s {httpStatus = a} :: GetVoiceChannelResponse)

-- | Undocumented member.
getVoiceChannelResponse_voiceChannelResponse :: Lens.Lens' GetVoiceChannelResponse VoiceChannelResponse
getVoiceChannelResponse_voiceChannelResponse = Lens.lens (\GetVoiceChannelResponse' {voiceChannelResponse} -> voiceChannelResponse) (\s@GetVoiceChannelResponse' {} a -> s {voiceChannelResponse = a} :: GetVoiceChannelResponse)

instance Prelude.NFData GetVoiceChannelResponse where
  rnf GetVoiceChannelResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf voiceChannelResponse
