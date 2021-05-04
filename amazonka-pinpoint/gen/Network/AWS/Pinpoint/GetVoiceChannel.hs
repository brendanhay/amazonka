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
-- Module      : Network.AWS.Pinpoint.GetVoiceChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the voice channel
-- for an application.
module Network.AWS.Pinpoint.GetVoiceChannel
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetVoiceChannel' smart constructor.
data GetVoiceChannel = GetVoiceChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetVoiceChannel where
  type Rs GetVoiceChannel = GetVoiceChannelResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVoiceChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable GetVoiceChannel

instance Prelude.NFData GetVoiceChannel

instance Prelude.ToHeaders GetVoiceChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetVoiceChannel where
  toPath GetVoiceChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Prelude.toBS applicationId,
        "/channels/voice"
      ]

instance Prelude.ToQuery GetVoiceChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVoiceChannelResponse' smart constructor.
data GetVoiceChannelResponse = GetVoiceChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    voiceChannelResponse :: VoiceChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData GetVoiceChannelResponse
