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
-- Module      : Network.AWS.Pinpoint.UpdateVoiceChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the voice channel for an application or updates the status and
-- settings of the voice channel for an application.
module Network.AWS.Pinpoint.UpdateVoiceChannel
  ( -- * Creating a Request
    UpdateVoiceChannel (..),
    newUpdateVoiceChannel,

    -- * Request Lenses
    updateVoiceChannel_applicationId,
    updateVoiceChannel_voiceChannelRequest,

    -- * Destructuring the Response
    UpdateVoiceChannelResponse (..),
    newUpdateVoiceChannelResponse,

    -- * Response Lenses
    updateVoiceChannelResponse_httpStatus,
    updateVoiceChannelResponse_voiceChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateVoiceChannel' smart constructor.
data UpdateVoiceChannel = UpdateVoiceChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    voiceChannelRequest :: VoiceChannelRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateVoiceChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateVoiceChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'voiceChannelRequest', 'updateVoiceChannel_voiceChannelRequest' - Undocumented member.
newUpdateVoiceChannel ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'voiceChannelRequest'
  VoiceChannelRequest ->
  UpdateVoiceChannel
newUpdateVoiceChannel
  pApplicationId_
  pVoiceChannelRequest_ =
    UpdateVoiceChannel'
      { applicationId =
          pApplicationId_,
        voiceChannelRequest = pVoiceChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateVoiceChannel_applicationId :: Lens.Lens' UpdateVoiceChannel Core.Text
updateVoiceChannel_applicationId = Lens.lens (\UpdateVoiceChannel' {applicationId} -> applicationId) (\s@UpdateVoiceChannel' {} a -> s {applicationId = a} :: UpdateVoiceChannel)

-- | Undocumented member.
updateVoiceChannel_voiceChannelRequest :: Lens.Lens' UpdateVoiceChannel VoiceChannelRequest
updateVoiceChannel_voiceChannelRequest = Lens.lens (\UpdateVoiceChannel' {voiceChannelRequest} -> voiceChannelRequest) (\s@UpdateVoiceChannel' {} a -> s {voiceChannelRequest = a} :: UpdateVoiceChannel)

instance Core.AWSRequest UpdateVoiceChannel where
  type
    AWSResponse UpdateVoiceChannel =
      UpdateVoiceChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVoiceChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable UpdateVoiceChannel

instance Core.NFData UpdateVoiceChannel

instance Core.ToHeaders UpdateVoiceChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateVoiceChannel where
  toJSON UpdateVoiceChannel' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("VoiceChannelRequest" Core..= voiceChannelRequest)
          ]
      )

instance Core.ToPath UpdateVoiceChannel where
  toPath UpdateVoiceChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/voice"
      ]

instance Core.ToQuery UpdateVoiceChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateVoiceChannelResponse' smart constructor.
data UpdateVoiceChannelResponse = UpdateVoiceChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    voiceChannelResponse :: VoiceChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateVoiceChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateVoiceChannelResponse_httpStatus' - The response's http status code.
--
-- 'voiceChannelResponse', 'updateVoiceChannelResponse_voiceChannelResponse' - Undocumented member.
newUpdateVoiceChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'voiceChannelResponse'
  VoiceChannelResponse ->
  UpdateVoiceChannelResponse
newUpdateVoiceChannelResponse
  pHttpStatus_
  pVoiceChannelResponse_ =
    UpdateVoiceChannelResponse'
      { httpStatus =
          pHttpStatus_,
        voiceChannelResponse = pVoiceChannelResponse_
      }

-- | The response's http status code.
updateVoiceChannelResponse_httpStatus :: Lens.Lens' UpdateVoiceChannelResponse Core.Int
updateVoiceChannelResponse_httpStatus = Lens.lens (\UpdateVoiceChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateVoiceChannelResponse' {} a -> s {httpStatus = a} :: UpdateVoiceChannelResponse)

-- | Undocumented member.
updateVoiceChannelResponse_voiceChannelResponse :: Lens.Lens' UpdateVoiceChannelResponse VoiceChannelResponse
updateVoiceChannelResponse_voiceChannelResponse = Lens.lens (\UpdateVoiceChannelResponse' {voiceChannelResponse} -> voiceChannelResponse) (\s@UpdateVoiceChannelResponse' {} a -> s {voiceChannelResponse = a} :: UpdateVoiceChannelResponse)

instance Core.NFData UpdateVoiceChannelResponse
