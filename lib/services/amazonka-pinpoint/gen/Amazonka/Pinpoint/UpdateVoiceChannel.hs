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
-- Module      : Amazonka.Pinpoint.UpdateVoiceChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the voice channel for an application or updates the status and
-- settings of the voice channel for an application.
module Amazonka.Pinpoint.UpdateVoiceChannel
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVoiceChannel' smart constructor.
data UpdateVoiceChannel = UpdateVoiceChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    voiceChannelRequest :: VoiceChannelRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
updateVoiceChannel_applicationId :: Lens.Lens' UpdateVoiceChannel Prelude.Text
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateVoiceChannel where
  hashWithSalt _salt UpdateVoiceChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` voiceChannelRequest

instance Prelude.NFData UpdateVoiceChannel where
  rnf UpdateVoiceChannel' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf voiceChannelRequest

instance Core.ToHeaders UpdateVoiceChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateVoiceChannel where
  toJSON UpdateVoiceChannel' {..} =
    Core.toJSON voiceChannelRequest

instance Core.ToPath UpdateVoiceChannel where
  toPath UpdateVoiceChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/voice"
      ]

instance Core.ToQuery UpdateVoiceChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVoiceChannelResponse' smart constructor.
data UpdateVoiceChannelResponse = UpdateVoiceChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    voiceChannelResponse :: VoiceChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
updateVoiceChannelResponse_httpStatus :: Lens.Lens' UpdateVoiceChannelResponse Prelude.Int
updateVoiceChannelResponse_httpStatus = Lens.lens (\UpdateVoiceChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateVoiceChannelResponse' {} a -> s {httpStatus = a} :: UpdateVoiceChannelResponse)

-- | Undocumented member.
updateVoiceChannelResponse_voiceChannelResponse :: Lens.Lens' UpdateVoiceChannelResponse VoiceChannelResponse
updateVoiceChannelResponse_voiceChannelResponse = Lens.lens (\UpdateVoiceChannelResponse' {voiceChannelResponse} -> voiceChannelResponse) (\s@UpdateVoiceChannelResponse' {} a -> s {voiceChannelResponse = a} :: UpdateVoiceChannelResponse)

instance Prelude.NFData UpdateVoiceChannelResponse where
  rnf UpdateVoiceChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf voiceChannelResponse
