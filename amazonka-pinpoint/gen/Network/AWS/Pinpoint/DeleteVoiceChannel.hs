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
-- Module      : Network.AWS.Pinpoint.DeleteVoiceChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the voice channel for an application and deletes any existing
-- settings for the channel.
module Network.AWS.Pinpoint.DeleteVoiceChannel
  ( -- * Creating a Request
    DeleteVoiceChannel (..),
    newDeleteVoiceChannel,

    -- * Request Lenses
    deleteVoiceChannel_applicationId,

    -- * Destructuring the Response
    DeleteVoiceChannelResponse (..),
    newDeleteVoiceChannelResponse,

    -- * Response Lenses
    deleteVoiceChannelResponse_httpStatus,
    deleteVoiceChannelResponse_voiceChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteVoiceChannel' smart constructor.
data DeleteVoiceChannel = DeleteVoiceChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVoiceChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteVoiceChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteVoiceChannel ::
  -- | 'applicationId'
  Core.Text ->
  DeleteVoiceChannel
newDeleteVoiceChannel pApplicationId_ =
  DeleteVoiceChannel'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteVoiceChannel_applicationId :: Lens.Lens' DeleteVoiceChannel Core.Text
deleteVoiceChannel_applicationId = Lens.lens (\DeleteVoiceChannel' {applicationId} -> applicationId) (\s@DeleteVoiceChannel' {} a -> s {applicationId = a} :: DeleteVoiceChannel)

instance Core.AWSRequest DeleteVoiceChannel where
  type
    AWSResponse DeleteVoiceChannel =
      DeleteVoiceChannelResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVoiceChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable DeleteVoiceChannel

instance Core.NFData DeleteVoiceChannel

instance Core.ToHeaders DeleteVoiceChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteVoiceChannel where
  toPath DeleteVoiceChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/voice"
      ]

instance Core.ToQuery DeleteVoiceChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteVoiceChannelResponse' smart constructor.
data DeleteVoiceChannelResponse = DeleteVoiceChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    voiceChannelResponse :: VoiceChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVoiceChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVoiceChannelResponse_httpStatus' - The response's http status code.
--
-- 'voiceChannelResponse', 'deleteVoiceChannelResponse_voiceChannelResponse' - Undocumented member.
newDeleteVoiceChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'voiceChannelResponse'
  VoiceChannelResponse ->
  DeleteVoiceChannelResponse
newDeleteVoiceChannelResponse
  pHttpStatus_
  pVoiceChannelResponse_ =
    DeleteVoiceChannelResponse'
      { httpStatus =
          pHttpStatus_,
        voiceChannelResponse = pVoiceChannelResponse_
      }

-- | The response's http status code.
deleteVoiceChannelResponse_httpStatus :: Lens.Lens' DeleteVoiceChannelResponse Core.Int
deleteVoiceChannelResponse_httpStatus = Lens.lens (\DeleteVoiceChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteVoiceChannelResponse' {} a -> s {httpStatus = a} :: DeleteVoiceChannelResponse)

-- | Undocumented member.
deleteVoiceChannelResponse_voiceChannelResponse :: Lens.Lens' DeleteVoiceChannelResponse VoiceChannelResponse
deleteVoiceChannelResponse_voiceChannelResponse = Lens.lens (\DeleteVoiceChannelResponse' {voiceChannelResponse} -> voiceChannelResponse) (\s@DeleteVoiceChannelResponse' {} a -> s {voiceChannelResponse = a} :: DeleteVoiceChannelResponse)

instance Core.NFData DeleteVoiceChannelResponse
