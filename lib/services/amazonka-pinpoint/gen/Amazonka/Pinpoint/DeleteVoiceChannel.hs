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
-- Module      : Amazonka.Pinpoint.DeleteVoiceChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the voice channel for an application and deletes any existing
-- settings for the channel.
module Amazonka.Pinpoint.DeleteVoiceChannel
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVoiceChannel' smart constructor.
data DeleteVoiceChannel = DeleteVoiceChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteVoiceChannel
newDeleteVoiceChannel pApplicationId_ =
  DeleteVoiceChannel'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteVoiceChannel_applicationId :: Lens.Lens' DeleteVoiceChannel Prelude.Text
deleteVoiceChannel_applicationId = Lens.lens (\DeleteVoiceChannel' {applicationId} -> applicationId) (\s@DeleteVoiceChannel' {} a -> s {applicationId = a} :: DeleteVoiceChannel)

instance Core.AWSRequest DeleteVoiceChannel where
  type
    AWSResponse DeleteVoiceChannel =
      DeleteVoiceChannelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVoiceChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteVoiceChannel where
  hashWithSalt _salt DeleteVoiceChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DeleteVoiceChannel where
  rnf DeleteVoiceChannel' {..} =
    Prelude.rnf applicationId

instance Data.ToHeaders DeleteVoiceChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteVoiceChannel where
  toPath DeleteVoiceChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/channels/voice"
      ]

instance Data.ToQuery DeleteVoiceChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVoiceChannelResponse' smart constructor.
data DeleteVoiceChannelResponse = DeleteVoiceChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    voiceChannelResponse :: VoiceChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
deleteVoiceChannelResponse_httpStatus :: Lens.Lens' DeleteVoiceChannelResponse Prelude.Int
deleteVoiceChannelResponse_httpStatus = Lens.lens (\DeleteVoiceChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteVoiceChannelResponse' {} a -> s {httpStatus = a} :: DeleteVoiceChannelResponse)

-- | Undocumented member.
deleteVoiceChannelResponse_voiceChannelResponse :: Lens.Lens' DeleteVoiceChannelResponse VoiceChannelResponse
deleteVoiceChannelResponse_voiceChannelResponse = Lens.lens (\DeleteVoiceChannelResponse' {voiceChannelResponse} -> voiceChannelResponse) (\s@DeleteVoiceChannelResponse' {} a -> s {voiceChannelResponse = a} :: DeleteVoiceChannelResponse)

instance Prelude.NFData DeleteVoiceChannelResponse where
  rnf DeleteVoiceChannelResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf voiceChannelResponse
