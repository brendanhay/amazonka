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
-- Module      : Network.AWS.Pinpoint.DeleteGcmChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the GCM channel for an application and deletes any existing
-- settings for the channel.
module Network.AWS.Pinpoint.DeleteGcmChannel
  ( -- * Creating a Request
    DeleteGcmChannel (..),
    newDeleteGcmChannel,

    -- * Request Lenses
    deleteGcmChannel_applicationId,

    -- * Destructuring the Response
    DeleteGcmChannelResponse (..),
    newDeleteGcmChannelResponse,

    -- * Response Lenses
    deleteGcmChannelResponse_httpStatus,
    deleteGcmChannelResponse_gCMChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteGcmChannel' smart constructor.
data DeleteGcmChannel = DeleteGcmChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteGcmChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteGcmChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteGcmChannel ::
  -- | 'applicationId'
  Core.Text ->
  DeleteGcmChannel
newDeleteGcmChannel pApplicationId_ =
  DeleteGcmChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteGcmChannel_applicationId :: Lens.Lens' DeleteGcmChannel Core.Text
deleteGcmChannel_applicationId = Lens.lens (\DeleteGcmChannel' {applicationId} -> applicationId) (\s@DeleteGcmChannel' {} a -> s {applicationId = a} :: DeleteGcmChannel)

instance Core.AWSRequest DeleteGcmChannel where
  type
    AWSResponse DeleteGcmChannel =
      DeleteGcmChannelResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteGcmChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable DeleteGcmChannel

instance Core.NFData DeleteGcmChannel

instance Core.ToHeaders DeleteGcmChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteGcmChannel where
  toPath DeleteGcmChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/gcm"
      ]

instance Core.ToQuery DeleteGcmChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteGcmChannelResponse' smart constructor.
data DeleteGcmChannelResponse = DeleteGcmChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    gCMChannelResponse :: GCMChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteGcmChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteGcmChannelResponse_httpStatus' - The response's http status code.
--
-- 'gCMChannelResponse', 'deleteGcmChannelResponse_gCMChannelResponse' - Undocumented member.
newDeleteGcmChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'gCMChannelResponse'
  GCMChannelResponse ->
  DeleteGcmChannelResponse
newDeleteGcmChannelResponse
  pHttpStatus_
  pGCMChannelResponse_ =
    DeleteGcmChannelResponse'
      { httpStatus =
          pHttpStatus_,
        gCMChannelResponse = pGCMChannelResponse_
      }

-- | The response's http status code.
deleteGcmChannelResponse_httpStatus :: Lens.Lens' DeleteGcmChannelResponse Core.Int
deleteGcmChannelResponse_httpStatus = Lens.lens (\DeleteGcmChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteGcmChannelResponse' {} a -> s {httpStatus = a} :: DeleteGcmChannelResponse)

-- | Undocumented member.
deleteGcmChannelResponse_gCMChannelResponse :: Lens.Lens' DeleteGcmChannelResponse GCMChannelResponse
deleteGcmChannelResponse_gCMChannelResponse = Lens.lens (\DeleteGcmChannelResponse' {gCMChannelResponse} -> gCMChannelResponse) (\s@DeleteGcmChannelResponse' {} a -> s {gCMChannelResponse = a} :: DeleteGcmChannelResponse)

instance Core.NFData DeleteGcmChannelResponse
