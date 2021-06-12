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
-- Module      : Network.AWS.Pinpoint.DeleteApnsVoipSandboxChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the APNs VoIP sandbox channel for an application and deletes
-- any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteApnsVoipSandboxChannel
  ( -- * Creating a Request
    DeleteApnsVoipSandboxChannel (..),
    newDeleteApnsVoipSandboxChannel,

    -- * Request Lenses
    deleteApnsVoipSandboxChannel_applicationId,

    -- * Destructuring the Response
    DeleteApnsVoipSandboxChannelResponse (..),
    newDeleteApnsVoipSandboxChannelResponse,

    -- * Response Lenses
    deleteApnsVoipSandboxChannelResponse_httpStatus,
    deleteApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteApnsVoipSandboxChannel' smart constructor.
data DeleteApnsVoipSandboxChannel = DeleteApnsVoipSandboxChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApnsVoipSandboxChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteApnsVoipSandboxChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteApnsVoipSandboxChannel ::
  -- | 'applicationId'
  Core.Text ->
  DeleteApnsVoipSandboxChannel
newDeleteApnsVoipSandboxChannel pApplicationId_ =
  DeleteApnsVoipSandboxChannel'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteApnsVoipSandboxChannel_applicationId :: Lens.Lens' DeleteApnsVoipSandboxChannel Core.Text
deleteApnsVoipSandboxChannel_applicationId = Lens.lens (\DeleteApnsVoipSandboxChannel' {applicationId} -> applicationId) (\s@DeleteApnsVoipSandboxChannel' {} a -> s {applicationId = a} :: DeleteApnsVoipSandboxChannel)

instance Core.AWSRequest DeleteApnsVoipSandboxChannel where
  type
    AWSResponse DeleteApnsVoipSandboxChannel =
      DeleteApnsVoipSandboxChannelResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteApnsVoipSandboxChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable DeleteApnsVoipSandboxChannel

instance Core.NFData DeleteApnsVoipSandboxChannel

instance Core.ToHeaders DeleteApnsVoipSandboxChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteApnsVoipSandboxChannel where
  toPath DeleteApnsVoipSandboxChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns_voip_sandbox"
      ]

instance Core.ToQuery DeleteApnsVoipSandboxChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteApnsVoipSandboxChannelResponse' smart constructor.
data DeleteApnsVoipSandboxChannelResponse = DeleteApnsVoipSandboxChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    aPNSVoipSandboxChannelResponse :: APNSVoipSandboxChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApnsVoipSandboxChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApnsVoipSandboxChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSVoipSandboxChannelResponse', 'deleteApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse' - Undocumented member.
newDeleteApnsVoipSandboxChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'aPNSVoipSandboxChannelResponse'
  APNSVoipSandboxChannelResponse ->
  DeleteApnsVoipSandboxChannelResponse
newDeleteApnsVoipSandboxChannelResponse
  pHttpStatus_
  pAPNSVoipSandboxChannelResponse_ =
    DeleteApnsVoipSandboxChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSVoipSandboxChannelResponse =
          pAPNSVoipSandboxChannelResponse_
      }

-- | The response's http status code.
deleteApnsVoipSandboxChannelResponse_httpStatus :: Lens.Lens' DeleteApnsVoipSandboxChannelResponse Core.Int
deleteApnsVoipSandboxChannelResponse_httpStatus = Lens.lens (\DeleteApnsVoipSandboxChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteApnsVoipSandboxChannelResponse' {} a -> s {httpStatus = a} :: DeleteApnsVoipSandboxChannelResponse)

-- | Undocumented member.
deleteApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse :: Lens.Lens' DeleteApnsVoipSandboxChannelResponse APNSVoipSandboxChannelResponse
deleteApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse = Lens.lens (\DeleteApnsVoipSandboxChannelResponse' {aPNSVoipSandboxChannelResponse} -> aPNSVoipSandboxChannelResponse) (\s@DeleteApnsVoipSandboxChannelResponse' {} a -> s {aPNSVoipSandboxChannelResponse = a} :: DeleteApnsVoipSandboxChannelResponse)

instance
  Core.NFData
    DeleteApnsVoipSandboxChannelResponse
