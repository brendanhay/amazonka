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
-- Module      : Network.AWS.Pinpoint.DeleteApnsVoipChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the APNs VoIP channel for an application and deletes any
-- existing settings for the channel.
module Network.AWS.Pinpoint.DeleteApnsVoipChannel
  ( -- * Creating a Request
    DeleteApnsVoipChannel (..),
    newDeleteApnsVoipChannel,

    -- * Request Lenses
    deleteApnsVoipChannel_applicationId,

    -- * Destructuring the Response
    DeleteApnsVoipChannelResponse (..),
    newDeleteApnsVoipChannelResponse,

    -- * Response Lenses
    deleteApnsVoipChannelResponse_httpStatus,
    deleteApnsVoipChannelResponse_aPNSVoipChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteApnsVoipChannel' smart constructor.
data DeleteApnsVoipChannel = DeleteApnsVoipChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApnsVoipChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteApnsVoipChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteApnsVoipChannel ::
  -- | 'applicationId'
  Core.Text ->
  DeleteApnsVoipChannel
newDeleteApnsVoipChannel pApplicationId_ =
  DeleteApnsVoipChannel'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteApnsVoipChannel_applicationId :: Lens.Lens' DeleteApnsVoipChannel Core.Text
deleteApnsVoipChannel_applicationId = Lens.lens (\DeleteApnsVoipChannel' {applicationId} -> applicationId) (\s@DeleteApnsVoipChannel' {} a -> s {applicationId = a} :: DeleteApnsVoipChannel)

instance Core.AWSRequest DeleteApnsVoipChannel where
  type
    AWSResponse DeleteApnsVoipChannel =
      DeleteApnsVoipChannelResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteApnsVoipChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable DeleteApnsVoipChannel

instance Core.NFData DeleteApnsVoipChannel

instance Core.ToHeaders DeleteApnsVoipChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteApnsVoipChannel where
  toPath DeleteApnsVoipChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns_voip"
      ]

instance Core.ToQuery DeleteApnsVoipChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteApnsVoipChannelResponse' smart constructor.
data DeleteApnsVoipChannelResponse = DeleteApnsVoipChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    aPNSVoipChannelResponse :: APNSVoipChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApnsVoipChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApnsVoipChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSVoipChannelResponse', 'deleteApnsVoipChannelResponse_aPNSVoipChannelResponse' - Undocumented member.
newDeleteApnsVoipChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'aPNSVoipChannelResponse'
  APNSVoipChannelResponse ->
  DeleteApnsVoipChannelResponse
newDeleteApnsVoipChannelResponse
  pHttpStatus_
  pAPNSVoipChannelResponse_ =
    DeleteApnsVoipChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSVoipChannelResponse =
          pAPNSVoipChannelResponse_
      }

-- | The response's http status code.
deleteApnsVoipChannelResponse_httpStatus :: Lens.Lens' DeleteApnsVoipChannelResponse Core.Int
deleteApnsVoipChannelResponse_httpStatus = Lens.lens (\DeleteApnsVoipChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteApnsVoipChannelResponse' {} a -> s {httpStatus = a} :: DeleteApnsVoipChannelResponse)

-- | Undocumented member.
deleteApnsVoipChannelResponse_aPNSVoipChannelResponse :: Lens.Lens' DeleteApnsVoipChannelResponse APNSVoipChannelResponse
deleteApnsVoipChannelResponse_aPNSVoipChannelResponse = Lens.lens (\DeleteApnsVoipChannelResponse' {aPNSVoipChannelResponse} -> aPNSVoipChannelResponse) (\s@DeleteApnsVoipChannelResponse' {} a -> s {aPNSVoipChannelResponse = a} :: DeleteApnsVoipChannelResponse)

instance Core.NFData DeleteApnsVoipChannelResponse
