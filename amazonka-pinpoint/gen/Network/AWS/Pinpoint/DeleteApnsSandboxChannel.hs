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
-- Module      : Network.AWS.Pinpoint.DeleteApnsSandboxChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the APNs sandbox channel for an application and deletes any
-- existing settings for the channel.
module Network.AWS.Pinpoint.DeleteApnsSandboxChannel
  ( -- * Creating a Request
    DeleteApnsSandboxChannel (..),
    newDeleteApnsSandboxChannel,

    -- * Request Lenses
    deleteApnsSandboxChannel_applicationId,

    -- * Destructuring the Response
    DeleteApnsSandboxChannelResponse (..),
    newDeleteApnsSandboxChannelResponse,

    -- * Response Lenses
    deleteApnsSandboxChannelResponse_httpStatus,
    deleteApnsSandboxChannelResponse_aPNSSandboxChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteApnsSandboxChannel' smart constructor.
data DeleteApnsSandboxChannel = DeleteApnsSandboxChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApnsSandboxChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteApnsSandboxChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteApnsSandboxChannel ::
  -- | 'applicationId'
  Core.Text ->
  DeleteApnsSandboxChannel
newDeleteApnsSandboxChannel pApplicationId_ =
  DeleteApnsSandboxChannel'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteApnsSandboxChannel_applicationId :: Lens.Lens' DeleteApnsSandboxChannel Core.Text
deleteApnsSandboxChannel_applicationId = Lens.lens (\DeleteApnsSandboxChannel' {applicationId} -> applicationId) (\s@DeleteApnsSandboxChannel' {} a -> s {applicationId = a} :: DeleteApnsSandboxChannel)

instance Core.AWSRequest DeleteApnsSandboxChannel where
  type
    AWSResponse DeleteApnsSandboxChannel =
      DeleteApnsSandboxChannelResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteApnsSandboxChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable DeleteApnsSandboxChannel

instance Core.NFData DeleteApnsSandboxChannel

instance Core.ToHeaders DeleteApnsSandboxChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteApnsSandboxChannel where
  toPath DeleteApnsSandboxChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns_sandbox"
      ]

instance Core.ToQuery DeleteApnsSandboxChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteApnsSandboxChannelResponse' smart constructor.
data DeleteApnsSandboxChannelResponse = DeleteApnsSandboxChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    aPNSSandboxChannelResponse :: APNSSandboxChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApnsSandboxChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApnsSandboxChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSSandboxChannelResponse', 'deleteApnsSandboxChannelResponse_aPNSSandboxChannelResponse' - Undocumented member.
newDeleteApnsSandboxChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'aPNSSandboxChannelResponse'
  APNSSandboxChannelResponse ->
  DeleteApnsSandboxChannelResponse
newDeleteApnsSandboxChannelResponse
  pHttpStatus_
  pAPNSSandboxChannelResponse_ =
    DeleteApnsSandboxChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSSandboxChannelResponse =
          pAPNSSandboxChannelResponse_
      }

-- | The response's http status code.
deleteApnsSandboxChannelResponse_httpStatus :: Lens.Lens' DeleteApnsSandboxChannelResponse Core.Int
deleteApnsSandboxChannelResponse_httpStatus = Lens.lens (\DeleteApnsSandboxChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteApnsSandboxChannelResponse' {} a -> s {httpStatus = a} :: DeleteApnsSandboxChannelResponse)

-- | Undocumented member.
deleteApnsSandboxChannelResponse_aPNSSandboxChannelResponse :: Lens.Lens' DeleteApnsSandboxChannelResponse APNSSandboxChannelResponse
deleteApnsSandboxChannelResponse_aPNSSandboxChannelResponse = Lens.lens (\DeleteApnsSandboxChannelResponse' {aPNSSandboxChannelResponse} -> aPNSSandboxChannelResponse) (\s@DeleteApnsSandboxChannelResponse' {} a -> s {aPNSSandboxChannelResponse = a} :: DeleteApnsSandboxChannelResponse)

instance Core.NFData DeleteApnsSandboxChannelResponse
