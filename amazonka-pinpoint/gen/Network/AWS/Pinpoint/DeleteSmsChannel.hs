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
-- Module      : Network.AWS.Pinpoint.DeleteSmsChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the SMS channel for an application and deletes any existing
-- settings for the channel.
module Network.AWS.Pinpoint.DeleteSmsChannel
  ( -- * Creating a Request
    DeleteSmsChannel (..),
    newDeleteSmsChannel,

    -- * Request Lenses
    deleteSmsChannel_applicationId,

    -- * Destructuring the Response
    DeleteSmsChannelResponse (..),
    newDeleteSmsChannelResponse,

    -- * Response Lenses
    deleteSmsChannelResponse_httpStatus,
    deleteSmsChannelResponse_sMSChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSmsChannel' smart constructor.
data DeleteSmsChannel = DeleteSmsChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSmsChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteSmsChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteSmsChannel ::
  -- | 'applicationId'
  Core.Text ->
  DeleteSmsChannel
newDeleteSmsChannel pApplicationId_ =
  DeleteSmsChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteSmsChannel_applicationId :: Lens.Lens' DeleteSmsChannel Core.Text
deleteSmsChannel_applicationId = Lens.lens (\DeleteSmsChannel' {applicationId} -> applicationId) (\s@DeleteSmsChannel' {} a -> s {applicationId = a} :: DeleteSmsChannel)

instance Core.AWSRequest DeleteSmsChannel where
  type
    AWSResponse DeleteSmsChannel =
      DeleteSmsChannelResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSmsChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable DeleteSmsChannel

instance Core.NFData DeleteSmsChannel

instance Core.ToHeaders DeleteSmsChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteSmsChannel where
  toPath DeleteSmsChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/sms"
      ]

instance Core.ToQuery DeleteSmsChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteSmsChannelResponse' smart constructor.
data DeleteSmsChannelResponse = DeleteSmsChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    sMSChannelResponse :: SMSChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSmsChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSmsChannelResponse_httpStatus' - The response's http status code.
--
-- 'sMSChannelResponse', 'deleteSmsChannelResponse_sMSChannelResponse' - Undocumented member.
newDeleteSmsChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'sMSChannelResponse'
  SMSChannelResponse ->
  DeleteSmsChannelResponse
newDeleteSmsChannelResponse
  pHttpStatus_
  pSMSChannelResponse_ =
    DeleteSmsChannelResponse'
      { httpStatus =
          pHttpStatus_,
        sMSChannelResponse = pSMSChannelResponse_
      }

-- | The response's http status code.
deleteSmsChannelResponse_httpStatus :: Lens.Lens' DeleteSmsChannelResponse Core.Int
deleteSmsChannelResponse_httpStatus = Lens.lens (\DeleteSmsChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteSmsChannelResponse' {} a -> s {httpStatus = a} :: DeleteSmsChannelResponse)

-- | Undocumented member.
deleteSmsChannelResponse_sMSChannelResponse :: Lens.Lens' DeleteSmsChannelResponse SMSChannelResponse
deleteSmsChannelResponse_sMSChannelResponse = Lens.lens (\DeleteSmsChannelResponse' {sMSChannelResponse} -> sMSChannelResponse) (\s@DeleteSmsChannelResponse' {} a -> s {sMSChannelResponse = a} :: DeleteSmsChannelResponse)

instance Core.NFData DeleteSmsChannelResponse
