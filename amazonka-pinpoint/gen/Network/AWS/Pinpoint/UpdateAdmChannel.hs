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
-- Module      : Network.AWS.Pinpoint.UpdateAdmChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the ADM channel for an application or updates the status and
-- settings of the ADM channel for an application.
module Network.AWS.Pinpoint.UpdateAdmChannel
  ( -- * Creating a Request
    UpdateAdmChannel (..),
    newUpdateAdmChannel,

    -- * Request Lenses
    updateAdmChannel_applicationId,
    updateAdmChannel_aDMChannelRequest,

    -- * Destructuring the Response
    UpdateAdmChannelResponse (..),
    newUpdateAdmChannelResponse,

    -- * Response Lenses
    updateAdmChannelResponse_httpStatus,
    updateAdmChannelResponse_aDMChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateAdmChannel' smart constructor.
data UpdateAdmChannel = UpdateAdmChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    aDMChannelRequest :: ADMChannelRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAdmChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateAdmChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'aDMChannelRequest', 'updateAdmChannel_aDMChannelRequest' - Undocumented member.
newUpdateAdmChannel ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'aDMChannelRequest'
  ADMChannelRequest ->
  UpdateAdmChannel
newUpdateAdmChannel
  pApplicationId_
  pADMChannelRequest_ =
    UpdateAdmChannel'
      { applicationId = pApplicationId_,
        aDMChannelRequest = pADMChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateAdmChannel_applicationId :: Lens.Lens' UpdateAdmChannel Core.Text
updateAdmChannel_applicationId = Lens.lens (\UpdateAdmChannel' {applicationId} -> applicationId) (\s@UpdateAdmChannel' {} a -> s {applicationId = a} :: UpdateAdmChannel)

-- | Undocumented member.
updateAdmChannel_aDMChannelRequest :: Lens.Lens' UpdateAdmChannel ADMChannelRequest
updateAdmChannel_aDMChannelRequest = Lens.lens (\UpdateAdmChannel' {aDMChannelRequest} -> aDMChannelRequest) (\s@UpdateAdmChannel' {} a -> s {aDMChannelRequest = a} :: UpdateAdmChannel)

instance Core.AWSRequest UpdateAdmChannel where
  type
    AWSResponse UpdateAdmChannel =
      UpdateAdmChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAdmChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable UpdateAdmChannel

instance Core.NFData UpdateAdmChannel

instance Core.ToHeaders UpdateAdmChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateAdmChannel where
  toJSON UpdateAdmChannel' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ADMChannelRequest" Core..= aDMChannelRequest)
          ]
      )

instance Core.ToPath UpdateAdmChannel where
  toPath UpdateAdmChannel' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/adm"
      ]

instance Core.ToQuery UpdateAdmChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateAdmChannelResponse' smart constructor.
data UpdateAdmChannelResponse = UpdateAdmChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    aDMChannelResponse :: ADMChannelResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAdmChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAdmChannelResponse_httpStatus' - The response's http status code.
--
-- 'aDMChannelResponse', 'updateAdmChannelResponse_aDMChannelResponse' - Undocumented member.
newUpdateAdmChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'aDMChannelResponse'
  ADMChannelResponse ->
  UpdateAdmChannelResponse
newUpdateAdmChannelResponse
  pHttpStatus_
  pADMChannelResponse_ =
    UpdateAdmChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aDMChannelResponse = pADMChannelResponse_
      }

-- | The response's http status code.
updateAdmChannelResponse_httpStatus :: Lens.Lens' UpdateAdmChannelResponse Core.Int
updateAdmChannelResponse_httpStatus = Lens.lens (\UpdateAdmChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateAdmChannelResponse' {} a -> s {httpStatus = a} :: UpdateAdmChannelResponse)

-- | Undocumented member.
updateAdmChannelResponse_aDMChannelResponse :: Lens.Lens' UpdateAdmChannelResponse ADMChannelResponse
updateAdmChannelResponse_aDMChannelResponse = Lens.lens (\UpdateAdmChannelResponse' {aDMChannelResponse} -> aDMChannelResponse) (\s@UpdateAdmChannelResponse' {} a -> s {aDMChannelResponse = a} :: UpdateAdmChannelResponse)

instance Core.NFData UpdateAdmChannelResponse
