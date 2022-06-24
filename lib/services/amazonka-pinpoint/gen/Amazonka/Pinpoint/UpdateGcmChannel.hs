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
-- Module      : Amazonka.Pinpoint.UpdateGcmChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the GCM channel for an application or updates the status and
-- settings of the GCM channel for an application.
module Amazonka.Pinpoint.UpdateGcmChannel
  ( -- * Creating a Request
    UpdateGcmChannel (..),
    newUpdateGcmChannel,

    -- * Request Lenses
    updateGcmChannel_applicationId,
    updateGcmChannel_gCMChannelRequest,

    -- * Destructuring the Response
    UpdateGcmChannelResponse (..),
    newUpdateGcmChannelResponse,

    -- * Response Lenses
    updateGcmChannelResponse_httpStatus,
    updateGcmChannelResponse_gCMChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGcmChannel' smart constructor.
data UpdateGcmChannel = UpdateGcmChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    gCMChannelRequest :: GCMChannelRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGcmChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateGcmChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'gCMChannelRequest', 'updateGcmChannel_gCMChannelRequest' - Undocumented member.
newUpdateGcmChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'gCMChannelRequest'
  GCMChannelRequest ->
  UpdateGcmChannel
newUpdateGcmChannel
  pApplicationId_
  pGCMChannelRequest_ =
    UpdateGcmChannel'
      { applicationId = pApplicationId_,
        gCMChannelRequest = pGCMChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateGcmChannel_applicationId :: Lens.Lens' UpdateGcmChannel Prelude.Text
updateGcmChannel_applicationId = Lens.lens (\UpdateGcmChannel' {applicationId} -> applicationId) (\s@UpdateGcmChannel' {} a -> s {applicationId = a} :: UpdateGcmChannel)

-- | Undocumented member.
updateGcmChannel_gCMChannelRequest :: Lens.Lens' UpdateGcmChannel GCMChannelRequest
updateGcmChannel_gCMChannelRequest = Lens.lens (\UpdateGcmChannel' {gCMChannelRequest} -> gCMChannelRequest) (\s@UpdateGcmChannel' {} a -> s {gCMChannelRequest = a} :: UpdateGcmChannel)

instance Core.AWSRequest UpdateGcmChannel where
  type
    AWSResponse UpdateGcmChannel =
      UpdateGcmChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGcmChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateGcmChannel where
  hashWithSalt _salt UpdateGcmChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` gCMChannelRequest

instance Prelude.NFData UpdateGcmChannel where
  rnf UpdateGcmChannel' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf gCMChannelRequest

instance Core.ToHeaders UpdateGcmChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateGcmChannel where
  toJSON UpdateGcmChannel' {..} =
    Core.toJSON gCMChannelRequest

instance Core.ToPath UpdateGcmChannel where
  toPath UpdateGcmChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/gcm"
      ]

instance Core.ToQuery UpdateGcmChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGcmChannelResponse' smart constructor.
data UpdateGcmChannelResponse = UpdateGcmChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    gCMChannelResponse :: GCMChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGcmChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateGcmChannelResponse_httpStatus' - The response's http status code.
--
-- 'gCMChannelResponse', 'updateGcmChannelResponse_gCMChannelResponse' - Undocumented member.
newUpdateGcmChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'gCMChannelResponse'
  GCMChannelResponse ->
  UpdateGcmChannelResponse
newUpdateGcmChannelResponse
  pHttpStatus_
  pGCMChannelResponse_ =
    UpdateGcmChannelResponse'
      { httpStatus =
          pHttpStatus_,
        gCMChannelResponse = pGCMChannelResponse_
      }

-- | The response's http status code.
updateGcmChannelResponse_httpStatus :: Lens.Lens' UpdateGcmChannelResponse Prelude.Int
updateGcmChannelResponse_httpStatus = Lens.lens (\UpdateGcmChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateGcmChannelResponse' {} a -> s {httpStatus = a} :: UpdateGcmChannelResponse)

-- | Undocumented member.
updateGcmChannelResponse_gCMChannelResponse :: Lens.Lens' UpdateGcmChannelResponse GCMChannelResponse
updateGcmChannelResponse_gCMChannelResponse = Lens.lens (\UpdateGcmChannelResponse' {gCMChannelResponse} -> gCMChannelResponse) (\s@UpdateGcmChannelResponse' {} a -> s {gCMChannelResponse = a} :: UpdateGcmChannelResponse)

instance Prelude.NFData UpdateGcmChannelResponse where
  rnf UpdateGcmChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf gCMChannelResponse
