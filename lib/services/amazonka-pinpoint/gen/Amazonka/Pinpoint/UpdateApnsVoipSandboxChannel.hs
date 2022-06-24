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
-- Module      : Amazonka.Pinpoint.UpdateApnsVoipSandboxChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs VoIP sandbox channel for an application or updates the
-- status and settings of the APNs VoIP sandbox channel for an application.
module Amazonka.Pinpoint.UpdateApnsVoipSandboxChannel
  ( -- * Creating a Request
    UpdateApnsVoipSandboxChannel (..),
    newUpdateApnsVoipSandboxChannel,

    -- * Request Lenses
    updateApnsVoipSandboxChannel_applicationId,
    updateApnsVoipSandboxChannel_aPNSVoipSandboxChannelRequest,

    -- * Destructuring the Response
    UpdateApnsVoipSandboxChannelResponse (..),
    newUpdateApnsVoipSandboxChannelResponse,

    -- * Response Lenses
    updateApnsVoipSandboxChannelResponse_httpStatus,
    updateApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApnsVoipSandboxChannel' smart constructor.
data UpdateApnsVoipSandboxChannel = UpdateApnsVoipSandboxChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    aPNSVoipSandboxChannelRequest :: APNSVoipSandboxChannelRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApnsVoipSandboxChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateApnsVoipSandboxChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'aPNSVoipSandboxChannelRequest', 'updateApnsVoipSandboxChannel_aPNSVoipSandboxChannelRequest' - Undocumented member.
newUpdateApnsVoipSandboxChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'aPNSVoipSandboxChannelRequest'
  APNSVoipSandboxChannelRequest ->
  UpdateApnsVoipSandboxChannel
newUpdateApnsVoipSandboxChannel
  pApplicationId_
  pAPNSVoipSandboxChannelRequest_ =
    UpdateApnsVoipSandboxChannel'
      { applicationId =
          pApplicationId_,
        aPNSVoipSandboxChannelRequest =
          pAPNSVoipSandboxChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateApnsVoipSandboxChannel_applicationId :: Lens.Lens' UpdateApnsVoipSandboxChannel Prelude.Text
updateApnsVoipSandboxChannel_applicationId = Lens.lens (\UpdateApnsVoipSandboxChannel' {applicationId} -> applicationId) (\s@UpdateApnsVoipSandboxChannel' {} a -> s {applicationId = a} :: UpdateApnsVoipSandboxChannel)

-- | Undocumented member.
updateApnsVoipSandboxChannel_aPNSVoipSandboxChannelRequest :: Lens.Lens' UpdateApnsVoipSandboxChannel APNSVoipSandboxChannelRequest
updateApnsVoipSandboxChannel_aPNSVoipSandboxChannelRequest = Lens.lens (\UpdateApnsVoipSandboxChannel' {aPNSVoipSandboxChannelRequest} -> aPNSVoipSandboxChannelRequest) (\s@UpdateApnsVoipSandboxChannel' {} a -> s {aPNSVoipSandboxChannelRequest = a} :: UpdateApnsVoipSandboxChannel)

instance Core.AWSRequest UpdateApnsVoipSandboxChannel where
  type
    AWSResponse UpdateApnsVoipSandboxChannel =
      UpdateApnsVoipSandboxChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApnsVoipSandboxChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance
  Prelude.Hashable
    UpdateApnsVoipSandboxChannel
  where
  hashWithSalt _salt UpdateApnsVoipSandboxChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` aPNSVoipSandboxChannelRequest

instance Prelude.NFData UpdateApnsVoipSandboxChannel where
  rnf UpdateApnsVoipSandboxChannel' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf aPNSVoipSandboxChannelRequest

instance Core.ToHeaders UpdateApnsVoipSandboxChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateApnsVoipSandboxChannel where
  toJSON UpdateApnsVoipSandboxChannel' {..} =
    Core.toJSON aPNSVoipSandboxChannelRequest

instance Core.ToPath UpdateApnsVoipSandboxChannel where
  toPath UpdateApnsVoipSandboxChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns_voip_sandbox"
      ]

instance Core.ToQuery UpdateApnsVoipSandboxChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApnsVoipSandboxChannelResponse' smart constructor.
data UpdateApnsVoipSandboxChannelResponse = UpdateApnsVoipSandboxChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    aPNSVoipSandboxChannelResponse :: APNSVoipSandboxChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApnsVoipSandboxChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApnsVoipSandboxChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSVoipSandboxChannelResponse', 'updateApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse' - Undocumented member.
newUpdateApnsVoipSandboxChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'aPNSVoipSandboxChannelResponse'
  APNSVoipSandboxChannelResponse ->
  UpdateApnsVoipSandboxChannelResponse
newUpdateApnsVoipSandboxChannelResponse
  pHttpStatus_
  pAPNSVoipSandboxChannelResponse_ =
    UpdateApnsVoipSandboxChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSVoipSandboxChannelResponse =
          pAPNSVoipSandboxChannelResponse_
      }

-- | The response's http status code.
updateApnsVoipSandboxChannelResponse_httpStatus :: Lens.Lens' UpdateApnsVoipSandboxChannelResponse Prelude.Int
updateApnsVoipSandboxChannelResponse_httpStatus = Lens.lens (\UpdateApnsVoipSandboxChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateApnsVoipSandboxChannelResponse' {} a -> s {httpStatus = a} :: UpdateApnsVoipSandboxChannelResponse)

-- | Undocumented member.
updateApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse :: Lens.Lens' UpdateApnsVoipSandboxChannelResponse APNSVoipSandboxChannelResponse
updateApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse = Lens.lens (\UpdateApnsVoipSandboxChannelResponse' {aPNSVoipSandboxChannelResponse} -> aPNSVoipSandboxChannelResponse) (\s@UpdateApnsVoipSandboxChannelResponse' {} a -> s {aPNSVoipSandboxChannelResponse = a} :: UpdateApnsVoipSandboxChannelResponse)

instance
  Prelude.NFData
    UpdateApnsVoipSandboxChannelResponse
  where
  rnf UpdateApnsVoipSandboxChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf aPNSVoipSandboxChannelResponse
