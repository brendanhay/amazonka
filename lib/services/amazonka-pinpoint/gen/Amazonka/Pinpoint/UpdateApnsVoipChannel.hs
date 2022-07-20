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
-- Module      : Amazonka.Pinpoint.UpdateApnsVoipChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs VoIP channel for an application or updates the status
-- and settings of the APNs VoIP channel for an application.
module Amazonka.Pinpoint.UpdateApnsVoipChannel
  ( -- * Creating a Request
    UpdateApnsVoipChannel (..),
    newUpdateApnsVoipChannel,

    -- * Request Lenses
    updateApnsVoipChannel_applicationId,
    updateApnsVoipChannel_aPNSVoipChannelRequest,

    -- * Destructuring the Response
    UpdateApnsVoipChannelResponse (..),
    newUpdateApnsVoipChannelResponse,

    -- * Response Lenses
    updateApnsVoipChannelResponse_httpStatus,
    updateApnsVoipChannelResponse_aPNSVoipChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApnsVoipChannel' smart constructor.
data UpdateApnsVoipChannel = UpdateApnsVoipChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    aPNSVoipChannelRequest :: APNSVoipChannelRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApnsVoipChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateApnsVoipChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'aPNSVoipChannelRequest', 'updateApnsVoipChannel_aPNSVoipChannelRequest' - Undocumented member.
newUpdateApnsVoipChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'aPNSVoipChannelRequest'
  APNSVoipChannelRequest ->
  UpdateApnsVoipChannel
newUpdateApnsVoipChannel
  pApplicationId_
  pAPNSVoipChannelRequest_ =
    UpdateApnsVoipChannel'
      { applicationId =
          pApplicationId_,
        aPNSVoipChannelRequest = pAPNSVoipChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateApnsVoipChannel_applicationId :: Lens.Lens' UpdateApnsVoipChannel Prelude.Text
updateApnsVoipChannel_applicationId = Lens.lens (\UpdateApnsVoipChannel' {applicationId} -> applicationId) (\s@UpdateApnsVoipChannel' {} a -> s {applicationId = a} :: UpdateApnsVoipChannel)

-- | Undocumented member.
updateApnsVoipChannel_aPNSVoipChannelRequest :: Lens.Lens' UpdateApnsVoipChannel APNSVoipChannelRequest
updateApnsVoipChannel_aPNSVoipChannelRequest = Lens.lens (\UpdateApnsVoipChannel' {aPNSVoipChannelRequest} -> aPNSVoipChannelRequest) (\s@UpdateApnsVoipChannel' {} a -> s {aPNSVoipChannelRequest = a} :: UpdateApnsVoipChannel)

instance Core.AWSRequest UpdateApnsVoipChannel where
  type
    AWSResponse UpdateApnsVoipChannel =
      UpdateApnsVoipChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApnsVoipChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateApnsVoipChannel where
  hashWithSalt _salt UpdateApnsVoipChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` aPNSVoipChannelRequest

instance Prelude.NFData UpdateApnsVoipChannel where
  rnf UpdateApnsVoipChannel' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf aPNSVoipChannelRequest

instance Core.ToHeaders UpdateApnsVoipChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateApnsVoipChannel where
  toJSON UpdateApnsVoipChannel' {..} =
    Core.toJSON aPNSVoipChannelRequest

instance Core.ToPath UpdateApnsVoipChannel where
  toPath UpdateApnsVoipChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns_voip"
      ]

instance Core.ToQuery UpdateApnsVoipChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApnsVoipChannelResponse' smart constructor.
data UpdateApnsVoipChannelResponse = UpdateApnsVoipChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    aPNSVoipChannelResponse :: APNSVoipChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApnsVoipChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApnsVoipChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSVoipChannelResponse', 'updateApnsVoipChannelResponse_aPNSVoipChannelResponse' - Undocumented member.
newUpdateApnsVoipChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'aPNSVoipChannelResponse'
  APNSVoipChannelResponse ->
  UpdateApnsVoipChannelResponse
newUpdateApnsVoipChannelResponse
  pHttpStatus_
  pAPNSVoipChannelResponse_ =
    UpdateApnsVoipChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSVoipChannelResponse =
          pAPNSVoipChannelResponse_
      }

-- | The response's http status code.
updateApnsVoipChannelResponse_httpStatus :: Lens.Lens' UpdateApnsVoipChannelResponse Prelude.Int
updateApnsVoipChannelResponse_httpStatus = Lens.lens (\UpdateApnsVoipChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateApnsVoipChannelResponse' {} a -> s {httpStatus = a} :: UpdateApnsVoipChannelResponse)

-- | Undocumented member.
updateApnsVoipChannelResponse_aPNSVoipChannelResponse :: Lens.Lens' UpdateApnsVoipChannelResponse APNSVoipChannelResponse
updateApnsVoipChannelResponse_aPNSVoipChannelResponse = Lens.lens (\UpdateApnsVoipChannelResponse' {aPNSVoipChannelResponse} -> aPNSVoipChannelResponse) (\s@UpdateApnsVoipChannelResponse' {} a -> s {aPNSVoipChannelResponse = a} :: UpdateApnsVoipChannelResponse)

instance Prelude.NFData UpdateApnsVoipChannelResponse where
  rnf UpdateApnsVoipChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf aPNSVoipChannelResponse
