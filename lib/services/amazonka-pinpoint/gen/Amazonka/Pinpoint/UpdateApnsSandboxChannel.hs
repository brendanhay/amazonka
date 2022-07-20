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
-- Module      : Amazonka.Pinpoint.UpdateApnsSandboxChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs sandbox channel for an application or updates the
-- status and settings of the APNs sandbox channel for an application.
module Amazonka.Pinpoint.UpdateApnsSandboxChannel
  ( -- * Creating a Request
    UpdateApnsSandboxChannel (..),
    newUpdateApnsSandboxChannel,

    -- * Request Lenses
    updateApnsSandboxChannel_applicationId,
    updateApnsSandboxChannel_aPNSSandboxChannelRequest,

    -- * Destructuring the Response
    UpdateApnsSandboxChannelResponse (..),
    newUpdateApnsSandboxChannelResponse,

    -- * Response Lenses
    updateApnsSandboxChannelResponse_httpStatus,
    updateApnsSandboxChannelResponse_aPNSSandboxChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApnsSandboxChannel' smart constructor.
data UpdateApnsSandboxChannel = UpdateApnsSandboxChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    aPNSSandboxChannelRequest :: APNSSandboxChannelRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApnsSandboxChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateApnsSandboxChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'aPNSSandboxChannelRequest', 'updateApnsSandboxChannel_aPNSSandboxChannelRequest' - Undocumented member.
newUpdateApnsSandboxChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'aPNSSandboxChannelRequest'
  APNSSandboxChannelRequest ->
  UpdateApnsSandboxChannel
newUpdateApnsSandboxChannel
  pApplicationId_
  pAPNSSandboxChannelRequest_ =
    UpdateApnsSandboxChannel'
      { applicationId =
          pApplicationId_,
        aPNSSandboxChannelRequest =
          pAPNSSandboxChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateApnsSandboxChannel_applicationId :: Lens.Lens' UpdateApnsSandboxChannel Prelude.Text
updateApnsSandboxChannel_applicationId = Lens.lens (\UpdateApnsSandboxChannel' {applicationId} -> applicationId) (\s@UpdateApnsSandboxChannel' {} a -> s {applicationId = a} :: UpdateApnsSandboxChannel)

-- | Undocumented member.
updateApnsSandboxChannel_aPNSSandboxChannelRequest :: Lens.Lens' UpdateApnsSandboxChannel APNSSandboxChannelRequest
updateApnsSandboxChannel_aPNSSandboxChannelRequest = Lens.lens (\UpdateApnsSandboxChannel' {aPNSSandboxChannelRequest} -> aPNSSandboxChannelRequest) (\s@UpdateApnsSandboxChannel' {} a -> s {aPNSSandboxChannelRequest = a} :: UpdateApnsSandboxChannel)

instance Core.AWSRequest UpdateApnsSandboxChannel where
  type
    AWSResponse UpdateApnsSandboxChannel =
      UpdateApnsSandboxChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApnsSandboxChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateApnsSandboxChannel where
  hashWithSalt _salt UpdateApnsSandboxChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` aPNSSandboxChannelRequest

instance Prelude.NFData UpdateApnsSandboxChannel where
  rnf UpdateApnsSandboxChannel' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf aPNSSandboxChannelRequest

instance Core.ToHeaders UpdateApnsSandboxChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateApnsSandboxChannel where
  toJSON UpdateApnsSandboxChannel' {..} =
    Core.toJSON aPNSSandboxChannelRequest

instance Core.ToPath UpdateApnsSandboxChannel where
  toPath UpdateApnsSandboxChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns_sandbox"
      ]

instance Core.ToQuery UpdateApnsSandboxChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApnsSandboxChannelResponse' smart constructor.
data UpdateApnsSandboxChannelResponse = UpdateApnsSandboxChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    aPNSSandboxChannelResponse :: APNSSandboxChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApnsSandboxChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApnsSandboxChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSSandboxChannelResponse', 'updateApnsSandboxChannelResponse_aPNSSandboxChannelResponse' - Undocumented member.
newUpdateApnsSandboxChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'aPNSSandboxChannelResponse'
  APNSSandboxChannelResponse ->
  UpdateApnsSandboxChannelResponse
newUpdateApnsSandboxChannelResponse
  pHttpStatus_
  pAPNSSandboxChannelResponse_ =
    UpdateApnsSandboxChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSSandboxChannelResponse =
          pAPNSSandboxChannelResponse_
      }

-- | The response's http status code.
updateApnsSandboxChannelResponse_httpStatus :: Lens.Lens' UpdateApnsSandboxChannelResponse Prelude.Int
updateApnsSandboxChannelResponse_httpStatus = Lens.lens (\UpdateApnsSandboxChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateApnsSandboxChannelResponse' {} a -> s {httpStatus = a} :: UpdateApnsSandboxChannelResponse)

-- | Undocumented member.
updateApnsSandboxChannelResponse_aPNSSandboxChannelResponse :: Lens.Lens' UpdateApnsSandboxChannelResponse APNSSandboxChannelResponse
updateApnsSandboxChannelResponse_aPNSSandboxChannelResponse = Lens.lens (\UpdateApnsSandboxChannelResponse' {aPNSSandboxChannelResponse} -> aPNSSandboxChannelResponse) (\s@UpdateApnsSandboxChannelResponse' {} a -> s {aPNSSandboxChannelResponse = a} :: UpdateApnsSandboxChannelResponse)

instance
  Prelude.NFData
    UpdateApnsSandboxChannelResponse
  where
  rnf UpdateApnsSandboxChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf aPNSSandboxChannelResponse
