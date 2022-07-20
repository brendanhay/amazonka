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
-- Module      : Amazonka.Pinpoint.UpdateApnsChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs channel for an application or updates the status and
-- settings of the APNs channel for an application.
module Amazonka.Pinpoint.UpdateApnsChannel
  ( -- * Creating a Request
    UpdateApnsChannel (..),
    newUpdateApnsChannel,

    -- * Request Lenses
    updateApnsChannel_applicationId,
    updateApnsChannel_aPNSChannelRequest,

    -- * Destructuring the Response
    UpdateApnsChannelResponse (..),
    newUpdateApnsChannelResponse,

    -- * Response Lenses
    updateApnsChannelResponse_httpStatus,
    updateApnsChannelResponse_aPNSChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApnsChannel' smart constructor.
data UpdateApnsChannel = UpdateApnsChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    aPNSChannelRequest :: APNSChannelRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApnsChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateApnsChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'aPNSChannelRequest', 'updateApnsChannel_aPNSChannelRequest' - Undocumented member.
newUpdateApnsChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'aPNSChannelRequest'
  APNSChannelRequest ->
  UpdateApnsChannel
newUpdateApnsChannel
  pApplicationId_
  pAPNSChannelRequest_ =
    UpdateApnsChannel'
      { applicationId = pApplicationId_,
        aPNSChannelRequest = pAPNSChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateApnsChannel_applicationId :: Lens.Lens' UpdateApnsChannel Prelude.Text
updateApnsChannel_applicationId = Lens.lens (\UpdateApnsChannel' {applicationId} -> applicationId) (\s@UpdateApnsChannel' {} a -> s {applicationId = a} :: UpdateApnsChannel)

-- | Undocumented member.
updateApnsChannel_aPNSChannelRequest :: Lens.Lens' UpdateApnsChannel APNSChannelRequest
updateApnsChannel_aPNSChannelRequest = Lens.lens (\UpdateApnsChannel' {aPNSChannelRequest} -> aPNSChannelRequest) (\s@UpdateApnsChannel' {} a -> s {aPNSChannelRequest = a} :: UpdateApnsChannel)

instance Core.AWSRequest UpdateApnsChannel where
  type
    AWSResponse UpdateApnsChannel =
      UpdateApnsChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApnsChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateApnsChannel where
  hashWithSalt _salt UpdateApnsChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` aPNSChannelRequest

instance Prelude.NFData UpdateApnsChannel where
  rnf UpdateApnsChannel' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf aPNSChannelRequest

instance Core.ToHeaders UpdateApnsChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateApnsChannel where
  toJSON UpdateApnsChannel' {..} =
    Core.toJSON aPNSChannelRequest

instance Core.ToPath UpdateApnsChannel where
  toPath UpdateApnsChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns"
      ]

instance Core.ToQuery UpdateApnsChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApnsChannelResponse' smart constructor.
data UpdateApnsChannelResponse = UpdateApnsChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    aPNSChannelResponse :: APNSChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApnsChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApnsChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSChannelResponse', 'updateApnsChannelResponse_aPNSChannelResponse' - Undocumented member.
newUpdateApnsChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'aPNSChannelResponse'
  APNSChannelResponse ->
  UpdateApnsChannelResponse
newUpdateApnsChannelResponse
  pHttpStatus_
  pAPNSChannelResponse_ =
    UpdateApnsChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSChannelResponse = pAPNSChannelResponse_
      }

-- | The response's http status code.
updateApnsChannelResponse_httpStatus :: Lens.Lens' UpdateApnsChannelResponse Prelude.Int
updateApnsChannelResponse_httpStatus = Lens.lens (\UpdateApnsChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateApnsChannelResponse' {} a -> s {httpStatus = a} :: UpdateApnsChannelResponse)

-- | Undocumented member.
updateApnsChannelResponse_aPNSChannelResponse :: Lens.Lens' UpdateApnsChannelResponse APNSChannelResponse
updateApnsChannelResponse_aPNSChannelResponse = Lens.lens (\UpdateApnsChannelResponse' {aPNSChannelResponse} -> aPNSChannelResponse) (\s@UpdateApnsChannelResponse' {} a -> s {aPNSChannelResponse = a} :: UpdateApnsChannelResponse)

instance Prelude.NFData UpdateApnsChannelResponse where
  rnf UpdateApnsChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf aPNSChannelResponse
