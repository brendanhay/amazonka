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
-- Module      : Amazonka.Pinpoint.UpdateSmsChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the SMS channel for an application or updates the status and
-- settings of the SMS channel for an application.
module Amazonka.Pinpoint.UpdateSmsChannel
  ( -- * Creating a Request
    UpdateSmsChannel (..),
    newUpdateSmsChannel,

    -- * Request Lenses
    updateSmsChannel_applicationId,
    updateSmsChannel_sMSChannelRequest,

    -- * Destructuring the Response
    UpdateSmsChannelResponse (..),
    newUpdateSmsChannelResponse,

    -- * Response Lenses
    updateSmsChannelResponse_httpStatus,
    updateSmsChannelResponse_sMSChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSmsChannel' smart constructor.
data UpdateSmsChannel = UpdateSmsChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    sMSChannelRequest :: SMSChannelRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSmsChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateSmsChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'sMSChannelRequest', 'updateSmsChannel_sMSChannelRequest' - Undocumented member.
newUpdateSmsChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'sMSChannelRequest'
  SMSChannelRequest ->
  UpdateSmsChannel
newUpdateSmsChannel
  pApplicationId_
  pSMSChannelRequest_ =
    UpdateSmsChannel'
      { applicationId = pApplicationId_,
        sMSChannelRequest = pSMSChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateSmsChannel_applicationId :: Lens.Lens' UpdateSmsChannel Prelude.Text
updateSmsChannel_applicationId = Lens.lens (\UpdateSmsChannel' {applicationId} -> applicationId) (\s@UpdateSmsChannel' {} a -> s {applicationId = a} :: UpdateSmsChannel)

-- | Undocumented member.
updateSmsChannel_sMSChannelRequest :: Lens.Lens' UpdateSmsChannel SMSChannelRequest
updateSmsChannel_sMSChannelRequest = Lens.lens (\UpdateSmsChannel' {sMSChannelRequest} -> sMSChannelRequest) (\s@UpdateSmsChannel' {} a -> s {sMSChannelRequest = a} :: UpdateSmsChannel)

instance Core.AWSRequest UpdateSmsChannel where
  type
    AWSResponse UpdateSmsChannel =
      UpdateSmsChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSmsChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateSmsChannel where
  hashWithSalt _salt UpdateSmsChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` sMSChannelRequest

instance Prelude.NFData UpdateSmsChannel where
  rnf UpdateSmsChannel' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf sMSChannelRequest

instance Core.ToHeaders UpdateSmsChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSmsChannel where
  toJSON UpdateSmsChannel' {..} =
    Core.toJSON sMSChannelRequest

instance Core.ToPath UpdateSmsChannel where
  toPath UpdateSmsChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/sms"
      ]

instance Core.ToQuery UpdateSmsChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSmsChannelResponse' smart constructor.
data UpdateSmsChannelResponse = UpdateSmsChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    sMSChannelResponse :: SMSChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSmsChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSmsChannelResponse_httpStatus' - The response's http status code.
--
-- 'sMSChannelResponse', 'updateSmsChannelResponse_sMSChannelResponse' - Undocumented member.
newUpdateSmsChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'sMSChannelResponse'
  SMSChannelResponse ->
  UpdateSmsChannelResponse
newUpdateSmsChannelResponse
  pHttpStatus_
  pSMSChannelResponse_ =
    UpdateSmsChannelResponse'
      { httpStatus =
          pHttpStatus_,
        sMSChannelResponse = pSMSChannelResponse_
      }

-- | The response's http status code.
updateSmsChannelResponse_httpStatus :: Lens.Lens' UpdateSmsChannelResponse Prelude.Int
updateSmsChannelResponse_httpStatus = Lens.lens (\UpdateSmsChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateSmsChannelResponse' {} a -> s {httpStatus = a} :: UpdateSmsChannelResponse)

-- | Undocumented member.
updateSmsChannelResponse_sMSChannelResponse :: Lens.Lens' UpdateSmsChannelResponse SMSChannelResponse
updateSmsChannelResponse_sMSChannelResponse = Lens.lens (\UpdateSmsChannelResponse' {sMSChannelResponse} -> sMSChannelResponse) (\s@UpdateSmsChannelResponse' {} a -> s {sMSChannelResponse = a} :: UpdateSmsChannelResponse)

instance Prelude.NFData UpdateSmsChannelResponse where
  rnf UpdateSmsChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf sMSChannelResponse
