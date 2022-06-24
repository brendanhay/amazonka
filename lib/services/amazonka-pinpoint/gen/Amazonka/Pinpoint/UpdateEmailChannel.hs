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
-- Module      : Amazonka.Pinpoint.UpdateEmailChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the email channel for an application or updates the status and
-- settings of the email channel for an application.
module Amazonka.Pinpoint.UpdateEmailChannel
  ( -- * Creating a Request
    UpdateEmailChannel (..),
    newUpdateEmailChannel,

    -- * Request Lenses
    updateEmailChannel_applicationId,
    updateEmailChannel_emailChannelRequest,

    -- * Destructuring the Response
    UpdateEmailChannelResponse (..),
    newUpdateEmailChannelResponse,

    -- * Response Lenses
    updateEmailChannelResponse_httpStatus,
    updateEmailChannelResponse_emailChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEmailChannel' smart constructor.
data UpdateEmailChannel = UpdateEmailChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    emailChannelRequest :: EmailChannelRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEmailChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateEmailChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'emailChannelRequest', 'updateEmailChannel_emailChannelRequest' - Undocumented member.
newUpdateEmailChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'emailChannelRequest'
  EmailChannelRequest ->
  UpdateEmailChannel
newUpdateEmailChannel
  pApplicationId_
  pEmailChannelRequest_ =
    UpdateEmailChannel'
      { applicationId =
          pApplicationId_,
        emailChannelRequest = pEmailChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateEmailChannel_applicationId :: Lens.Lens' UpdateEmailChannel Prelude.Text
updateEmailChannel_applicationId = Lens.lens (\UpdateEmailChannel' {applicationId} -> applicationId) (\s@UpdateEmailChannel' {} a -> s {applicationId = a} :: UpdateEmailChannel)

-- | Undocumented member.
updateEmailChannel_emailChannelRequest :: Lens.Lens' UpdateEmailChannel EmailChannelRequest
updateEmailChannel_emailChannelRequest = Lens.lens (\UpdateEmailChannel' {emailChannelRequest} -> emailChannelRequest) (\s@UpdateEmailChannel' {} a -> s {emailChannelRequest = a} :: UpdateEmailChannel)

instance Core.AWSRequest UpdateEmailChannel where
  type
    AWSResponse UpdateEmailChannel =
      UpdateEmailChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEmailChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateEmailChannel where
  hashWithSalt _salt UpdateEmailChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` emailChannelRequest

instance Prelude.NFData UpdateEmailChannel where
  rnf UpdateEmailChannel' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf emailChannelRequest

instance Core.ToHeaders UpdateEmailChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateEmailChannel where
  toJSON UpdateEmailChannel' {..} =
    Core.toJSON emailChannelRequest

instance Core.ToPath UpdateEmailChannel where
  toPath UpdateEmailChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/email"
      ]

instance Core.ToQuery UpdateEmailChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEmailChannelResponse' smart constructor.
data UpdateEmailChannelResponse = UpdateEmailChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    emailChannelResponse :: EmailChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEmailChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEmailChannelResponse_httpStatus' - The response's http status code.
--
-- 'emailChannelResponse', 'updateEmailChannelResponse_emailChannelResponse' - Undocumented member.
newUpdateEmailChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'emailChannelResponse'
  EmailChannelResponse ->
  UpdateEmailChannelResponse
newUpdateEmailChannelResponse
  pHttpStatus_
  pEmailChannelResponse_ =
    UpdateEmailChannelResponse'
      { httpStatus =
          pHttpStatus_,
        emailChannelResponse = pEmailChannelResponse_
      }

-- | The response's http status code.
updateEmailChannelResponse_httpStatus :: Lens.Lens' UpdateEmailChannelResponse Prelude.Int
updateEmailChannelResponse_httpStatus = Lens.lens (\UpdateEmailChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateEmailChannelResponse' {} a -> s {httpStatus = a} :: UpdateEmailChannelResponse)

-- | Undocumented member.
updateEmailChannelResponse_emailChannelResponse :: Lens.Lens' UpdateEmailChannelResponse EmailChannelResponse
updateEmailChannelResponse_emailChannelResponse = Lens.lens (\UpdateEmailChannelResponse' {emailChannelResponse} -> emailChannelResponse) (\s@UpdateEmailChannelResponse' {} a -> s {emailChannelResponse = a} :: UpdateEmailChannelResponse)

instance Prelude.NFData UpdateEmailChannelResponse where
  rnf UpdateEmailChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf emailChannelResponse
