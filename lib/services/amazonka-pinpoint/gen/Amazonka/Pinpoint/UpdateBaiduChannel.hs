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
-- Module      : Amazonka.Pinpoint.UpdateBaiduChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the Baidu channel for an application or updates the status and
-- settings of the Baidu channel for an application.
module Amazonka.Pinpoint.UpdateBaiduChannel
  ( -- * Creating a Request
    UpdateBaiduChannel (..),
    newUpdateBaiduChannel,

    -- * Request Lenses
    updateBaiduChannel_applicationId,
    updateBaiduChannel_baiduChannelRequest,

    -- * Destructuring the Response
    UpdateBaiduChannelResponse (..),
    newUpdateBaiduChannelResponse,

    -- * Response Lenses
    updateBaiduChannelResponse_httpStatus,
    updateBaiduChannelResponse_baiduChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateBaiduChannel' smart constructor.
data UpdateBaiduChannel = UpdateBaiduChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    baiduChannelRequest :: BaiduChannelRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBaiduChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateBaiduChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'baiduChannelRequest', 'updateBaiduChannel_baiduChannelRequest' - Undocumented member.
newUpdateBaiduChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'baiduChannelRequest'
  BaiduChannelRequest ->
  UpdateBaiduChannel
newUpdateBaiduChannel
  pApplicationId_
  pBaiduChannelRequest_ =
    UpdateBaiduChannel'
      { applicationId =
          pApplicationId_,
        baiduChannelRequest = pBaiduChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateBaiduChannel_applicationId :: Lens.Lens' UpdateBaiduChannel Prelude.Text
updateBaiduChannel_applicationId = Lens.lens (\UpdateBaiduChannel' {applicationId} -> applicationId) (\s@UpdateBaiduChannel' {} a -> s {applicationId = a} :: UpdateBaiduChannel)

-- | Undocumented member.
updateBaiduChannel_baiduChannelRequest :: Lens.Lens' UpdateBaiduChannel BaiduChannelRequest
updateBaiduChannel_baiduChannelRequest = Lens.lens (\UpdateBaiduChannel' {baiduChannelRequest} -> baiduChannelRequest) (\s@UpdateBaiduChannel' {} a -> s {baiduChannelRequest = a} :: UpdateBaiduChannel)

instance Core.AWSRequest UpdateBaiduChannel where
  type
    AWSResponse UpdateBaiduChannel =
      UpdateBaiduChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBaiduChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateBaiduChannel where
  hashWithSalt _salt UpdateBaiduChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` baiduChannelRequest

instance Prelude.NFData UpdateBaiduChannel where
  rnf UpdateBaiduChannel' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf baiduChannelRequest

instance Core.ToHeaders UpdateBaiduChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateBaiduChannel where
  toJSON UpdateBaiduChannel' {..} =
    Core.toJSON baiduChannelRequest

instance Core.ToPath UpdateBaiduChannel where
  toPath UpdateBaiduChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/baidu"
      ]

instance Core.ToQuery UpdateBaiduChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBaiduChannelResponse' smart constructor.
data UpdateBaiduChannelResponse = UpdateBaiduChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    baiduChannelResponse :: BaiduChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBaiduChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateBaiduChannelResponse_httpStatus' - The response's http status code.
--
-- 'baiduChannelResponse', 'updateBaiduChannelResponse_baiduChannelResponse' - Undocumented member.
newUpdateBaiduChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'baiduChannelResponse'
  BaiduChannelResponse ->
  UpdateBaiduChannelResponse
newUpdateBaiduChannelResponse
  pHttpStatus_
  pBaiduChannelResponse_ =
    UpdateBaiduChannelResponse'
      { httpStatus =
          pHttpStatus_,
        baiduChannelResponse = pBaiduChannelResponse_
      }

-- | The response's http status code.
updateBaiduChannelResponse_httpStatus :: Lens.Lens' UpdateBaiduChannelResponse Prelude.Int
updateBaiduChannelResponse_httpStatus = Lens.lens (\UpdateBaiduChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateBaiduChannelResponse' {} a -> s {httpStatus = a} :: UpdateBaiduChannelResponse)

-- | Undocumented member.
updateBaiduChannelResponse_baiduChannelResponse :: Lens.Lens' UpdateBaiduChannelResponse BaiduChannelResponse
updateBaiduChannelResponse_baiduChannelResponse = Lens.lens (\UpdateBaiduChannelResponse' {baiduChannelResponse} -> baiduChannelResponse) (\s@UpdateBaiduChannelResponse' {} a -> s {baiduChannelResponse = a} :: UpdateBaiduChannelResponse)

instance Prelude.NFData UpdateBaiduChannelResponse where
  rnf UpdateBaiduChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf baiduChannelResponse
