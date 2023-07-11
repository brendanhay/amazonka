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
-- Module      : Amazonka.Pinpoint.GetBaiduChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the Baidu channel
-- for an application.
module Amazonka.Pinpoint.GetBaiduChannel
  ( -- * Creating a Request
    GetBaiduChannel (..),
    newGetBaiduChannel,

    -- * Request Lenses
    getBaiduChannel_applicationId,

    -- * Destructuring the Response
    GetBaiduChannelResponse (..),
    newGetBaiduChannelResponse,

    -- * Response Lenses
    getBaiduChannelResponse_httpStatus,
    getBaiduChannelResponse_baiduChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBaiduChannel' smart constructor.
data GetBaiduChannel = GetBaiduChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBaiduChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getBaiduChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetBaiduChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  GetBaiduChannel
newGetBaiduChannel pApplicationId_ =
  GetBaiduChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getBaiduChannel_applicationId :: Lens.Lens' GetBaiduChannel Prelude.Text
getBaiduChannel_applicationId = Lens.lens (\GetBaiduChannel' {applicationId} -> applicationId) (\s@GetBaiduChannel' {} a -> s {applicationId = a} :: GetBaiduChannel)

instance Core.AWSRequest GetBaiduChannel where
  type
    AWSResponse GetBaiduChannel =
      GetBaiduChannelResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBaiduChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetBaiduChannel where
  hashWithSalt _salt GetBaiduChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetBaiduChannel where
  rnf GetBaiduChannel' {..} = Prelude.rnf applicationId

instance Data.ToHeaders GetBaiduChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBaiduChannel where
  toPath GetBaiduChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/channels/baidu"
      ]

instance Data.ToQuery GetBaiduChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBaiduChannelResponse' smart constructor.
data GetBaiduChannelResponse = GetBaiduChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    baiduChannelResponse :: BaiduChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBaiduChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getBaiduChannelResponse_httpStatus' - The response's http status code.
--
-- 'baiduChannelResponse', 'getBaiduChannelResponse_baiduChannelResponse' - Undocumented member.
newGetBaiduChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'baiduChannelResponse'
  BaiduChannelResponse ->
  GetBaiduChannelResponse
newGetBaiduChannelResponse
  pHttpStatus_
  pBaiduChannelResponse_ =
    GetBaiduChannelResponse'
      { httpStatus = pHttpStatus_,
        baiduChannelResponse = pBaiduChannelResponse_
      }

-- | The response's http status code.
getBaiduChannelResponse_httpStatus :: Lens.Lens' GetBaiduChannelResponse Prelude.Int
getBaiduChannelResponse_httpStatus = Lens.lens (\GetBaiduChannelResponse' {httpStatus} -> httpStatus) (\s@GetBaiduChannelResponse' {} a -> s {httpStatus = a} :: GetBaiduChannelResponse)

-- | Undocumented member.
getBaiduChannelResponse_baiduChannelResponse :: Lens.Lens' GetBaiduChannelResponse BaiduChannelResponse
getBaiduChannelResponse_baiduChannelResponse = Lens.lens (\GetBaiduChannelResponse' {baiduChannelResponse} -> baiduChannelResponse) (\s@GetBaiduChannelResponse' {} a -> s {baiduChannelResponse = a} :: GetBaiduChannelResponse)

instance Prelude.NFData GetBaiduChannelResponse where
  rnf GetBaiduChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf baiduChannelResponse
