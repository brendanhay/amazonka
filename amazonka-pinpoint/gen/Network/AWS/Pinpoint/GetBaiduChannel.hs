{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.GetBaiduChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the Baidu channel
-- for an application.
module Network.AWS.Pinpoint.GetBaiduChannel
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBaiduChannel' smart constructor.
data GetBaiduChannel = GetBaiduChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetBaiduChannel where
  type Rs GetBaiduChannel = GetBaiduChannelResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBaiduChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable GetBaiduChannel

instance Prelude.NFData GetBaiduChannel

instance Prelude.ToHeaders GetBaiduChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetBaiduChannel where
  toPath GetBaiduChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Prelude.toBS applicationId,
        "/channels/baidu"
      ]

instance Prelude.ToQuery GetBaiduChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBaiduChannelResponse' smart constructor.
data GetBaiduChannelResponse = GetBaiduChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    baiduChannelResponse :: BaiduChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData GetBaiduChannelResponse
