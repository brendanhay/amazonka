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
-- Module      : Network.AWS.Pinpoint.DeleteBaiduChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the Baidu channel for an application and deletes any existing
-- settings for the channel.
module Network.AWS.Pinpoint.DeleteBaiduChannel
  ( -- * Creating a Request
    DeleteBaiduChannel (..),
    newDeleteBaiduChannel,

    -- * Request Lenses
    deleteBaiduChannel_applicationId,

    -- * Destructuring the Response
    DeleteBaiduChannelResponse (..),
    newDeleteBaiduChannelResponse,

    -- * Response Lenses
    deleteBaiduChannelResponse_httpStatus,
    deleteBaiduChannelResponse_baiduChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBaiduChannel' smart constructor.
data DeleteBaiduChannel = DeleteBaiduChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBaiduChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteBaiduChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteBaiduChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  DeleteBaiduChannel
newDeleteBaiduChannel pApplicationId_ =
  DeleteBaiduChannel'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteBaiduChannel_applicationId :: Lens.Lens' DeleteBaiduChannel Prelude.Text
deleteBaiduChannel_applicationId = Lens.lens (\DeleteBaiduChannel' {applicationId} -> applicationId) (\s@DeleteBaiduChannel' {} a -> s {applicationId = a} :: DeleteBaiduChannel)

instance Prelude.AWSRequest DeleteBaiduChannel where
  type
    Rs DeleteBaiduChannel =
      DeleteBaiduChannelResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBaiduChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteBaiduChannel

instance Prelude.NFData DeleteBaiduChannel

instance Prelude.ToHeaders DeleteBaiduChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteBaiduChannel where
  toPath DeleteBaiduChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Prelude.toBS applicationId,
        "/channels/baidu"
      ]

instance Prelude.ToQuery DeleteBaiduChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBaiduChannelResponse' smart constructor.
data DeleteBaiduChannelResponse = DeleteBaiduChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    baiduChannelResponse :: BaiduChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBaiduChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteBaiduChannelResponse_httpStatus' - The response's http status code.
--
-- 'baiduChannelResponse', 'deleteBaiduChannelResponse_baiduChannelResponse' - Undocumented member.
newDeleteBaiduChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'baiduChannelResponse'
  BaiduChannelResponse ->
  DeleteBaiduChannelResponse
newDeleteBaiduChannelResponse
  pHttpStatus_
  pBaiduChannelResponse_ =
    DeleteBaiduChannelResponse'
      { httpStatus =
          pHttpStatus_,
        baiduChannelResponse = pBaiduChannelResponse_
      }

-- | The response's http status code.
deleteBaiduChannelResponse_httpStatus :: Lens.Lens' DeleteBaiduChannelResponse Prelude.Int
deleteBaiduChannelResponse_httpStatus = Lens.lens (\DeleteBaiduChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteBaiduChannelResponse' {} a -> s {httpStatus = a} :: DeleteBaiduChannelResponse)

-- | Undocumented member.
deleteBaiduChannelResponse_baiduChannelResponse :: Lens.Lens' DeleteBaiduChannelResponse BaiduChannelResponse
deleteBaiduChannelResponse_baiduChannelResponse = Lens.lens (\DeleteBaiduChannelResponse' {baiduChannelResponse} -> baiduChannelResponse) (\s@DeleteBaiduChannelResponse' {} a -> s {baiduChannelResponse = a} :: DeleteBaiduChannelResponse)

instance Prelude.NFData DeleteBaiduChannelResponse
