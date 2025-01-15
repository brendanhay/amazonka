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
-- Module      : Amazonka.Pinpoint.DeleteBaiduChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the Baidu channel for an application and deletes any existing
-- settings for the channel.
module Amazonka.Pinpoint.DeleteBaiduChannel
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBaiduChannel' smart constructor.
data DeleteBaiduChannel = DeleteBaiduChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteBaiduChannel where
  type
    AWSResponse DeleteBaiduChannel =
      DeleteBaiduChannelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBaiduChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteBaiduChannel where
  hashWithSalt _salt DeleteBaiduChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DeleteBaiduChannel where
  rnf DeleteBaiduChannel' {..} =
    Prelude.rnf applicationId

instance Data.ToHeaders DeleteBaiduChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteBaiduChannel where
  toPath DeleteBaiduChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/channels/baidu"
      ]

instance Data.ToQuery DeleteBaiduChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBaiduChannelResponse' smart constructor.
data DeleteBaiduChannelResponse = DeleteBaiduChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    baiduChannelResponse :: BaiduChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteBaiduChannelResponse where
  rnf DeleteBaiduChannelResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf baiduChannelResponse
