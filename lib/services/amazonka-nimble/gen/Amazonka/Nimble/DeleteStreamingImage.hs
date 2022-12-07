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
-- Module      : Amazonka.Nimble.DeleteStreamingImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete streaming image.
module Amazonka.Nimble.DeleteStreamingImage
  ( -- * Creating a Request
    DeleteStreamingImage (..),
    newDeleteStreamingImage,

    -- * Request Lenses
    deleteStreamingImage_clientToken,
    deleteStreamingImage_streamingImageId,
    deleteStreamingImage_studioId,

    -- * Destructuring the Response
    DeleteStreamingImageResponse (..),
    newDeleteStreamingImageResponse,

    -- * Response Lenses
    deleteStreamingImageResponse_streamingImage,
    deleteStreamingImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStreamingImage' smart constructor.
data DeleteStreamingImage = DeleteStreamingImage'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the AWS
    -- SDK automatically generates a client token and uses it for the request
    -- to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The streaming image ID.
    streamingImageId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStreamingImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteStreamingImage_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
--
-- 'streamingImageId', 'deleteStreamingImage_streamingImageId' - The streaming image ID.
--
-- 'studioId', 'deleteStreamingImage_studioId' - The studio ID.
newDeleteStreamingImage ::
  -- | 'streamingImageId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  DeleteStreamingImage
newDeleteStreamingImage pStreamingImageId_ pStudioId_ =
  DeleteStreamingImage'
    { clientToken =
        Prelude.Nothing,
      streamingImageId = pStreamingImageId_,
      studioId = pStudioId_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
deleteStreamingImage_clientToken :: Lens.Lens' DeleteStreamingImage (Prelude.Maybe Prelude.Text)
deleteStreamingImage_clientToken = Lens.lens (\DeleteStreamingImage' {clientToken} -> clientToken) (\s@DeleteStreamingImage' {} a -> s {clientToken = a} :: DeleteStreamingImage)

-- | The streaming image ID.
deleteStreamingImage_streamingImageId :: Lens.Lens' DeleteStreamingImage Prelude.Text
deleteStreamingImage_streamingImageId = Lens.lens (\DeleteStreamingImage' {streamingImageId} -> streamingImageId) (\s@DeleteStreamingImage' {} a -> s {streamingImageId = a} :: DeleteStreamingImage)

-- | The studio ID.
deleteStreamingImage_studioId :: Lens.Lens' DeleteStreamingImage Prelude.Text
deleteStreamingImage_studioId = Lens.lens (\DeleteStreamingImage' {studioId} -> studioId) (\s@DeleteStreamingImage' {} a -> s {studioId = a} :: DeleteStreamingImage)

instance Core.AWSRequest DeleteStreamingImage where
  type
    AWSResponse DeleteStreamingImage =
      DeleteStreamingImageResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteStreamingImageResponse'
            Prelude.<$> (x Data..?> "streamingImage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStreamingImage where
  hashWithSalt _salt DeleteStreamingImage' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` streamingImageId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData DeleteStreamingImage where
  rnf DeleteStreamingImage' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf streamingImageId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders DeleteStreamingImage where
  toHeaders DeleteStreamingImage' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DeleteStreamingImage where
  toPath DeleteStreamingImage' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/streaming-images/",
        Data.toBS streamingImageId
      ]

instance Data.ToQuery DeleteStreamingImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStreamingImageResponse' smart constructor.
data DeleteStreamingImageResponse = DeleteStreamingImageResponse'
  { -- | The streaming image.
    streamingImage :: Prelude.Maybe StreamingImage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStreamingImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingImage', 'deleteStreamingImageResponse_streamingImage' - The streaming image.
--
-- 'httpStatus', 'deleteStreamingImageResponse_httpStatus' - The response's http status code.
newDeleteStreamingImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteStreamingImageResponse
newDeleteStreamingImageResponse pHttpStatus_ =
  DeleteStreamingImageResponse'
    { streamingImage =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The streaming image.
deleteStreamingImageResponse_streamingImage :: Lens.Lens' DeleteStreamingImageResponse (Prelude.Maybe StreamingImage)
deleteStreamingImageResponse_streamingImage = Lens.lens (\DeleteStreamingImageResponse' {streamingImage} -> streamingImage) (\s@DeleteStreamingImageResponse' {} a -> s {streamingImage = a} :: DeleteStreamingImageResponse)

-- | The response's http status code.
deleteStreamingImageResponse_httpStatus :: Lens.Lens' DeleteStreamingImageResponse Prelude.Int
deleteStreamingImageResponse_httpStatus = Lens.lens (\DeleteStreamingImageResponse' {httpStatus} -> httpStatus) (\s@DeleteStreamingImageResponse' {} a -> s {httpStatus = a} :: DeleteStreamingImageResponse)

instance Prelude.NFData DeleteStreamingImageResponse where
  rnf DeleteStreamingImageResponse' {..} =
    Prelude.rnf streamingImage
      `Prelude.seq` Prelude.rnf httpStatus
