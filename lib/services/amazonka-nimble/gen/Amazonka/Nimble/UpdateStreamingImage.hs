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
-- Module      : Amazonka.Nimble.UpdateStreamingImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update streaming image.
module Amazonka.Nimble.UpdateStreamingImage
  ( -- * Creating a Request
    UpdateStreamingImage (..),
    newUpdateStreamingImage,

    -- * Request Lenses
    updateStreamingImage_clientToken,
    updateStreamingImage_description,
    updateStreamingImage_name,
    updateStreamingImage_streamingImageId,
    updateStreamingImage_studioId,

    -- * Destructuring the Response
    UpdateStreamingImageResponse (..),
    newUpdateStreamingImageResponse,

    -- * Response Lenses
    updateStreamingImageResponse_streamingImage,
    updateStreamingImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateStreamingImage' smart constructor.
data UpdateStreamingImage = UpdateStreamingImage'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the
    -- Amazon Web Services SDK automatically generates a client token and uses
    -- it for the request to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name for the streaming image.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The streaming image ID.
    streamingImageId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStreamingImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateStreamingImage_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
--
-- 'description', 'updateStreamingImage_description' - The description.
--
-- 'name', 'updateStreamingImage_name' - The name for the streaming image.
--
-- 'streamingImageId', 'updateStreamingImage_streamingImageId' - The streaming image ID.
--
-- 'studioId', 'updateStreamingImage_studioId' - The studio ID.
newUpdateStreamingImage ::
  -- | 'streamingImageId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  UpdateStreamingImage
newUpdateStreamingImage pStreamingImageId_ pStudioId_ =
  UpdateStreamingImage'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      streamingImageId = pStreamingImageId_,
      studioId = pStudioId_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
updateStreamingImage_clientToken :: Lens.Lens' UpdateStreamingImage (Prelude.Maybe Prelude.Text)
updateStreamingImage_clientToken = Lens.lens (\UpdateStreamingImage' {clientToken} -> clientToken) (\s@UpdateStreamingImage' {} a -> s {clientToken = a} :: UpdateStreamingImage)

-- | The description.
updateStreamingImage_description :: Lens.Lens' UpdateStreamingImage (Prelude.Maybe Prelude.Text)
updateStreamingImage_description = Lens.lens (\UpdateStreamingImage' {description} -> description) (\s@UpdateStreamingImage' {} a -> s {description = a} :: UpdateStreamingImage) Prelude.. Lens.mapping Data._Sensitive

-- | The name for the streaming image.
updateStreamingImage_name :: Lens.Lens' UpdateStreamingImage (Prelude.Maybe Prelude.Text)
updateStreamingImage_name = Lens.lens (\UpdateStreamingImage' {name} -> name) (\s@UpdateStreamingImage' {} a -> s {name = a} :: UpdateStreamingImage) Prelude.. Lens.mapping Data._Sensitive

-- | The streaming image ID.
updateStreamingImage_streamingImageId :: Lens.Lens' UpdateStreamingImage Prelude.Text
updateStreamingImage_streamingImageId = Lens.lens (\UpdateStreamingImage' {streamingImageId} -> streamingImageId) (\s@UpdateStreamingImage' {} a -> s {streamingImageId = a} :: UpdateStreamingImage)

-- | The studio ID.
updateStreamingImage_studioId :: Lens.Lens' UpdateStreamingImage Prelude.Text
updateStreamingImage_studioId = Lens.lens (\UpdateStreamingImage' {studioId} -> studioId) (\s@UpdateStreamingImage' {} a -> s {studioId = a} :: UpdateStreamingImage)

instance Core.AWSRequest UpdateStreamingImage where
  type
    AWSResponse UpdateStreamingImage =
      UpdateStreamingImageResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStreamingImageResponse'
            Prelude.<$> (x Data..?> "streamingImage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStreamingImage where
  hashWithSalt _salt UpdateStreamingImage' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` streamingImageId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData UpdateStreamingImage where
  rnf UpdateStreamingImage' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf streamingImageId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders UpdateStreamingImage where
  toHeaders UpdateStreamingImage' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON UpdateStreamingImage where
  toJSON UpdateStreamingImage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateStreamingImage where
  toPath UpdateStreamingImage' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/streaming-images/",
        Data.toBS streamingImageId
      ]

instance Data.ToQuery UpdateStreamingImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStreamingImageResponse' smart constructor.
data UpdateStreamingImageResponse = UpdateStreamingImageResponse'
  { streamingImage :: Prelude.Maybe StreamingImage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStreamingImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingImage', 'updateStreamingImageResponse_streamingImage' - Undocumented member.
--
-- 'httpStatus', 'updateStreamingImageResponse_httpStatus' - The response's http status code.
newUpdateStreamingImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStreamingImageResponse
newUpdateStreamingImageResponse pHttpStatus_ =
  UpdateStreamingImageResponse'
    { streamingImage =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateStreamingImageResponse_streamingImage :: Lens.Lens' UpdateStreamingImageResponse (Prelude.Maybe StreamingImage)
updateStreamingImageResponse_streamingImage = Lens.lens (\UpdateStreamingImageResponse' {streamingImage} -> streamingImage) (\s@UpdateStreamingImageResponse' {} a -> s {streamingImage = a} :: UpdateStreamingImageResponse)

-- | The response's http status code.
updateStreamingImageResponse_httpStatus :: Lens.Lens' UpdateStreamingImageResponse Prelude.Int
updateStreamingImageResponse_httpStatus = Lens.lens (\UpdateStreamingImageResponse' {httpStatus} -> httpStatus) (\s@UpdateStreamingImageResponse' {} a -> s {httpStatus = a} :: UpdateStreamingImageResponse)

instance Prelude.NFData UpdateStreamingImageResponse where
  rnf UpdateStreamingImageResponse' {..} =
    Prelude.rnf streamingImage
      `Prelude.seq` Prelude.rnf httpStatus
