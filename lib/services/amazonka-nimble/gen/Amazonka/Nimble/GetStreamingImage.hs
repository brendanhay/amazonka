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
-- Module      : Amazonka.Nimble.GetStreamingImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get streaming image.
module Amazonka.Nimble.GetStreamingImage
  ( -- * Creating a Request
    GetStreamingImage (..),
    newGetStreamingImage,

    -- * Request Lenses
    getStreamingImage_streamingImageId,
    getStreamingImage_studioId,

    -- * Destructuring the Response
    GetStreamingImageResponse (..),
    newGetStreamingImageResponse,

    -- * Response Lenses
    getStreamingImageResponse_streamingImage,
    getStreamingImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStreamingImage' smart constructor.
data GetStreamingImage = GetStreamingImage'
  { -- | The streaming image ID.
    streamingImageId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamingImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingImageId', 'getStreamingImage_streamingImageId' - The streaming image ID.
--
-- 'studioId', 'getStreamingImage_studioId' - The studio ID.
newGetStreamingImage ::
  -- | 'streamingImageId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  GetStreamingImage
newGetStreamingImage pStreamingImageId_ pStudioId_ =
  GetStreamingImage'
    { streamingImageId =
        pStreamingImageId_,
      studioId = pStudioId_
    }

-- | The streaming image ID.
getStreamingImage_streamingImageId :: Lens.Lens' GetStreamingImage Prelude.Text
getStreamingImage_streamingImageId = Lens.lens (\GetStreamingImage' {streamingImageId} -> streamingImageId) (\s@GetStreamingImage' {} a -> s {streamingImageId = a} :: GetStreamingImage)

-- | The studio ID.
getStreamingImage_studioId :: Lens.Lens' GetStreamingImage Prelude.Text
getStreamingImage_studioId = Lens.lens (\GetStreamingImage' {studioId} -> studioId) (\s@GetStreamingImage' {} a -> s {studioId = a} :: GetStreamingImage)

instance Core.AWSRequest GetStreamingImage where
  type
    AWSResponse GetStreamingImage =
      GetStreamingImageResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStreamingImageResponse'
            Prelude.<$> (x Data..?> "streamingImage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStreamingImage where
  hashWithSalt _salt GetStreamingImage' {..} =
    _salt `Prelude.hashWithSalt` streamingImageId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData GetStreamingImage where
  rnf GetStreamingImage' {..} =
    Prelude.rnf streamingImageId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders GetStreamingImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetStreamingImage where
  toPath GetStreamingImage' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/streaming-images/",
        Data.toBS streamingImageId
      ]

instance Data.ToQuery GetStreamingImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStreamingImageResponse' smart constructor.
data GetStreamingImageResponse = GetStreamingImageResponse'
  { -- | The streaming image.
    streamingImage :: Prelude.Maybe StreamingImage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamingImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingImage', 'getStreamingImageResponse_streamingImage' - The streaming image.
--
-- 'httpStatus', 'getStreamingImageResponse_httpStatus' - The response's http status code.
newGetStreamingImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStreamingImageResponse
newGetStreamingImageResponse pHttpStatus_ =
  GetStreamingImageResponse'
    { streamingImage =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The streaming image.
getStreamingImageResponse_streamingImage :: Lens.Lens' GetStreamingImageResponse (Prelude.Maybe StreamingImage)
getStreamingImageResponse_streamingImage = Lens.lens (\GetStreamingImageResponse' {streamingImage} -> streamingImage) (\s@GetStreamingImageResponse' {} a -> s {streamingImage = a} :: GetStreamingImageResponse)

-- | The response's http status code.
getStreamingImageResponse_httpStatus :: Lens.Lens' GetStreamingImageResponse Prelude.Int
getStreamingImageResponse_httpStatus = Lens.lens (\GetStreamingImageResponse' {httpStatus} -> httpStatus) (\s@GetStreamingImageResponse' {} a -> s {httpStatus = a} :: GetStreamingImageResponse)

instance Prelude.NFData GetStreamingImageResponse where
  rnf GetStreamingImageResponse' {..} =
    Prelude.rnf streamingImage
      `Prelude.seq` Prelude.rnf httpStatus
