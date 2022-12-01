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
-- Module      : Amazonka.ImageBuilder.GetImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an image.
module Amazonka.ImageBuilder.GetImage
  ( -- * Creating a Request
    GetImage (..),
    newGetImage,

    -- * Request Lenses
    getImage_imageBuildVersionArn,

    -- * Destructuring the Response
    GetImageResponse (..),
    newGetImageResponse,

    -- * Response Lenses
    getImageResponse_requestId,
    getImageResponse_image,
    getImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetImage' smart constructor.
data GetImage = GetImage'
  { -- | The Amazon Resource Name (ARN) of the image that you want to retrieve.
    imageBuildVersionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageBuildVersionArn', 'getImage_imageBuildVersionArn' - The Amazon Resource Name (ARN) of the image that you want to retrieve.
newGetImage ::
  -- | 'imageBuildVersionArn'
  Prelude.Text ->
  GetImage
newGetImage pImageBuildVersionArn_ =
  GetImage'
    { imageBuildVersionArn =
        pImageBuildVersionArn_
    }

-- | The Amazon Resource Name (ARN) of the image that you want to retrieve.
getImage_imageBuildVersionArn :: Lens.Lens' GetImage Prelude.Text
getImage_imageBuildVersionArn = Lens.lens (\GetImage' {imageBuildVersionArn} -> imageBuildVersionArn) (\s@GetImage' {} a -> s {imageBuildVersionArn = a} :: GetImage)

instance Core.AWSRequest GetImage where
  type AWSResponse GetImage = GetImageResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImageResponse'
            Prelude.<$> (x Core..?> "requestId")
            Prelude.<*> (x Core..?> "image")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetImage where
  hashWithSalt _salt GetImage' {..} =
    _salt `Prelude.hashWithSalt` imageBuildVersionArn

instance Prelude.NFData GetImage where
  rnf GetImage' {..} = Prelude.rnf imageBuildVersionArn

instance Core.ToHeaders GetImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetImage where
  toPath = Prelude.const "/GetImage"

instance Core.ToQuery GetImage where
  toQuery GetImage' {..} =
    Prelude.mconcat
      ["imageBuildVersionArn" Core.=: imageBuildVersionArn]

-- | /See:/ 'newGetImageResponse' smart constructor.
data GetImageResponse = GetImageResponse'
  { -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The image object.
    image :: Prelude.Maybe Image,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'getImageResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'image', 'getImageResponse_image' - The image object.
--
-- 'httpStatus', 'getImageResponse_httpStatus' - The response's http status code.
newGetImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetImageResponse
newGetImageResponse pHttpStatus_ =
  GetImageResponse'
    { requestId = Prelude.Nothing,
      image = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The request ID that uniquely identifies this request.
getImageResponse_requestId :: Lens.Lens' GetImageResponse (Prelude.Maybe Prelude.Text)
getImageResponse_requestId = Lens.lens (\GetImageResponse' {requestId} -> requestId) (\s@GetImageResponse' {} a -> s {requestId = a} :: GetImageResponse)

-- | The image object.
getImageResponse_image :: Lens.Lens' GetImageResponse (Prelude.Maybe Image)
getImageResponse_image = Lens.lens (\GetImageResponse' {image} -> image) (\s@GetImageResponse' {} a -> s {image = a} :: GetImageResponse)

-- | The response's http status code.
getImageResponse_httpStatus :: Lens.Lens' GetImageResponse Prelude.Int
getImageResponse_httpStatus = Lens.lens (\GetImageResponse' {httpStatus} -> httpStatus) (\s@GetImageResponse' {} a -> s {httpStatus = a} :: GetImageResponse)

instance Prelude.NFData GetImageResponse where
  rnf GetImageResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf image
      `Prelude.seq` Prelude.rnf httpStatus
