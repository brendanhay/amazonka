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
-- Module      : Amazonka.ImageBuilder.GetImageRecipe
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an image recipe.
module Amazonka.ImageBuilder.GetImageRecipe
  ( -- * Creating a Request
    GetImageRecipe (..),
    newGetImageRecipe,

    -- * Request Lenses
    getImageRecipe_imageRecipeArn,

    -- * Destructuring the Response
    GetImageRecipeResponse (..),
    newGetImageRecipeResponse,

    -- * Response Lenses
    getImageRecipeResponse_requestId,
    getImageRecipeResponse_imageRecipe,
    getImageRecipeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetImageRecipe' smart constructor.
data GetImageRecipe = GetImageRecipe'
  { -- | The Amazon Resource Name (ARN) of the image recipe that you want to
    -- retrieve.
    imageRecipeArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImageRecipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageRecipeArn', 'getImageRecipe_imageRecipeArn' - The Amazon Resource Name (ARN) of the image recipe that you want to
-- retrieve.
newGetImageRecipe ::
  -- | 'imageRecipeArn'
  Prelude.Text ->
  GetImageRecipe
newGetImageRecipe pImageRecipeArn_ =
  GetImageRecipe' {imageRecipeArn = pImageRecipeArn_}

-- | The Amazon Resource Name (ARN) of the image recipe that you want to
-- retrieve.
getImageRecipe_imageRecipeArn :: Lens.Lens' GetImageRecipe Prelude.Text
getImageRecipe_imageRecipeArn = Lens.lens (\GetImageRecipe' {imageRecipeArn} -> imageRecipeArn) (\s@GetImageRecipe' {} a -> s {imageRecipeArn = a} :: GetImageRecipe)

instance Core.AWSRequest GetImageRecipe where
  type
    AWSResponse GetImageRecipe =
      GetImageRecipeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImageRecipeResponse'
            Prelude.<$> (x Core..?> "requestId")
            Prelude.<*> (x Core..?> "imageRecipe")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetImageRecipe where
  hashWithSalt _salt GetImageRecipe' {..} =
    _salt `Prelude.hashWithSalt` imageRecipeArn

instance Prelude.NFData GetImageRecipe where
  rnf GetImageRecipe' {..} = Prelude.rnf imageRecipeArn

instance Core.ToHeaders GetImageRecipe where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetImageRecipe where
  toPath = Prelude.const "/GetImageRecipe"

instance Core.ToQuery GetImageRecipe where
  toQuery GetImageRecipe' {..} =
    Prelude.mconcat
      ["imageRecipeArn" Core.=: imageRecipeArn]

-- | /See:/ 'newGetImageRecipeResponse' smart constructor.
data GetImageRecipeResponse = GetImageRecipeResponse'
  { -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The image recipe object.
    imageRecipe :: Prelude.Maybe ImageRecipe,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImageRecipeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'getImageRecipeResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'imageRecipe', 'getImageRecipeResponse_imageRecipe' - The image recipe object.
--
-- 'httpStatus', 'getImageRecipeResponse_httpStatus' - The response's http status code.
newGetImageRecipeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetImageRecipeResponse
newGetImageRecipeResponse pHttpStatus_ =
  GetImageRecipeResponse'
    { requestId =
        Prelude.Nothing,
      imageRecipe = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The request ID that uniquely identifies this request.
getImageRecipeResponse_requestId :: Lens.Lens' GetImageRecipeResponse (Prelude.Maybe Prelude.Text)
getImageRecipeResponse_requestId = Lens.lens (\GetImageRecipeResponse' {requestId} -> requestId) (\s@GetImageRecipeResponse' {} a -> s {requestId = a} :: GetImageRecipeResponse)

-- | The image recipe object.
getImageRecipeResponse_imageRecipe :: Lens.Lens' GetImageRecipeResponse (Prelude.Maybe ImageRecipe)
getImageRecipeResponse_imageRecipe = Lens.lens (\GetImageRecipeResponse' {imageRecipe} -> imageRecipe) (\s@GetImageRecipeResponse' {} a -> s {imageRecipe = a} :: GetImageRecipeResponse)

-- | The response's http status code.
getImageRecipeResponse_httpStatus :: Lens.Lens' GetImageRecipeResponse Prelude.Int
getImageRecipeResponse_httpStatus = Lens.lens (\GetImageRecipeResponse' {httpStatus} -> httpStatus) (\s@GetImageRecipeResponse' {} a -> s {httpStatus = a} :: GetImageRecipeResponse)

instance Prelude.NFData GetImageRecipeResponse where
  rnf GetImageRecipeResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf imageRecipe
      `Prelude.seq` Prelude.rnf httpStatus
