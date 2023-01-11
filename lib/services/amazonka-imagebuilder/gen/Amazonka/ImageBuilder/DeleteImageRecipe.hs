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
-- Module      : Amazonka.ImageBuilder.DeleteImageRecipe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an image recipe.
module Amazonka.ImageBuilder.DeleteImageRecipe
  ( -- * Creating a Request
    DeleteImageRecipe (..),
    newDeleteImageRecipe,

    -- * Request Lenses
    deleteImageRecipe_imageRecipeArn,

    -- * Destructuring the Response
    DeleteImageRecipeResponse (..),
    newDeleteImageRecipeResponse,

    -- * Response Lenses
    deleteImageRecipeResponse_imageRecipeArn,
    deleteImageRecipeResponse_requestId,
    deleteImageRecipeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteImageRecipe' smart constructor.
data DeleteImageRecipe = DeleteImageRecipe'
  { -- | The Amazon Resource Name (ARN) of the image recipe to delete.
    imageRecipeArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImageRecipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageRecipeArn', 'deleteImageRecipe_imageRecipeArn' - The Amazon Resource Name (ARN) of the image recipe to delete.
newDeleteImageRecipe ::
  -- | 'imageRecipeArn'
  Prelude.Text ->
  DeleteImageRecipe
newDeleteImageRecipe pImageRecipeArn_ =
  DeleteImageRecipe'
    { imageRecipeArn =
        pImageRecipeArn_
    }

-- | The Amazon Resource Name (ARN) of the image recipe to delete.
deleteImageRecipe_imageRecipeArn :: Lens.Lens' DeleteImageRecipe Prelude.Text
deleteImageRecipe_imageRecipeArn = Lens.lens (\DeleteImageRecipe' {imageRecipeArn} -> imageRecipeArn) (\s@DeleteImageRecipe' {} a -> s {imageRecipeArn = a} :: DeleteImageRecipe)

instance Core.AWSRequest DeleteImageRecipe where
  type
    AWSResponse DeleteImageRecipe =
      DeleteImageRecipeResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteImageRecipeResponse'
            Prelude.<$> (x Data..?> "imageRecipeArn")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteImageRecipe where
  hashWithSalt _salt DeleteImageRecipe' {..} =
    _salt `Prelude.hashWithSalt` imageRecipeArn

instance Prelude.NFData DeleteImageRecipe where
  rnf DeleteImageRecipe' {..} =
    Prelude.rnf imageRecipeArn

instance Data.ToHeaders DeleteImageRecipe where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteImageRecipe where
  toPath = Prelude.const "/DeleteImageRecipe"

instance Data.ToQuery DeleteImageRecipe where
  toQuery DeleteImageRecipe' {..} =
    Prelude.mconcat
      ["imageRecipeArn" Data.=: imageRecipeArn]

-- | /See:/ 'newDeleteImageRecipeResponse' smart constructor.
data DeleteImageRecipeResponse = DeleteImageRecipeResponse'
  { -- | The Amazon Resource Name (ARN) of the image recipe that was deleted.
    imageRecipeArn :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImageRecipeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageRecipeArn', 'deleteImageRecipeResponse_imageRecipeArn' - The Amazon Resource Name (ARN) of the image recipe that was deleted.
--
-- 'requestId', 'deleteImageRecipeResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'deleteImageRecipeResponse_httpStatus' - The response's http status code.
newDeleteImageRecipeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteImageRecipeResponse
newDeleteImageRecipeResponse pHttpStatus_ =
  DeleteImageRecipeResponse'
    { imageRecipeArn =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the image recipe that was deleted.
deleteImageRecipeResponse_imageRecipeArn :: Lens.Lens' DeleteImageRecipeResponse (Prelude.Maybe Prelude.Text)
deleteImageRecipeResponse_imageRecipeArn = Lens.lens (\DeleteImageRecipeResponse' {imageRecipeArn} -> imageRecipeArn) (\s@DeleteImageRecipeResponse' {} a -> s {imageRecipeArn = a} :: DeleteImageRecipeResponse)

-- | The request ID that uniquely identifies this request.
deleteImageRecipeResponse_requestId :: Lens.Lens' DeleteImageRecipeResponse (Prelude.Maybe Prelude.Text)
deleteImageRecipeResponse_requestId = Lens.lens (\DeleteImageRecipeResponse' {requestId} -> requestId) (\s@DeleteImageRecipeResponse' {} a -> s {requestId = a} :: DeleteImageRecipeResponse)

-- | The response's http status code.
deleteImageRecipeResponse_httpStatus :: Lens.Lens' DeleteImageRecipeResponse Prelude.Int
deleteImageRecipeResponse_httpStatus = Lens.lens (\DeleteImageRecipeResponse' {httpStatus} -> httpStatus) (\s@DeleteImageRecipeResponse' {} a -> s {httpStatus = a} :: DeleteImageRecipeResponse)

instance Prelude.NFData DeleteImageRecipeResponse where
  rnf DeleteImageRecipeResponse' {..} =
    Prelude.rnf imageRecipeArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
