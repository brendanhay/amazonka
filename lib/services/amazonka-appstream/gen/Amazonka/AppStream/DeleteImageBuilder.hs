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
-- Module      : Amazonka.AppStream.DeleteImageBuilder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified image builder and releases the capacity.
module Amazonka.AppStream.DeleteImageBuilder
  ( -- * Creating a Request
    DeleteImageBuilder (..),
    newDeleteImageBuilder,

    -- * Request Lenses
    deleteImageBuilder_name,

    -- * Destructuring the Response
    DeleteImageBuilderResponse (..),
    newDeleteImageBuilderResponse,

    -- * Response Lenses
    deleteImageBuilderResponse_imageBuilder,
    deleteImageBuilderResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteImageBuilder' smart constructor.
data DeleteImageBuilder = DeleteImageBuilder'
  { -- | The name of the image builder.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImageBuilder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteImageBuilder_name' - The name of the image builder.
newDeleteImageBuilder ::
  -- | 'name'
  Prelude.Text ->
  DeleteImageBuilder
newDeleteImageBuilder pName_ =
  DeleteImageBuilder' {name = pName_}

-- | The name of the image builder.
deleteImageBuilder_name :: Lens.Lens' DeleteImageBuilder Prelude.Text
deleteImageBuilder_name = Lens.lens (\DeleteImageBuilder' {name} -> name) (\s@DeleteImageBuilder' {} a -> s {name = a} :: DeleteImageBuilder)

instance Core.AWSRequest DeleteImageBuilder where
  type
    AWSResponse DeleteImageBuilder =
      DeleteImageBuilderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteImageBuilderResponse'
            Prelude.<$> (x Data..?> "ImageBuilder")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteImageBuilder where
  hashWithSalt _salt DeleteImageBuilder' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteImageBuilder where
  rnf DeleteImageBuilder' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteImageBuilder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DeleteImageBuilder" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteImageBuilder where
  toJSON DeleteImageBuilder' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteImageBuilder where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteImageBuilder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteImageBuilderResponse' smart constructor.
data DeleteImageBuilderResponse = DeleteImageBuilderResponse'
  { -- | Information about the image builder.
    imageBuilder :: Prelude.Maybe ImageBuilder,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImageBuilderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageBuilder', 'deleteImageBuilderResponse_imageBuilder' - Information about the image builder.
--
-- 'httpStatus', 'deleteImageBuilderResponse_httpStatus' - The response's http status code.
newDeleteImageBuilderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteImageBuilderResponse
newDeleteImageBuilderResponse pHttpStatus_ =
  DeleteImageBuilderResponse'
    { imageBuilder =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the image builder.
deleteImageBuilderResponse_imageBuilder :: Lens.Lens' DeleteImageBuilderResponse (Prelude.Maybe ImageBuilder)
deleteImageBuilderResponse_imageBuilder = Lens.lens (\DeleteImageBuilderResponse' {imageBuilder} -> imageBuilder) (\s@DeleteImageBuilderResponse' {} a -> s {imageBuilder = a} :: DeleteImageBuilderResponse)

-- | The response's http status code.
deleteImageBuilderResponse_httpStatus :: Lens.Lens' DeleteImageBuilderResponse Prelude.Int
deleteImageBuilderResponse_httpStatus = Lens.lens (\DeleteImageBuilderResponse' {httpStatus} -> httpStatus) (\s@DeleteImageBuilderResponse' {} a -> s {httpStatus = a} :: DeleteImageBuilderResponse)

instance Prelude.NFData DeleteImageBuilderResponse where
  rnf DeleteImageBuilderResponse' {..} =
    Prelude.rnf imageBuilder
      `Prelude.seq` Prelude.rnf httpStatus
