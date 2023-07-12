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
-- Module      : Amazonka.MediaStoreData.DeleteObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an object at the specified path.
module Amazonka.MediaStoreData.DeleteObject
  ( -- * Creating a Request
    DeleteObject (..),
    newDeleteObject,

    -- * Request Lenses
    deleteObject_path,

    -- * Destructuring the Response
    DeleteObjectResponse (..),
    newDeleteObjectResponse,

    -- * Response Lenses
    deleteObjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStoreData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteObject' smart constructor.
data DeleteObject = DeleteObject'
  { -- | The path (including the file name) where the object is stored in the
    -- container. Format: \<folder name>\/\<folder name>\/\<file name>
    path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'deleteObject_path' - The path (including the file name) where the object is stored in the
-- container. Format: \<folder name>\/\<folder name>\/\<file name>
newDeleteObject ::
  -- | 'path'
  Prelude.Text ->
  DeleteObject
newDeleteObject pPath_ = DeleteObject' {path = pPath_}

-- | The path (including the file name) where the object is stored in the
-- container. Format: \<folder name>\/\<folder name>\/\<file name>
deleteObject_path :: Lens.Lens' DeleteObject Prelude.Text
deleteObject_path = Lens.lens (\DeleteObject' {path} -> path) (\s@DeleteObject' {} a -> s {path = a} :: DeleteObject)

instance Core.AWSRequest DeleteObject where
  type AWSResponse DeleteObject = DeleteObjectResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteObjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteObject where
  hashWithSalt _salt DeleteObject' {..} =
    _salt `Prelude.hashWithSalt` path

instance Prelude.NFData DeleteObject where
  rnf DeleteObject' {..} = Prelude.rnf path

instance Data.ToHeaders DeleteObject where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteObject where
  toPath DeleteObject' {..} =
    Prelude.mconcat ["/", Data.toBS path]

instance Data.ToQuery DeleteObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteObjectResponse' smart constructor.
data DeleteObjectResponse = DeleteObjectResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteObjectResponse_httpStatus' - The response's http status code.
newDeleteObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteObjectResponse
newDeleteObjectResponse pHttpStatus_ =
  DeleteObjectResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteObjectResponse_httpStatus :: Lens.Lens' DeleteObjectResponse Prelude.Int
deleteObjectResponse_httpStatus = Lens.lens (\DeleteObjectResponse' {httpStatus} -> httpStatus) (\s@DeleteObjectResponse' {} a -> s {httpStatus = a} :: DeleteObjectResponse)

instance Prelude.NFData DeleteObjectResponse where
  rnf DeleteObjectResponse' {..} =
    Prelude.rnf httpStatus
