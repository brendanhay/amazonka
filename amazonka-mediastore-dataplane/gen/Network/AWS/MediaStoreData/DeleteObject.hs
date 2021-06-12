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
-- Module      : Network.AWS.MediaStoreData.DeleteObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an object at the specified path.
module Network.AWS.MediaStoreData.DeleteObject
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStoreData.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteObject' smart constructor.
data DeleteObject = DeleteObject'
  { -- | The path (including the file name) where the object is stored in the
    -- container. Format: \<folder name>\/\<folder name>\/\<file name>
    path :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteObject
newDeleteObject pPath_ = DeleteObject' {path = pPath_}

-- | The path (including the file name) where the object is stored in the
-- container. Format: \<folder name>\/\<folder name>\/\<file name>
deleteObject_path :: Lens.Lens' DeleteObject Core.Text
deleteObject_path = Lens.lens (\DeleteObject' {path} -> path) (\s@DeleteObject' {} a -> s {path = a} :: DeleteObject)

instance Core.AWSRequest DeleteObject where
  type AWSResponse DeleteObject = DeleteObjectResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteObjectResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteObject

instance Core.NFData DeleteObject

instance Core.ToHeaders DeleteObject where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteObject where
  toPath DeleteObject' {..} =
    Core.mconcat ["/", Core.toBS path]

instance Core.ToQuery DeleteObject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteObjectResponse' smart constructor.
data DeleteObjectResponse = DeleteObjectResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteObjectResponse
newDeleteObjectResponse pHttpStatus_ =
  DeleteObjectResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteObjectResponse_httpStatus :: Lens.Lens' DeleteObjectResponse Core.Int
deleteObjectResponse_httpStatus = Lens.lens (\DeleteObjectResponse' {httpStatus} -> httpStatus) (\s@DeleteObjectResponse' {} a -> s {httpStatus = a} :: DeleteObjectResponse)

instance Core.NFData DeleteObjectResponse
