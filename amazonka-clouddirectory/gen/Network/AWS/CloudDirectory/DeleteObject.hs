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
-- Module      : Network.AWS.CloudDirectory.DeleteObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an object and its associated attributes. Only objects with no
-- children and no parents can be deleted. The maximum number of attributes
-- that can be deleted during an object deletion is 30. For more
-- information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/limits.html Amazon Cloud Directory Limits>.
module Network.AWS.CloudDirectory.DeleteObject
  ( -- * Creating a Request
    DeleteObject (..),
    newDeleteObject,

    -- * Request Lenses
    deleteObject_directoryArn,
    deleteObject_objectReference,

    -- * Destructuring the Response
    DeleteObjectResponse (..),
    newDeleteObjectResponse,

    -- * Response Lenses
    deleteObjectResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteObject' smart constructor.
data DeleteObject = DeleteObject'
  { -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where the object resides. For more information, see arns.
    directoryArn :: Core.Text,
    -- | A reference that identifies the object.
    objectReference :: ObjectReference
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
-- 'directoryArn', 'deleteObject_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
--
-- 'objectReference', 'deleteObject_objectReference' - A reference that identifies the object.
newDeleteObject ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'objectReference'
  ObjectReference ->
  DeleteObject
newDeleteObject pDirectoryArn_ pObjectReference_ =
  DeleteObject'
    { directoryArn = pDirectoryArn_,
      objectReference = pObjectReference_
    }

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
deleteObject_directoryArn :: Lens.Lens' DeleteObject Core.Text
deleteObject_directoryArn = Lens.lens (\DeleteObject' {directoryArn} -> directoryArn) (\s@DeleteObject' {} a -> s {directoryArn = a} :: DeleteObject)

-- | A reference that identifies the object.
deleteObject_objectReference :: Lens.Lens' DeleteObject ObjectReference
deleteObject_objectReference = Lens.lens (\DeleteObject' {objectReference} -> objectReference) (\s@DeleteObject' {} a -> s {objectReference = a} :: DeleteObject)

instance Core.AWSRequest DeleteObject where
  type AWSResponse DeleteObject = DeleteObjectResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteObjectResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteObject

instance Core.NFData DeleteObject

instance Core.ToHeaders DeleteObject where
  toHeaders DeleteObject' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON DeleteObject where
  toJSON DeleteObject' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.ToPath DeleteObject where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/object/delete"

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
