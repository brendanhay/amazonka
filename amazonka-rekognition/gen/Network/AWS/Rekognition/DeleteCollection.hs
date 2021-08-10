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
-- Module      : Network.AWS.Rekognition.DeleteCollection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified collection. Note that this operation removes all
-- faces in the collection. For an example, see
-- delete-collection-procedure.
--
-- This operation requires permissions to perform the
-- @rekognition:DeleteCollection@ action.
module Network.AWS.Rekognition.DeleteCollection
  ( -- * Creating a Request
    DeleteCollection (..),
    newDeleteCollection,

    -- * Request Lenses
    deleteCollection_collectionId,

    -- * Destructuring the Response
    DeleteCollectionResponse (..),
    newDeleteCollectionResponse,

    -- * Response Lenses
    deleteCollectionResponse_statusCode,
    deleteCollectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCollection' smart constructor.
data DeleteCollection = DeleteCollection'
  { -- | ID of the collection to delete.
    collectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionId', 'deleteCollection_collectionId' - ID of the collection to delete.
newDeleteCollection ::
  -- | 'collectionId'
  Prelude.Text ->
  DeleteCollection
newDeleteCollection pCollectionId_ =
  DeleteCollection' {collectionId = pCollectionId_}

-- | ID of the collection to delete.
deleteCollection_collectionId :: Lens.Lens' DeleteCollection Prelude.Text
deleteCollection_collectionId = Lens.lens (\DeleteCollection' {collectionId} -> collectionId) (\s@DeleteCollection' {} a -> s {collectionId = a} :: DeleteCollection)

instance Core.AWSRequest DeleteCollection where
  type
    AWSResponse DeleteCollection =
      DeleteCollectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteCollectionResponse'
            Prelude.<$> (x Core..?> "StatusCode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCollection

instance Prelude.NFData DeleteCollection

instance Core.ToHeaders DeleteCollection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.DeleteCollection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteCollection where
  toJSON DeleteCollection' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("CollectionId" Core..= collectionId)]
      )

instance Core.ToPath DeleteCollection where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteCollection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCollectionResponse' smart constructor.
data DeleteCollectionResponse = DeleteCollectionResponse'
  { -- | HTTP status code that indicates the result of the operation.
    statusCode :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusCode', 'deleteCollectionResponse_statusCode' - HTTP status code that indicates the result of the operation.
--
-- 'httpStatus', 'deleteCollectionResponse_httpStatus' - The response's http status code.
newDeleteCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCollectionResponse
newDeleteCollectionResponse pHttpStatus_ =
  DeleteCollectionResponse'
    { statusCode =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | HTTP status code that indicates the result of the operation.
deleteCollectionResponse_statusCode :: Lens.Lens' DeleteCollectionResponse (Prelude.Maybe Prelude.Natural)
deleteCollectionResponse_statusCode = Lens.lens (\DeleteCollectionResponse' {statusCode} -> statusCode) (\s@DeleteCollectionResponse' {} a -> s {statusCode = a} :: DeleteCollectionResponse)

-- | The response's http status code.
deleteCollectionResponse_httpStatus :: Lens.Lens' DeleteCollectionResponse Prelude.Int
deleteCollectionResponse_httpStatus = Lens.lens (\DeleteCollectionResponse' {httpStatus} -> httpStatus) (\s@DeleteCollectionResponse' {} a -> s {httpStatus = a} :: DeleteCollectionResponse)

instance Prelude.NFData DeleteCollectionResponse
