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
-- Module      : Amazonka.Rekognition.DeleteFaces
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes faces from a collection. You specify a collection ID and an
-- array of face IDs to remove from the collection.
--
-- This operation requires permissions to perform the
-- @rekognition:DeleteFaces@ action.
module Amazonka.Rekognition.DeleteFaces
  ( -- * Creating a Request
    DeleteFaces (..),
    newDeleteFaces,

    -- * Request Lenses
    deleteFaces_collectionId,
    deleteFaces_faceIds,

    -- * Destructuring the Response
    DeleteFacesResponse (..),
    newDeleteFacesResponse,

    -- * Response Lenses
    deleteFacesResponse_deletedFaces,
    deleteFacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFaces' smart constructor.
data DeleteFaces = DeleteFaces'
  { -- | Collection from which to remove the specific faces.
    collectionId :: Prelude.Text,
    -- | An array of face IDs to delete.
    faceIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionId', 'deleteFaces_collectionId' - Collection from which to remove the specific faces.
--
-- 'faceIds', 'deleteFaces_faceIds' - An array of face IDs to delete.
newDeleteFaces ::
  -- | 'collectionId'
  Prelude.Text ->
  -- | 'faceIds'
  Prelude.NonEmpty Prelude.Text ->
  DeleteFaces
newDeleteFaces pCollectionId_ pFaceIds_ =
  DeleteFaces'
    { collectionId = pCollectionId_,
      faceIds = Lens.coerced Lens.# pFaceIds_
    }

-- | Collection from which to remove the specific faces.
deleteFaces_collectionId :: Lens.Lens' DeleteFaces Prelude.Text
deleteFaces_collectionId = Lens.lens (\DeleteFaces' {collectionId} -> collectionId) (\s@DeleteFaces' {} a -> s {collectionId = a} :: DeleteFaces)

-- | An array of face IDs to delete.
deleteFaces_faceIds :: Lens.Lens' DeleteFaces (Prelude.NonEmpty Prelude.Text)
deleteFaces_faceIds = Lens.lens (\DeleteFaces' {faceIds} -> faceIds) (\s@DeleteFaces' {} a -> s {faceIds = a} :: DeleteFaces) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteFaces where
  type AWSResponse DeleteFaces = DeleteFacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFacesResponse'
            Prelude.<$> (x Data..?> "DeletedFaces")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFaces where
  hashWithSalt _salt DeleteFaces' {..} =
    _salt
      `Prelude.hashWithSalt` collectionId
      `Prelude.hashWithSalt` faceIds

instance Prelude.NFData DeleteFaces where
  rnf DeleteFaces' {..} =
    Prelude.rnf collectionId
      `Prelude.seq` Prelude.rnf faceIds

instance Data.ToHeaders DeleteFaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.DeleteFaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFaces where
  toJSON DeleteFaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("CollectionId" Data..= collectionId),
            Prelude.Just ("FaceIds" Data..= faceIds)
          ]
      )

instance Data.ToPath DeleteFaces where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFacesResponse' smart constructor.
data DeleteFacesResponse = DeleteFacesResponse'
  { -- | An array of strings (face IDs) of the faces that were deleted.
    deletedFaces :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletedFaces', 'deleteFacesResponse_deletedFaces' - An array of strings (face IDs) of the faces that were deleted.
--
-- 'httpStatus', 'deleteFacesResponse_httpStatus' - The response's http status code.
newDeleteFacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFacesResponse
newDeleteFacesResponse pHttpStatus_ =
  DeleteFacesResponse'
    { deletedFaces =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of strings (face IDs) of the faces that were deleted.
deleteFacesResponse_deletedFaces :: Lens.Lens' DeleteFacesResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
deleteFacesResponse_deletedFaces = Lens.lens (\DeleteFacesResponse' {deletedFaces} -> deletedFaces) (\s@DeleteFacesResponse' {} a -> s {deletedFaces = a} :: DeleteFacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteFacesResponse_httpStatus :: Lens.Lens' DeleteFacesResponse Prelude.Int
deleteFacesResponse_httpStatus = Lens.lens (\DeleteFacesResponse' {httpStatus} -> httpStatus) (\s@DeleteFacesResponse' {} a -> s {httpStatus = a} :: DeleteFacesResponse)

instance Prelude.NFData DeleteFacesResponse where
  rnf DeleteFacesResponse' {..} =
    Prelude.rnf deletedFaces
      `Prelude.seq` Prelude.rnf httpStatus
