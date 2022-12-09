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
-- Module      : Amazonka.Rekognition.SearchFaces
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a given input face ID, searches for matching faces in the collection
-- the face belongs to. You get a face ID when you add a face to the
-- collection using the IndexFaces operation. The operation compares the
-- features of the input face with faces in the specified collection.
--
-- You can also search faces without indexing faces by using the
-- @SearchFacesByImage@ operation.
--
-- The operation response returns an array of faces that match, ordered by
-- similarity score with the highest similarity first. More specifically,
-- it is an array of metadata for each face match that is found. Along with
-- the metadata, the response also includes a @confidence@ value for each
-- face match, indicating the confidence that the specific face matches the
-- input face.
--
-- For an example, see Searching for a face using its face ID in the Amazon
-- Rekognition Developer Guide.
--
-- This operation requires permissions to perform the
-- @rekognition:SearchFaces@ action.
module Amazonka.Rekognition.SearchFaces
  ( -- * Creating a Request
    SearchFaces (..),
    newSearchFaces,

    -- * Request Lenses
    searchFaces_faceMatchThreshold,
    searchFaces_maxFaces,
    searchFaces_collectionId,
    searchFaces_faceId,

    -- * Destructuring the Response
    SearchFacesResponse (..),
    newSearchFacesResponse,

    -- * Response Lenses
    searchFacesResponse_faceMatches,
    searchFacesResponse_faceModelVersion,
    searchFacesResponse_searchedFaceId,
    searchFacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchFaces' smart constructor.
data SearchFaces = SearchFaces'
  { -- | Optional value specifying the minimum confidence in the face match to
    -- return. For example, don\'t return any matches where confidence in
    -- matches is less than 70%. The default value is 80%.
    faceMatchThreshold :: Prelude.Maybe Prelude.Double,
    -- | Maximum number of faces to return. The operation returns the maximum
    -- number of faces with the highest confidence in the match.
    maxFaces :: Prelude.Maybe Prelude.Natural,
    -- | ID of the collection the face belongs to.
    collectionId :: Prelude.Text,
    -- | ID of a face to find matches for in the collection.
    faceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchFaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceMatchThreshold', 'searchFaces_faceMatchThreshold' - Optional value specifying the minimum confidence in the face match to
-- return. For example, don\'t return any matches where confidence in
-- matches is less than 70%. The default value is 80%.
--
-- 'maxFaces', 'searchFaces_maxFaces' - Maximum number of faces to return. The operation returns the maximum
-- number of faces with the highest confidence in the match.
--
-- 'collectionId', 'searchFaces_collectionId' - ID of the collection the face belongs to.
--
-- 'faceId', 'searchFaces_faceId' - ID of a face to find matches for in the collection.
newSearchFaces ::
  -- | 'collectionId'
  Prelude.Text ->
  -- | 'faceId'
  Prelude.Text ->
  SearchFaces
newSearchFaces pCollectionId_ pFaceId_ =
  SearchFaces'
    { faceMatchThreshold = Prelude.Nothing,
      maxFaces = Prelude.Nothing,
      collectionId = pCollectionId_,
      faceId = pFaceId_
    }

-- | Optional value specifying the minimum confidence in the face match to
-- return. For example, don\'t return any matches where confidence in
-- matches is less than 70%. The default value is 80%.
searchFaces_faceMatchThreshold :: Lens.Lens' SearchFaces (Prelude.Maybe Prelude.Double)
searchFaces_faceMatchThreshold = Lens.lens (\SearchFaces' {faceMatchThreshold} -> faceMatchThreshold) (\s@SearchFaces' {} a -> s {faceMatchThreshold = a} :: SearchFaces)

-- | Maximum number of faces to return. The operation returns the maximum
-- number of faces with the highest confidence in the match.
searchFaces_maxFaces :: Lens.Lens' SearchFaces (Prelude.Maybe Prelude.Natural)
searchFaces_maxFaces = Lens.lens (\SearchFaces' {maxFaces} -> maxFaces) (\s@SearchFaces' {} a -> s {maxFaces = a} :: SearchFaces)

-- | ID of the collection the face belongs to.
searchFaces_collectionId :: Lens.Lens' SearchFaces Prelude.Text
searchFaces_collectionId = Lens.lens (\SearchFaces' {collectionId} -> collectionId) (\s@SearchFaces' {} a -> s {collectionId = a} :: SearchFaces)

-- | ID of a face to find matches for in the collection.
searchFaces_faceId :: Lens.Lens' SearchFaces Prelude.Text
searchFaces_faceId = Lens.lens (\SearchFaces' {faceId} -> faceId) (\s@SearchFaces' {} a -> s {faceId = a} :: SearchFaces)

instance Core.AWSRequest SearchFaces where
  type AWSResponse SearchFaces = SearchFacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchFacesResponse'
            Prelude.<$> (x Data..?> "FaceMatches" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "FaceModelVersion")
            Prelude.<*> (x Data..?> "SearchedFaceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchFaces where
  hashWithSalt _salt SearchFaces' {..} =
    _salt `Prelude.hashWithSalt` faceMatchThreshold
      `Prelude.hashWithSalt` maxFaces
      `Prelude.hashWithSalt` collectionId
      `Prelude.hashWithSalt` faceId

instance Prelude.NFData SearchFaces where
  rnf SearchFaces' {..} =
    Prelude.rnf faceMatchThreshold
      `Prelude.seq` Prelude.rnf maxFaces
      `Prelude.seq` Prelude.rnf collectionId
      `Prelude.seq` Prelude.rnf faceId

instance Data.ToHeaders SearchFaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.SearchFaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchFaces where
  toJSON SearchFaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FaceMatchThreshold" Data..=)
              Prelude.<$> faceMatchThreshold,
            ("MaxFaces" Data..=) Prelude.<$> maxFaces,
            Prelude.Just ("CollectionId" Data..= collectionId),
            Prelude.Just ("FaceId" Data..= faceId)
          ]
      )

instance Data.ToPath SearchFaces where
  toPath = Prelude.const "/"

instance Data.ToQuery SearchFaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchFacesResponse' smart constructor.
data SearchFacesResponse = SearchFacesResponse'
  { -- | An array of faces that matched the input face, along with the confidence
    -- in the match.
    faceMatches :: Prelude.Maybe [FaceMatch],
    -- | Version number of the face detection model associated with the input
    -- collection (@CollectionId@).
    faceModelVersion :: Prelude.Maybe Prelude.Text,
    -- | ID of the face that was searched for matches in a collection.
    searchedFaceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchFacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceMatches', 'searchFacesResponse_faceMatches' - An array of faces that matched the input face, along with the confidence
-- in the match.
--
-- 'faceModelVersion', 'searchFacesResponse_faceModelVersion' - Version number of the face detection model associated with the input
-- collection (@CollectionId@).
--
-- 'searchedFaceId', 'searchFacesResponse_searchedFaceId' - ID of the face that was searched for matches in a collection.
--
-- 'httpStatus', 'searchFacesResponse_httpStatus' - The response's http status code.
newSearchFacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchFacesResponse
newSearchFacesResponse pHttpStatus_ =
  SearchFacesResponse'
    { faceMatches = Prelude.Nothing,
      faceModelVersion = Prelude.Nothing,
      searchedFaceId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of faces that matched the input face, along with the confidence
-- in the match.
searchFacesResponse_faceMatches :: Lens.Lens' SearchFacesResponse (Prelude.Maybe [FaceMatch])
searchFacesResponse_faceMatches = Lens.lens (\SearchFacesResponse' {faceMatches} -> faceMatches) (\s@SearchFacesResponse' {} a -> s {faceMatches = a} :: SearchFacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Version number of the face detection model associated with the input
-- collection (@CollectionId@).
searchFacesResponse_faceModelVersion :: Lens.Lens' SearchFacesResponse (Prelude.Maybe Prelude.Text)
searchFacesResponse_faceModelVersion = Lens.lens (\SearchFacesResponse' {faceModelVersion} -> faceModelVersion) (\s@SearchFacesResponse' {} a -> s {faceModelVersion = a} :: SearchFacesResponse)

-- | ID of the face that was searched for matches in a collection.
searchFacesResponse_searchedFaceId :: Lens.Lens' SearchFacesResponse (Prelude.Maybe Prelude.Text)
searchFacesResponse_searchedFaceId = Lens.lens (\SearchFacesResponse' {searchedFaceId} -> searchedFaceId) (\s@SearchFacesResponse' {} a -> s {searchedFaceId = a} :: SearchFacesResponse)

-- | The response's http status code.
searchFacesResponse_httpStatus :: Lens.Lens' SearchFacesResponse Prelude.Int
searchFacesResponse_httpStatus = Lens.lens (\SearchFacesResponse' {httpStatus} -> httpStatus) (\s@SearchFacesResponse' {} a -> s {httpStatus = a} :: SearchFacesResponse)

instance Prelude.NFData SearchFacesResponse where
  rnf SearchFacesResponse' {..} =
    Prelude.rnf faceMatches
      `Prelude.seq` Prelude.rnf faceModelVersion
      `Prelude.seq` Prelude.rnf searchedFaceId
      `Prelude.seq` Prelude.rnf httpStatus
