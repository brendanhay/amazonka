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
-- Module      : Network.AWS.Rekognition.SearchFaces
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- For an example, see Searching for a Face Using Its Face ID in the Amazon
-- Rekognition Developer Guide.
--
-- This operation requires permissions to perform the
-- @rekognition:SearchFaces@ action.
module Network.AWS.Rekognition.SearchFaces
  ( -- * Creating a Request
    SearchFaces (..),
    newSearchFaces,

    -- * Request Lenses
    searchFaces_maxFaces,
    searchFaces_faceMatchThreshold,
    searchFaces_collectionId,
    searchFaces_faceId,

    -- * Destructuring the Response
    SearchFacesResponse (..),
    newSearchFacesResponse,

    -- * Response Lenses
    searchFacesResponse_faceModelVersion,
    searchFacesResponse_faceMatches,
    searchFacesResponse_searchedFaceId,
    searchFacesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchFaces' smart constructor.
data SearchFaces = SearchFaces'
  { -- | Maximum number of faces to return. The operation returns the maximum
    -- number of faces with the highest confidence in the match.
    maxFaces :: Core.Maybe Core.Natural,
    -- | Optional value specifying the minimum confidence in the face match to
    -- return. For example, don\'t return any matches where confidence in
    -- matches is less than 70%. The default value is 80%.
    faceMatchThreshold :: Core.Maybe Core.Double,
    -- | ID of the collection the face belongs to.
    collectionId :: Core.Text,
    -- | ID of a face to find matches for in the collection.
    faceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchFaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxFaces', 'searchFaces_maxFaces' - Maximum number of faces to return. The operation returns the maximum
-- number of faces with the highest confidence in the match.
--
-- 'faceMatchThreshold', 'searchFaces_faceMatchThreshold' - Optional value specifying the minimum confidence in the face match to
-- return. For example, don\'t return any matches where confidence in
-- matches is less than 70%. The default value is 80%.
--
-- 'collectionId', 'searchFaces_collectionId' - ID of the collection the face belongs to.
--
-- 'faceId', 'searchFaces_faceId' - ID of a face to find matches for in the collection.
newSearchFaces ::
  -- | 'collectionId'
  Core.Text ->
  -- | 'faceId'
  Core.Text ->
  SearchFaces
newSearchFaces pCollectionId_ pFaceId_ =
  SearchFaces'
    { maxFaces = Core.Nothing,
      faceMatchThreshold = Core.Nothing,
      collectionId = pCollectionId_,
      faceId = pFaceId_
    }

-- | Maximum number of faces to return. The operation returns the maximum
-- number of faces with the highest confidence in the match.
searchFaces_maxFaces :: Lens.Lens' SearchFaces (Core.Maybe Core.Natural)
searchFaces_maxFaces = Lens.lens (\SearchFaces' {maxFaces} -> maxFaces) (\s@SearchFaces' {} a -> s {maxFaces = a} :: SearchFaces)

-- | Optional value specifying the minimum confidence in the face match to
-- return. For example, don\'t return any matches where confidence in
-- matches is less than 70%. The default value is 80%.
searchFaces_faceMatchThreshold :: Lens.Lens' SearchFaces (Core.Maybe Core.Double)
searchFaces_faceMatchThreshold = Lens.lens (\SearchFaces' {faceMatchThreshold} -> faceMatchThreshold) (\s@SearchFaces' {} a -> s {faceMatchThreshold = a} :: SearchFaces)

-- | ID of the collection the face belongs to.
searchFaces_collectionId :: Lens.Lens' SearchFaces Core.Text
searchFaces_collectionId = Lens.lens (\SearchFaces' {collectionId} -> collectionId) (\s@SearchFaces' {} a -> s {collectionId = a} :: SearchFaces)

-- | ID of a face to find matches for in the collection.
searchFaces_faceId :: Lens.Lens' SearchFaces Core.Text
searchFaces_faceId = Lens.lens (\SearchFaces' {faceId} -> faceId) (\s@SearchFaces' {} a -> s {faceId = a} :: SearchFaces)

instance Core.AWSRequest SearchFaces where
  type AWSResponse SearchFaces = SearchFacesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchFacesResponse'
            Core.<$> (x Core..?> "FaceModelVersion")
            Core.<*> (x Core..?> "FaceMatches" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "SearchedFaceId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SearchFaces

instance Core.NFData SearchFaces

instance Core.ToHeaders SearchFaces where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.SearchFaces" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SearchFaces where
  toJSON SearchFaces' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxFaces" Core..=) Core.<$> maxFaces,
            ("FaceMatchThreshold" Core..=)
              Core.<$> faceMatchThreshold,
            Core.Just ("CollectionId" Core..= collectionId),
            Core.Just ("FaceId" Core..= faceId)
          ]
      )

instance Core.ToPath SearchFaces where
  toPath = Core.const "/"

instance Core.ToQuery SearchFaces where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSearchFacesResponse' smart constructor.
data SearchFacesResponse = SearchFacesResponse'
  { -- | Version number of the face detection model associated with the input
    -- collection (@CollectionId@).
    faceModelVersion :: Core.Maybe Core.Text,
    -- | An array of faces that matched the input face, along with the confidence
    -- in the match.
    faceMatches :: Core.Maybe [FaceMatch],
    -- | ID of the face that was searched for matches in a collection.
    searchedFaceId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchFacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceModelVersion', 'searchFacesResponse_faceModelVersion' - Version number of the face detection model associated with the input
-- collection (@CollectionId@).
--
-- 'faceMatches', 'searchFacesResponse_faceMatches' - An array of faces that matched the input face, along with the confidence
-- in the match.
--
-- 'searchedFaceId', 'searchFacesResponse_searchedFaceId' - ID of the face that was searched for matches in a collection.
--
-- 'httpStatus', 'searchFacesResponse_httpStatus' - The response's http status code.
newSearchFacesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SearchFacesResponse
newSearchFacesResponse pHttpStatus_ =
  SearchFacesResponse'
    { faceModelVersion =
        Core.Nothing,
      faceMatches = Core.Nothing,
      searchedFaceId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Version number of the face detection model associated with the input
-- collection (@CollectionId@).
searchFacesResponse_faceModelVersion :: Lens.Lens' SearchFacesResponse (Core.Maybe Core.Text)
searchFacesResponse_faceModelVersion = Lens.lens (\SearchFacesResponse' {faceModelVersion} -> faceModelVersion) (\s@SearchFacesResponse' {} a -> s {faceModelVersion = a} :: SearchFacesResponse)

-- | An array of faces that matched the input face, along with the confidence
-- in the match.
searchFacesResponse_faceMatches :: Lens.Lens' SearchFacesResponse (Core.Maybe [FaceMatch])
searchFacesResponse_faceMatches = Lens.lens (\SearchFacesResponse' {faceMatches} -> faceMatches) (\s@SearchFacesResponse' {} a -> s {faceMatches = a} :: SearchFacesResponse) Core.. Lens.mapping Lens._Coerce

-- | ID of the face that was searched for matches in a collection.
searchFacesResponse_searchedFaceId :: Lens.Lens' SearchFacesResponse (Core.Maybe Core.Text)
searchFacesResponse_searchedFaceId = Lens.lens (\SearchFacesResponse' {searchedFaceId} -> searchedFaceId) (\s@SearchFacesResponse' {} a -> s {searchedFaceId = a} :: SearchFacesResponse)

-- | The response's http status code.
searchFacesResponse_httpStatus :: Lens.Lens' SearchFacesResponse Core.Int
searchFacesResponse_httpStatus = Lens.lens (\SearchFacesResponse' {httpStatus} -> httpStatus) (\s@SearchFacesResponse' {} a -> s {httpStatus = a} :: SearchFacesResponse)

instance Core.NFData SearchFacesResponse
