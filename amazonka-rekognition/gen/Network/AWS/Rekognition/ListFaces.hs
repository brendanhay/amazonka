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
-- Module      : Network.AWS.Rekognition.ListFaces
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata for faces in the specified collection. This metadata
-- includes information such as the bounding box coordinates, the
-- confidence (that the bounding box contains a face), and face ID. For an
-- example, see Listing Faces in a Collection in the Amazon Rekognition
-- Developer Guide.
--
-- This operation requires permissions to perform the
-- @rekognition:ListFaces@ action.
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.ListFaces
  ( -- * Creating a Request
    ListFaces (..),
    newListFaces,

    -- * Request Lenses
    listFaces_nextToken,
    listFaces_maxResults,
    listFaces_collectionId,

    -- * Destructuring the Response
    ListFacesResponse (..),
    newListFacesResponse,

    -- * Response Lenses
    listFacesResponse_faceModelVersion,
    listFacesResponse_nextToken,
    listFacesResponse_faces,
    listFacesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFaces' smart constructor.
data ListFaces = ListFaces'
  { -- | If the previous response was incomplete (because there is more data to
    -- retrieve), Amazon Rekognition returns a pagination token in the
    -- response. You can use this pagination token to retrieve the next set of
    -- faces.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of faces to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | ID of the collection from which to list the faces.
    collectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFaces_nextToken' - If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Rekognition returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- faces.
--
-- 'maxResults', 'listFaces_maxResults' - Maximum number of faces to return.
--
-- 'collectionId', 'listFaces_collectionId' - ID of the collection from which to list the faces.
newListFaces ::
  -- | 'collectionId'
  Prelude.Text ->
  ListFaces
newListFaces pCollectionId_ =
  ListFaces'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      collectionId = pCollectionId_
    }

-- | If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Rekognition returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- faces.
listFaces_nextToken :: Lens.Lens' ListFaces (Prelude.Maybe Prelude.Text)
listFaces_nextToken = Lens.lens (\ListFaces' {nextToken} -> nextToken) (\s@ListFaces' {} a -> s {nextToken = a} :: ListFaces)

-- | Maximum number of faces to return.
listFaces_maxResults :: Lens.Lens' ListFaces (Prelude.Maybe Prelude.Natural)
listFaces_maxResults = Lens.lens (\ListFaces' {maxResults} -> maxResults) (\s@ListFaces' {} a -> s {maxResults = a} :: ListFaces)

-- | ID of the collection from which to list the faces.
listFaces_collectionId :: Lens.Lens' ListFaces Prelude.Text
listFaces_collectionId = Lens.lens (\ListFaces' {collectionId} -> collectionId) (\s@ListFaces' {} a -> s {collectionId = a} :: ListFaces)

instance Core.AWSPager ListFaces where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFacesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFacesResponse_faces Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFaces_nextToken
          Lens..~ rs
          Lens.^? listFacesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListFaces where
  type AWSResponse ListFaces = ListFacesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFacesResponse'
            Prelude.<$> (x Core..?> "FaceModelVersion")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Faces" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFaces

instance Prelude.NFData ListFaces

instance Core.ToHeaders ListFaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.ListFaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListFaces where
  toJSON ListFaces' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("CollectionId" Core..= collectionId)
          ]
      )

instance Core.ToPath ListFaces where
  toPath = Prelude.const "/"

instance Core.ToQuery ListFaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFacesResponse' smart constructor.
data ListFacesResponse = ListFacesResponse'
  { -- | Version number of the face detection model associated with the input
    -- collection (@CollectionId@).
    faceModelVersion :: Prelude.Maybe Prelude.Text,
    -- | If the response is truncated, Amazon Rekognition returns this token that
    -- you can use in the subsequent request to retrieve the next set of faces.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @Face@ objects.
    faces :: Prelude.Maybe [Face],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceModelVersion', 'listFacesResponse_faceModelVersion' - Version number of the face detection model associated with the input
-- collection (@CollectionId@).
--
-- 'nextToken', 'listFacesResponse_nextToken' - If the response is truncated, Amazon Rekognition returns this token that
-- you can use in the subsequent request to retrieve the next set of faces.
--
-- 'faces', 'listFacesResponse_faces' - An array of @Face@ objects.
--
-- 'httpStatus', 'listFacesResponse_httpStatus' - The response's http status code.
newListFacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFacesResponse
newListFacesResponse pHttpStatus_ =
  ListFacesResponse'
    { faceModelVersion =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      faces = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Version number of the face detection model associated with the input
-- collection (@CollectionId@).
listFacesResponse_faceModelVersion :: Lens.Lens' ListFacesResponse (Prelude.Maybe Prelude.Text)
listFacesResponse_faceModelVersion = Lens.lens (\ListFacesResponse' {faceModelVersion} -> faceModelVersion) (\s@ListFacesResponse' {} a -> s {faceModelVersion = a} :: ListFacesResponse)

-- | If the response is truncated, Amazon Rekognition returns this token that
-- you can use in the subsequent request to retrieve the next set of faces.
listFacesResponse_nextToken :: Lens.Lens' ListFacesResponse (Prelude.Maybe Prelude.Text)
listFacesResponse_nextToken = Lens.lens (\ListFacesResponse' {nextToken} -> nextToken) (\s@ListFacesResponse' {} a -> s {nextToken = a} :: ListFacesResponse)

-- | An array of @Face@ objects.
listFacesResponse_faces :: Lens.Lens' ListFacesResponse (Prelude.Maybe [Face])
listFacesResponse_faces = Lens.lens (\ListFacesResponse' {faces} -> faces) (\s@ListFacesResponse' {} a -> s {faces = a} :: ListFacesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listFacesResponse_httpStatus :: Lens.Lens' ListFacesResponse Prelude.Int
listFacesResponse_httpStatus = Lens.lens (\ListFacesResponse' {httpStatus} -> httpStatus) (\s@ListFacesResponse' {} a -> s {httpStatus = a} :: ListFacesResponse)

instance Prelude.NFData ListFacesResponse
