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
-- Module      : Network.AWS.Rekognition.ListCollections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of collection IDs in your account. If the result is
-- truncated, the response also provides a @NextToken@ that you can use in
-- the subsequent request to fetch the next set of collection IDs.
--
-- For an example, see Listing Collections in the Amazon Rekognition
-- Developer Guide.
--
-- This operation requires permissions to perform the
-- @rekognition:ListCollections@ action.
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.ListCollections
  ( -- * Creating a Request
    ListCollections (..),
    newListCollections,

    -- * Request Lenses
    listCollections_nextToken,
    listCollections_maxResults,

    -- * Destructuring the Response
    ListCollectionsResponse (..),
    newListCollectionsResponse,

    -- * Response Lenses
    listCollectionsResponse_faceModelVersions,
    listCollectionsResponse_nextToken,
    listCollectionsResponse_collectionIds,
    listCollectionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListCollections' smart constructor.
data ListCollections = ListCollections'
  { -- | Pagination token from the previous response.
    nextToken :: Core.Maybe Core.Text,
    -- | Maximum number of collection IDs to return.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCollections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCollections_nextToken' - Pagination token from the previous response.
--
-- 'maxResults', 'listCollections_maxResults' - Maximum number of collection IDs to return.
newListCollections ::
  ListCollections
newListCollections =
  ListCollections'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | Pagination token from the previous response.
listCollections_nextToken :: Lens.Lens' ListCollections (Core.Maybe Core.Text)
listCollections_nextToken = Lens.lens (\ListCollections' {nextToken} -> nextToken) (\s@ListCollections' {} a -> s {nextToken = a} :: ListCollections)

-- | Maximum number of collection IDs to return.
listCollections_maxResults :: Lens.Lens' ListCollections (Core.Maybe Core.Natural)
listCollections_maxResults = Lens.lens (\ListCollections' {maxResults} -> maxResults) (\s@ListCollections' {} a -> s {maxResults = a} :: ListCollections)

instance Core.AWSPager ListCollections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCollectionsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listCollectionsResponse_collectionIds
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listCollectionsResponse_faceModelVersions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listCollections_nextToken
          Lens..~ rs
          Lens.^? listCollectionsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListCollections where
  type
    AWSResponse ListCollections =
      ListCollectionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCollectionsResponse'
            Core.<$> (x Core..?> "FaceModelVersions" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "CollectionIds" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListCollections

instance Core.NFData ListCollections

instance Core.ToHeaders ListCollections where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.ListCollections" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListCollections where
  toJSON ListCollections' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListCollections where
  toPath = Core.const "/"

instance Core.ToQuery ListCollections where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListCollectionsResponse' smart constructor.
data ListCollectionsResponse = ListCollectionsResponse'
  { -- | Version numbers of the face detection models associated with the
    -- collections in the array @CollectionIds@. For example, the value of
    -- @FaceModelVersions[2]@ is the version number for the face detection
    -- model used by the collection in @CollectionId[2]@.
    faceModelVersions :: Core.Maybe [Core.Text],
    -- | If the result is truncated, the response provides a @NextToken@ that you
    -- can use in the subsequent request to fetch the next set of collection
    -- IDs.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of collection IDs.
    collectionIds :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCollectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceModelVersions', 'listCollectionsResponse_faceModelVersions' - Version numbers of the face detection models associated with the
-- collections in the array @CollectionIds@. For example, the value of
-- @FaceModelVersions[2]@ is the version number for the face detection
-- model used by the collection in @CollectionId[2]@.
--
-- 'nextToken', 'listCollectionsResponse_nextToken' - If the result is truncated, the response provides a @NextToken@ that you
-- can use in the subsequent request to fetch the next set of collection
-- IDs.
--
-- 'collectionIds', 'listCollectionsResponse_collectionIds' - An array of collection IDs.
--
-- 'httpStatus', 'listCollectionsResponse_httpStatus' - The response's http status code.
newListCollectionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListCollectionsResponse
newListCollectionsResponse pHttpStatus_ =
  ListCollectionsResponse'
    { faceModelVersions =
        Core.Nothing,
      nextToken = Core.Nothing,
      collectionIds = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Version numbers of the face detection models associated with the
-- collections in the array @CollectionIds@. For example, the value of
-- @FaceModelVersions[2]@ is the version number for the face detection
-- model used by the collection in @CollectionId[2]@.
listCollectionsResponse_faceModelVersions :: Lens.Lens' ListCollectionsResponse (Core.Maybe [Core.Text])
listCollectionsResponse_faceModelVersions = Lens.lens (\ListCollectionsResponse' {faceModelVersions} -> faceModelVersions) (\s@ListCollectionsResponse' {} a -> s {faceModelVersions = a} :: ListCollectionsResponse) Core.. Lens.mapping Lens._Coerce

-- | If the result is truncated, the response provides a @NextToken@ that you
-- can use in the subsequent request to fetch the next set of collection
-- IDs.
listCollectionsResponse_nextToken :: Lens.Lens' ListCollectionsResponse (Core.Maybe Core.Text)
listCollectionsResponse_nextToken = Lens.lens (\ListCollectionsResponse' {nextToken} -> nextToken) (\s@ListCollectionsResponse' {} a -> s {nextToken = a} :: ListCollectionsResponse)

-- | An array of collection IDs.
listCollectionsResponse_collectionIds :: Lens.Lens' ListCollectionsResponse (Core.Maybe [Core.Text])
listCollectionsResponse_collectionIds = Lens.lens (\ListCollectionsResponse' {collectionIds} -> collectionIds) (\s@ListCollectionsResponse' {} a -> s {collectionIds = a} :: ListCollectionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCollectionsResponse_httpStatus :: Lens.Lens' ListCollectionsResponse Core.Int
listCollectionsResponse_httpStatus = Lens.lens (\ListCollectionsResponse' {httpStatus} -> httpStatus) (\s@ListCollectionsResponse' {} a -> s {httpStatus = a} :: ListCollectionsResponse)

instance Core.NFData ListCollectionsResponse
