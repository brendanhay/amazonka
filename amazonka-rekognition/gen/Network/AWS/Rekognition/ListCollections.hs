{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListCollections' smart constructor.
data ListCollections = ListCollections'
  { -- | Pagination token from the previous response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of collection IDs to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Pagination token from the previous response.
listCollections_nextToken :: Lens.Lens' ListCollections (Prelude.Maybe Prelude.Text)
listCollections_nextToken = Lens.lens (\ListCollections' {nextToken} -> nextToken) (\s@ListCollections' {} a -> s {nextToken = a} :: ListCollections)

-- | Maximum number of collection IDs to return.
listCollections_maxResults :: Lens.Lens' ListCollections (Prelude.Maybe Prelude.Natural)
listCollections_maxResults = Lens.lens (\ListCollections' {maxResults} -> maxResults) (\s@ListCollections' {} a -> s {maxResults = a} :: ListCollections)

instance Pager.AWSPager ListCollections where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listCollectionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listCollectionsResponse_collectionIds
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listCollectionsResponse_faceModelVersions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listCollections_nextToken
          Lens..~ rs
          Lens.^? listCollectionsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListCollections where
  type Rs ListCollections = ListCollectionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCollectionsResponse'
            Prelude.<$> ( x Prelude..?> "FaceModelVersions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "CollectionIds"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCollections

instance Prelude.NFData ListCollections

instance Prelude.ToHeaders ListCollections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "RekognitionService.ListCollections" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListCollections where
  toJSON ListCollections' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults
          ]
      )

instance Prelude.ToPath ListCollections where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListCollections where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCollectionsResponse' smart constructor.
data ListCollectionsResponse = ListCollectionsResponse'
  { -- | Version numbers of the face detection models associated with the
    -- collections in the array @CollectionIds@. For example, the value of
    -- @FaceModelVersions[2]@ is the version number for the face detection
    -- model used by the collection in @CollectionId[2]@.
    faceModelVersions :: Prelude.Maybe [Prelude.Text],
    -- | If the result is truncated, the response provides a @NextToken@ that you
    -- can use in the subsequent request to fetch the next set of collection
    -- IDs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of collection IDs.
    collectionIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListCollectionsResponse
newListCollectionsResponse pHttpStatus_ =
  ListCollectionsResponse'
    { faceModelVersions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      collectionIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Version numbers of the face detection models associated with the
-- collections in the array @CollectionIds@. For example, the value of
-- @FaceModelVersions[2]@ is the version number for the face detection
-- model used by the collection in @CollectionId[2]@.
listCollectionsResponse_faceModelVersions :: Lens.Lens' ListCollectionsResponse (Prelude.Maybe [Prelude.Text])
listCollectionsResponse_faceModelVersions = Lens.lens (\ListCollectionsResponse' {faceModelVersions} -> faceModelVersions) (\s@ListCollectionsResponse' {} a -> s {faceModelVersions = a} :: ListCollectionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | If the result is truncated, the response provides a @NextToken@ that you
-- can use in the subsequent request to fetch the next set of collection
-- IDs.
listCollectionsResponse_nextToken :: Lens.Lens' ListCollectionsResponse (Prelude.Maybe Prelude.Text)
listCollectionsResponse_nextToken = Lens.lens (\ListCollectionsResponse' {nextToken} -> nextToken) (\s@ListCollectionsResponse' {} a -> s {nextToken = a} :: ListCollectionsResponse)

-- | An array of collection IDs.
listCollectionsResponse_collectionIds :: Lens.Lens' ListCollectionsResponse (Prelude.Maybe [Prelude.Text])
listCollectionsResponse_collectionIds = Lens.lens (\ListCollectionsResponse' {collectionIds} -> collectionIds) (\s@ListCollectionsResponse' {} a -> s {collectionIds = a} :: ListCollectionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listCollectionsResponse_httpStatus :: Lens.Lens' ListCollectionsResponse Prelude.Int
listCollectionsResponse_httpStatus = Lens.lens (\ListCollectionsResponse' {httpStatus} -> httpStatus) (\s@ListCollectionsResponse' {} a -> s {httpStatus = a} :: ListCollectionsResponse)

instance Prelude.NFData ListCollectionsResponse
