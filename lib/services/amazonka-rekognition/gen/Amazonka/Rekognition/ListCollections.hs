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
-- Module      : Amazonka.Rekognition.ListCollections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of collection IDs in your account. If the result is
-- truncated, the response also provides a @NextToken@ that you can use in
-- the subsequent request to fetch the next set of collection IDs.
--
-- For an example, see Listing collections in the Amazon Rekognition
-- Developer Guide.
--
-- This operation requires permissions to perform the
-- @rekognition:ListCollections@ action.
--
-- This operation returns paginated results.
module Amazonka.Rekognition.ListCollections
  ( -- * Creating a Request
    ListCollections (..),
    newListCollections,

    -- * Request Lenses
    listCollections_maxResults,
    listCollections_nextToken,

    -- * Destructuring the Response
    ListCollectionsResponse (..),
    newListCollectionsResponse,

    -- * Response Lenses
    listCollectionsResponse_collectionIds,
    listCollectionsResponse_faceModelVersions,
    listCollectionsResponse_nextToken,
    listCollectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCollections' smart constructor.
data ListCollections = ListCollections'
  { -- | Maximum number of collection IDs to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Pagination token from the previous response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCollections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCollections_maxResults' - Maximum number of collection IDs to return.
--
-- 'nextToken', 'listCollections_nextToken' - Pagination token from the previous response.
newListCollections ::
  ListCollections
newListCollections =
  ListCollections'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Maximum number of collection IDs to return.
listCollections_maxResults :: Lens.Lens' ListCollections (Prelude.Maybe Prelude.Natural)
listCollections_maxResults = Lens.lens (\ListCollections' {maxResults} -> maxResults) (\s@ListCollections' {} a -> s {maxResults = a} :: ListCollections)

-- | Pagination token from the previous response.
listCollections_nextToken :: Lens.Lens' ListCollections (Prelude.Maybe Prelude.Text)
listCollections_nextToken = Lens.lens (\ListCollections' {nextToken} -> nextToken) (\s@ListCollections' {} a -> s {nextToken = a} :: ListCollections)

instance Core.AWSPager ListCollections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCollectionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCollectionsResponse_collectionIds
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCollectionsResponse_faceModelVersions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCollections_nextToken
          Lens..~ rs
          Lens.^? listCollectionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListCollections where
  type
    AWSResponse ListCollections =
      ListCollectionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCollectionsResponse'
            Prelude.<$> (x Data..?> "CollectionIds" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "FaceModelVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCollections where
  hashWithSalt _salt ListCollections' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListCollections where
  rnf ListCollections' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListCollections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.ListCollections" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCollections where
  toJSON ListCollections' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListCollections where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCollections where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCollectionsResponse' smart constructor.
data ListCollectionsResponse = ListCollectionsResponse'
  { -- | An array of collection IDs.
    collectionIds :: Prelude.Maybe [Prelude.Text],
    -- | Version numbers of the face detection models associated with the
    -- collections in the array @CollectionIds@. For example, the value of
    -- @FaceModelVersions[2]@ is the version number for the face detection
    -- model used by the collection in @CollectionId[2]@.
    faceModelVersions :: Prelude.Maybe [Prelude.Text],
    -- | If the result is truncated, the response provides a @NextToken@ that you
    -- can use in the subsequent request to fetch the next set of collection
    -- IDs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCollectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionIds', 'listCollectionsResponse_collectionIds' - An array of collection IDs.
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
-- 'httpStatus', 'listCollectionsResponse_httpStatus' - The response's http status code.
newListCollectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCollectionsResponse
newListCollectionsResponse pHttpStatus_ =
  ListCollectionsResponse'
    { collectionIds =
        Prelude.Nothing,
      faceModelVersions = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of collection IDs.
listCollectionsResponse_collectionIds :: Lens.Lens' ListCollectionsResponse (Prelude.Maybe [Prelude.Text])
listCollectionsResponse_collectionIds = Lens.lens (\ListCollectionsResponse' {collectionIds} -> collectionIds) (\s@ListCollectionsResponse' {} a -> s {collectionIds = a} :: ListCollectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Version numbers of the face detection models associated with the
-- collections in the array @CollectionIds@. For example, the value of
-- @FaceModelVersions[2]@ is the version number for the face detection
-- model used by the collection in @CollectionId[2]@.
listCollectionsResponse_faceModelVersions :: Lens.Lens' ListCollectionsResponse (Prelude.Maybe [Prelude.Text])
listCollectionsResponse_faceModelVersions = Lens.lens (\ListCollectionsResponse' {faceModelVersions} -> faceModelVersions) (\s@ListCollectionsResponse' {} a -> s {faceModelVersions = a} :: ListCollectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the result is truncated, the response provides a @NextToken@ that you
-- can use in the subsequent request to fetch the next set of collection
-- IDs.
listCollectionsResponse_nextToken :: Lens.Lens' ListCollectionsResponse (Prelude.Maybe Prelude.Text)
listCollectionsResponse_nextToken = Lens.lens (\ListCollectionsResponse' {nextToken} -> nextToken) (\s@ListCollectionsResponse' {} a -> s {nextToken = a} :: ListCollectionsResponse)

-- | The response's http status code.
listCollectionsResponse_httpStatus :: Lens.Lens' ListCollectionsResponse Prelude.Int
listCollectionsResponse_httpStatus = Lens.lens (\ListCollectionsResponse' {httpStatus} -> httpStatus) (\s@ListCollectionsResponse' {} a -> s {httpStatus = a} :: ListCollectionsResponse)

instance Prelude.NFData ListCollectionsResponse where
  rnf ListCollectionsResponse' {..} =
    Prelude.rnf collectionIds
      `Prelude.seq` Prelude.rnf faceModelVersions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
