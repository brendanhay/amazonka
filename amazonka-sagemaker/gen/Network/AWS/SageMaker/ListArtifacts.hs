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
-- Module      : Network.AWS.SageMaker.ListArtifacts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the artifacts in your account and their properties.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListArtifacts
  ( -- * Creating a Request
    ListArtifacts (..),
    newListArtifacts,

    -- * Request Lenses
    listArtifacts_createdAfter,
    listArtifacts_sortOrder,
    listArtifacts_nextToken,
    listArtifacts_createdBefore,
    listArtifacts_artifactType,
    listArtifacts_maxResults,
    listArtifacts_sourceUri,
    listArtifacts_sortBy,

    -- * Destructuring the Response
    ListArtifactsResponse (..),
    newListArtifactsResponse,

    -- * Response Lenses
    listArtifactsResponse_nextToken,
    listArtifactsResponse_artifactSummaries,
    listArtifactsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListArtifacts' smart constructor.
data ListArtifacts = ListArtifacts'
  { -- | A filter that returns only artifacts created on or after the specified
    -- time.
    createdAfter :: Prelude.Maybe Prelude.POSIX,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | If the previous call to @ListArtifacts@ didn\'t return the full set of
    -- artifacts, the call returns a token for getting the next set of
    -- artifacts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only artifacts created on or before the specified
    -- time.
    createdBefore :: Prelude.Maybe Prelude.POSIX,
    -- | A filter that returns only artifacts of the specified type.
    artifactType :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of artifacts to return in the response. The default
    -- value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only artifacts with the specified source URI.
    sourceUri :: Prelude.Maybe Prelude.Text,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Prelude.Maybe SortArtifactsBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'listArtifacts_createdAfter' - A filter that returns only artifacts created on or after the specified
-- time.
--
-- 'sortOrder', 'listArtifacts_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'nextToken', 'listArtifacts_nextToken' - If the previous call to @ListArtifacts@ didn\'t return the full set of
-- artifacts, the call returns a token for getting the next set of
-- artifacts.
--
-- 'createdBefore', 'listArtifacts_createdBefore' - A filter that returns only artifacts created on or before the specified
-- time.
--
-- 'artifactType', 'listArtifacts_artifactType' - A filter that returns only artifacts of the specified type.
--
-- 'maxResults', 'listArtifacts_maxResults' - The maximum number of artifacts to return in the response. The default
-- value is 10.
--
-- 'sourceUri', 'listArtifacts_sourceUri' - A filter that returns only artifacts with the specified source URI.
--
-- 'sortBy', 'listArtifacts_sortBy' - The property used to sort results. The default value is @CreationTime@.
newListArtifacts ::
  ListArtifacts
newListArtifacts =
  ListArtifacts'
    { createdAfter = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      artifactType = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sourceUri = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | A filter that returns only artifacts created on or after the specified
-- time.
listArtifacts_createdAfter :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.UTCTime)
listArtifacts_createdAfter = Lens.lens (\ListArtifacts' {createdAfter} -> createdAfter) (\s@ListArtifacts' {} a -> s {createdAfter = a} :: ListArtifacts) Prelude.. Lens.mapping Prelude._Time

-- | The sort order. The default value is @Descending@.
listArtifacts_sortOrder :: Lens.Lens' ListArtifacts (Prelude.Maybe SortOrder)
listArtifacts_sortOrder = Lens.lens (\ListArtifacts' {sortOrder} -> sortOrder) (\s@ListArtifacts' {} a -> s {sortOrder = a} :: ListArtifacts)

-- | If the previous call to @ListArtifacts@ didn\'t return the full set of
-- artifacts, the call returns a token for getting the next set of
-- artifacts.
listArtifacts_nextToken :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.Text)
listArtifacts_nextToken = Lens.lens (\ListArtifacts' {nextToken} -> nextToken) (\s@ListArtifacts' {} a -> s {nextToken = a} :: ListArtifacts)

-- | A filter that returns only artifacts created on or before the specified
-- time.
listArtifacts_createdBefore :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.UTCTime)
listArtifacts_createdBefore = Lens.lens (\ListArtifacts' {createdBefore} -> createdBefore) (\s@ListArtifacts' {} a -> s {createdBefore = a} :: ListArtifacts) Prelude.. Lens.mapping Prelude._Time

-- | A filter that returns only artifacts of the specified type.
listArtifacts_artifactType :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.Text)
listArtifacts_artifactType = Lens.lens (\ListArtifacts' {artifactType} -> artifactType) (\s@ListArtifacts' {} a -> s {artifactType = a} :: ListArtifacts)

-- | The maximum number of artifacts to return in the response. The default
-- value is 10.
listArtifacts_maxResults :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.Natural)
listArtifacts_maxResults = Lens.lens (\ListArtifacts' {maxResults} -> maxResults) (\s@ListArtifacts' {} a -> s {maxResults = a} :: ListArtifacts)

-- | A filter that returns only artifacts with the specified source URI.
listArtifacts_sourceUri :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.Text)
listArtifacts_sourceUri = Lens.lens (\ListArtifacts' {sourceUri} -> sourceUri) (\s@ListArtifacts' {} a -> s {sourceUri = a} :: ListArtifacts)

-- | The property used to sort results. The default value is @CreationTime@.
listArtifacts_sortBy :: Lens.Lens' ListArtifacts (Prelude.Maybe SortArtifactsBy)
listArtifacts_sortBy = Lens.lens (\ListArtifacts' {sortBy} -> sortBy) (\s@ListArtifacts' {} a -> s {sortBy = a} :: ListArtifacts)

instance Pager.AWSPager ListArtifacts where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listArtifactsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listArtifactsResponse_artifactSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listArtifacts_nextToken
          Lens..~ rs
          Lens.^? listArtifactsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListArtifacts where
  type Rs ListArtifacts = ListArtifactsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListArtifactsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "ArtifactSummaries"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListArtifacts

instance Prelude.NFData ListArtifacts

instance Prelude.ToHeaders ListArtifacts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.ListArtifacts" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListArtifacts where
  toJSON ListArtifacts' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CreatedAfter" Prelude..=)
              Prelude.<$> createdAfter,
            ("SortOrder" Prelude..=) Prelude.<$> sortOrder,
            ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("CreatedBefore" Prelude..=)
              Prelude.<$> createdBefore,
            ("ArtifactType" Prelude..=) Prelude.<$> artifactType,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("SourceUri" Prelude..=) Prelude.<$> sourceUri,
            ("SortBy" Prelude..=) Prelude.<$> sortBy
          ]
      )

instance Prelude.ToPath ListArtifacts where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListArtifacts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListArtifactsResponse' smart constructor.
data ListArtifactsResponse = ListArtifactsResponse'
  { -- | A token for getting the next set of artifacts, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of artifacts and their properties.
    artifactSummaries :: Prelude.Maybe [ArtifactSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListArtifactsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listArtifactsResponse_nextToken' - A token for getting the next set of artifacts, if there are any.
--
-- 'artifactSummaries', 'listArtifactsResponse_artifactSummaries' - A list of artifacts and their properties.
--
-- 'httpStatus', 'listArtifactsResponse_httpStatus' - The response's http status code.
newListArtifactsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListArtifactsResponse
newListArtifactsResponse pHttpStatus_ =
  ListArtifactsResponse'
    { nextToken = Prelude.Nothing,
      artifactSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of artifacts, if there are any.
listArtifactsResponse_nextToken :: Lens.Lens' ListArtifactsResponse (Prelude.Maybe Prelude.Text)
listArtifactsResponse_nextToken = Lens.lens (\ListArtifactsResponse' {nextToken} -> nextToken) (\s@ListArtifactsResponse' {} a -> s {nextToken = a} :: ListArtifactsResponse)

-- | A list of artifacts and their properties.
listArtifactsResponse_artifactSummaries :: Lens.Lens' ListArtifactsResponse (Prelude.Maybe [ArtifactSummary])
listArtifactsResponse_artifactSummaries = Lens.lens (\ListArtifactsResponse' {artifactSummaries} -> artifactSummaries) (\s@ListArtifactsResponse' {} a -> s {artifactSummaries = a} :: ListArtifactsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listArtifactsResponse_httpStatus :: Lens.Lens' ListArtifactsResponse Prelude.Int
listArtifactsResponse_httpStatus = Lens.lens (\ListArtifactsResponse' {httpStatus} -> httpStatus) (\s@ListArtifactsResponse' {} a -> s {httpStatus = a} :: ListArtifactsResponse)

instance Prelude.NFData ListArtifactsResponse
