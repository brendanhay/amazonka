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
-- Module      : Amazonka.SageMaker.ListArtifacts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the artifacts in your account and their properties.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListArtifacts
  ( -- * Creating a Request
    ListArtifacts (..),
    newListArtifacts,

    -- * Request Lenses
    listArtifacts_sortOrder,
    listArtifacts_nextToken,
    listArtifacts_sourceUri,
    listArtifacts_artifactType,
    listArtifacts_createdBefore,
    listArtifacts_sortBy,
    listArtifacts_maxResults,
    listArtifacts_createdAfter,

    -- * Destructuring the Response
    ListArtifactsResponse (..),
    newListArtifactsResponse,

    -- * Response Lenses
    listArtifactsResponse_nextToken,
    listArtifactsResponse_artifactSummaries,
    listArtifactsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListArtifacts' smart constructor.
data ListArtifacts = ListArtifacts'
  { -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | If the previous call to @ListArtifacts@ didn\'t return the full set of
    -- artifacts, the call returns a token for getting the next set of
    -- artifacts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only artifacts with the specified source URI.
    sourceUri :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only artifacts of the specified type.
    artifactType :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only artifacts created on or before the specified
    -- time.
    createdBefore :: Prelude.Maybe Data.POSIX,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Prelude.Maybe SortArtifactsBy,
    -- | The maximum number of artifacts to return in the response. The default
    -- value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only artifacts created on or after the specified
    -- time.
    createdAfter :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listArtifacts_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'nextToken', 'listArtifacts_nextToken' - If the previous call to @ListArtifacts@ didn\'t return the full set of
-- artifacts, the call returns a token for getting the next set of
-- artifacts.
--
-- 'sourceUri', 'listArtifacts_sourceUri' - A filter that returns only artifacts with the specified source URI.
--
-- 'artifactType', 'listArtifacts_artifactType' - A filter that returns only artifacts of the specified type.
--
-- 'createdBefore', 'listArtifacts_createdBefore' - A filter that returns only artifacts created on or before the specified
-- time.
--
-- 'sortBy', 'listArtifacts_sortBy' - The property used to sort results. The default value is @CreationTime@.
--
-- 'maxResults', 'listArtifacts_maxResults' - The maximum number of artifacts to return in the response. The default
-- value is 10.
--
-- 'createdAfter', 'listArtifacts_createdAfter' - A filter that returns only artifacts created on or after the specified
-- time.
newListArtifacts ::
  ListArtifacts
newListArtifacts =
  ListArtifacts'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sourceUri = Prelude.Nothing,
      artifactType = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      createdAfter = Prelude.Nothing
    }

-- | The sort order. The default value is @Descending@.
listArtifacts_sortOrder :: Lens.Lens' ListArtifacts (Prelude.Maybe SortOrder)
listArtifacts_sortOrder = Lens.lens (\ListArtifacts' {sortOrder} -> sortOrder) (\s@ListArtifacts' {} a -> s {sortOrder = a} :: ListArtifacts)

-- | If the previous call to @ListArtifacts@ didn\'t return the full set of
-- artifacts, the call returns a token for getting the next set of
-- artifacts.
listArtifacts_nextToken :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.Text)
listArtifacts_nextToken = Lens.lens (\ListArtifacts' {nextToken} -> nextToken) (\s@ListArtifacts' {} a -> s {nextToken = a} :: ListArtifacts)

-- | A filter that returns only artifacts with the specified source URI.
listArtifacts_sourceUri :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.Text)
listArtifacts_sourceUri = Lens.lens (\ListArtifacts' {sourceUri} -> sourceUri) (\s@ListArtifacts' {} a -> s {sourceUri = a} :: ListArtifacts)

-- | A filter that returns only artifacts of the specified type.
listArtifacts_artifactType :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.Text)
listArtifacts_artifactType = Lens.lens (\ListArtifacts' {artifactType} -> artifactType) (\s@ListArtifacts' {} a -> s {artifactType = a} :: ListArtifacts)

-- | A filter that returns only artifacts created on or before the specified
-- time.
listArtifacts_createdBefore :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.UTCTime)
listArtifacts_createdBefore = Lens.lens (\ListArtifacts' {createdBefore} -> createdBefore) (\s@ListArtifacts' {} a -> s {createdBefore = a} :: ListArtifacts) Prelude.. Lens.mapping Data._Time

-- | The property used to sort results. The default value is @CreationTime@.
listArtifacts_sortBy :: Lens.Lens' ListArtifacts (Prelude.Maybe SortArtifactsBy)
listArtifacts_sortBy = Lens.lens (\ListArtifacts' {sortBy} -> sortBy) (\s@ListArtifacts' {} a -> s {sortBy = a} :: ListArtifacts)

-- | The maximum number of artifacts to return in the response. The default
-- value is 10.
listArtifacts_maxResults :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.Natural)
listArtifacts_maxResults = Lens.lens (\ListArtifacts' {maxResults} -> maxResults) (\s@ListArtifacts' {} a -> s {maxResults = a} :: ListArtifacts)

-- | A filter that returns only artifacts created on or after the specified
-- time.
listArtifacts_createdAfter :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.UTCTime)
listArtifacts_createdAfter = Lens.lens (\ListArtifacts' {createdAfter} -> createdAfter) (\s@ListArtifacts' {} a -> s {createdAfter = a} :: ListArtifacts) Prelude.. Lens.mapping Data._Time

instance Core.AWSPager ListArtifacts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listArtifactsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listArtifactsResponse_artifactSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listArtifacts_nextToken
          Lens..~ rs
          Lens.^? listArtifactsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListArtifacts where
  type
    AWSResponse ListArtifacts =
      ListArtifactsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListArtifactsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ArtifactSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListArtifacts where
  hashWithSalt _salt ListArtifacts' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sourceUri
      `Prelude.hashWithSalt` artifactType
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` createdAfter

instance Prelude.NFData ListArtifacts where
  rnf ListArtifacts' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sourceUri
      `Prelude.seq` Prelude.rnf artifactType
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf createdAfter

instance Data.ToHeaders ListArtifacts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListArtifacts" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListArtifacts where
  toJSON ListArtifacts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SourceUri" Data..=) Prelude.<$> sourceUri,
            ("ArtifactType" Data..=) Prelude.<$> artifactType,
            ("CreatedBefore" Data..=) Prelude.<$> createdBefore,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("CreatedAfter" Data..=) Prelude.<$> createdAfter
          ]
      )

instance Data.ToPath ListArtifacts where
  toPath = Prelude.const "/"

instance Data.ToQuery ListArtifacts where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
listArtifactsResponse_artifactSummaries = Lens.lens (\ListArtifactsResponse' {artifactSummaries} -> artifactSummaries) (\s@ListArtifactsResponse' {} a -> s {artifactSummaries = a} :: ListArtifactsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listArtifactsResponse_httpStatus :: Lens.Lens' ListArtifactsResponse Prelude.Int
listArtifactsResponse_httpStatus = Lens.lens (\ListArtifactsResponse' {httpStatus} -> httpStatus) (\s@ListArtifactsResponse' {} a -> s {httpStatus = a} :: ListArtifactsResponse)

instance Prelude.NFData ListArtifactsResponse where
  rnf ListArtifactsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf artifactSummaries
      `Prelude.seq` Prelude.rnf httpStatus
