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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    listArtifacts_artifactType,
    listArtifacts_createdAfter,
    listArtifacts_createdBefore,
    listArtifacts_maxResults,
    listArtifacts_nextToken,
    listArtifacts_sortBy,
    listArtifacts_sortOrder,
    listArtifacts_sourceUri,

    -- * Destructuring the Response
    ListArtifactsResponse (..),
    newListArtifactsResponse,

    -- * Response Lenses
    listArtifactsResponse_artifactSummaries,
    listArtifactsResponse_nextToken,
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
  { -- | A filter that returns only artifacts of the specified type.
    artifactType :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only artifacts created on or after the specified
    -- time.
    createdAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only artifacts created on or before the specified
    -- time.
    createdBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of artifacts to return in the response. The default
    -- value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous call to @ListArtifacts@ didn\'t return the full set of
    -- artifacts, the call returns a token for getting the next set of
    -- artifacts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Prelude.Maybe SortArtifactsBy,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns only artifacts with the specified source URI.
    sourceUri :: Prelude.Maybe Prelude.Text
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
-- 'artifactType', 'listArtifacts_artifactType' - A filter that returns only artifacts of the specified type.
--
-- 'createdAfter', 'listArtifacts_createdAfter' - A filter that returns only artifacts created on or after the specified
-- time.
--
-- 'createdBefore', 'listArtifacts_createdBefore' - A filter that returns only artifacts created on or before the specified
-- time.
--
-- 'maxResults', 'listArtifacts_maxResults' - The maximum number of artifacts to return in the response. The default
-- value is 10.
--
-- 'nextToken', 'listArtifacts_nextToken' - If the previous call to @ListArtifacts@ didn\'t return the full set of
-- artifacts, the call returns a token for getting the next set of
-- artifacts.
--
-- 'sortBy', 'listArtifacts_sortBy' - The property used to sort results. The default value is @CreationTime@.
--
-- 'sortOrder', 'listArtifacts_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'sourceUri', 'listArtifacts_sourceUri' - A filter that returns only artifacts with the specified source URI.
newListArtifacts ::
  ListArtifacts
newListArtifacts =
  ListArtifacts'
    { artifactType = Prelude.Nothing,
      createdAfter = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      sourceUri = Prelude.Nothing
    }

-- | A filter that returns only artifacts of the specified type.
listArtifacts_artifactType :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.Text)
listArtifacts_artifactType = Lens.lens (\ListArtifacts' {artifactType} -> artifactType) (\s@ListArtifacts' {} a -> s {artifactType = a} :: ListArtifacts)

-- | A filter that returns only artifacts created on or after the specified
-- time.
listArtifacts_createdAfter :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.UTCTime)
listArtifacts_createdAfter = Lens.lens (\ListArtifacts' {createdAfter} -> createdAfter) (\s@ListArtifacts' {} a -> s {createdAfter = a} :: ListArtifacts) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only artifacts created on or before the specified
-- time.
listArtifacts_createdBefore :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.UTCTime)
listArtifacts_createdBefore = Lens.lens (\ListArtifacts' {createdBefore} -> createdBefore) (\s@ListArtifacts' {} a -> s {createdBefore = a} :: ListArtifacts) Prelude.. Lens.mapping Data._Time

-- | The maximum number of artifacts to return in the response. The default
-- value is 10.
listArtifacts_maxResults :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.Natural)
listArtifacts_maxResults = Lens.lens (\ListArtifacts' {maxResults} -> maxResults) (\s@ListArtifacts' {} a -> s {maxResults = a} :: ListArtifacts)

-- | If the previous call to @ListArtifacts@ didn\'t return the full set of
-- artifacts, the call returns a token for getting the next set of
-- artifacts.
listArtifacts_nextToken :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.Text)
listArtifacts_nextToken = Lens.lens (\ListArtifacts' {nextToken} -> nextToken) (\s@ListArtifacts' {} a -> s {nextToken = a} :: ListArtifacts)

-- | The property used to sort results. The default value is @CreationTime@.
listArtifacts_sortBy :: Lens.Lens' ListArtifacts (Prelude.Maybe SortArtifactsBy)
listArtifacts_sortBy = Lens.lens (\ListArtifacts' {sortBy} -> sortBy) (\s@ListArtifacts' {} a -> s {sortBy = a} :: ListArtifacts)

-- | The sort order. The default value is @Descending@.
listArtifacts_sortOrder :: Lens.Lens' ListArtifacts (Prelude.Maybe SortOrder)
listArtifacts_sortOrder = Lens.lens (\ListArtifacts' {sortOrder} -> sortOrder) (\s@ListArtifacts' {} a -> s {sortOrder = a} :: ListArtifacts)

-- | A filter that returns only artifacts with the specified source URI.
listArtifacts_sourceUri :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.Text)
listArtifacts_sourceUri = Lens.lens (\ListArtifacts' {sourceUri} -> sourceUri) (\s@ListArtifacts' {} a -> s {sourceUri = a} :: ListArtifacts)

instance Core.AWSPager ListArtifacts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listArtifactsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listArtifactsResponse_artifactSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listArtifacts_nextToken
          Lens..~ rs
          Lens.^? listArtifactsResponse_nextToken
          Prelude.. Lens._Just

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
            Prelude.<$> ( x
                            Data..?> "ArtifactSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListArtifacts where
  hashWithSalt _salt ListArtifacts' {..} =
    _salt
      `Prelude.hashWithSalt` artifactType
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` sourceUri

instance Prelude.NFData ListArtifacts where
  rnf ListArtifacts' {..} =
    Prelude.rnf artifactType
      `Prelude.seq` Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf sourceUri

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
          [ ("ArtifactType" Data..=) Prelude.<$> artifactType,
            ("CreatedAfter" Data..=) Prelude.<$> createdAfter,
            ("CreatedBefore" Data..=) Prelude.<$> createdBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("SourceUri" Data..=) Prelude.<$> sourceUri
          ]
      )

instance Data.ToPath ListArtifacts where
  toPath = Prelude.const "/"

instance Data.ToQuery ListArtifacts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListArtifactsResponse' smart constructor.
data ListArtifactsResponse = ListArtifactsResponse'
  { -- | A list of artifacts and their properties.
    artifactSummaries :: Prelude.Maybe [ArtifactSummary],
    -- | A token for getting the next set of artifacts, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'artifactSummaries', 'listArtifactsResponse_artifactSummaries' - A list of artifacts and their properties.
--
-- 'nextToken', 'listArtifactsResponse_nextToken' - A token for getting the next set of artifacts, if there are any.
--
-- 'httpStatus', 'listArtifactsResponse_httpStatus' - The response's http status code.
newListArtifactsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListArtifactsResponse
newListArtifactsResponse pHttpStatus_ =
  ListArtifactsResponse'
    { artifactSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of artifacts and their properties.
listArtifactsResponse_artifactSummaries :: Lens.Lens' ListArtifactsResponse (Prelude.Maybe [ArtifactSummary])
listArtifactsResponse_artifactSummaries = Lens.lens (\ListArtifactsResponse' {artifactSummaries} -> artifactSummaries) (\s@ListArtifactsResponse' {} a -> s {artifactSummaries = a} :: ListArtifactsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token for getting the next set of artifacts, if there are any.
listArtifactsResponse_nextToken :: Lens.Lens' ListArtifactsResponse (Prelude.Maybe Prelude.Text)
listArtifactsResponse_nextToken = Lens.lens (\ListArtifactsResponse' {nextToken} -> nextToken) (\s@ListArtifactsResponse' {} a -> s {nextToken = a} :: ListArtifactsResponse)

-- | The response's http status code.
listArtifactsResponse_httpStatus :: Lens.Lens' ListArtifactsResponse Prelude.Int
listArtifactsResponse_httpStatus = Lens.lens (\ListArtifactsResponse' {httpStatus} -> httpStatus) (\s@ListArtifactsResponse' {} a -> s {httpStatus = a} :: ListArtifactsResponse)

instance Prelude.NFData ListArtifactsResponse where
  rnf ListArtifactsResponse' {..} =
    Prelude.rnf artifactSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
