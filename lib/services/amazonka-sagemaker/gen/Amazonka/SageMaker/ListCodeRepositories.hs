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
-- Module      : Amazonka.SageMaker.ListCodeRepositories
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the Git repositories in your account.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListCodeRepositories
  ( -- * Creating a Request
    ListCodeRepositories (..),
    newListCodeRepositories,

    -- * Request Lenses
    listCodeRepositories_creationTimeAfter,
    listCodeRepositories_creationTimeBefore,
    listCodeRepositories_lastModifiedTimeAfter,
    listCodeRepositories_lastModifiedTimeBefore,
    listCodeRepositories_maxResults,
    listCodeRepositories_nameContains,
    listCodeRepositories_nextToken,
    listCodeRepositories_sortBy,
    listCodeRepositories_sortOrder,

    -- * Destructuring the Response
    ListCodeRepositoriesResponse (..),
    newListCodeRepositoriesResponse,

    -- * Response Lenses
    listCodeRepositoriesResponse_nextToken,
    listCodeRepositoriesResponse_httpStatus,
    listCodeRepositoriesResponse_codeRepositorySummaryList,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListCodeRepositories' smart constructor.
data ListCodeRepositories = ListCodeRepositories'
  { -- | A filter that returns only Git repositories that were created after the
    -- specified time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only Git repositories that were created before the
    -- specified time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only Git repositories that were last modified
    -- after the specified time.
    lastModifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only Git repositories that were last modified
    -- before the specified time.
    lastModifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of Git repositories to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A string in the Git repositories name. This filter returns only
    -- repositories whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the result of a @ListCodeRepositoriesOutput@ request was truncated,
    -- the response includes a @NextToken@. To get the next set of Git
    -- repositories, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The field to sort results by. The default is @Name@.
    sortBy :: Prelude.Maybe CodeRepositorySortBy,
    -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe CodeRepositorySortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCodeRepositories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listCodeRepositories_creationTimeAfter' - A filter that returns only Git repositories that were created after the
-- specified time.
--
-- 'creationTimeBefore', 'listCodeRepositories_creationTimeBefore' - A filter that returns only Git repositories that were created before the
-- specified time.
--
-- 'lastModifiedTimeAfter', 'listCodeRepositories_lastModifiedTimeAfter' - A filter that returns only Git repositories that were last modified
-- after the specified time.
--
-- 'lastModifiedTimeBefore', 'listCodeRepositories_lastModifiedTimeBefore' - A filter that returns only Git repositories that were last modified
-- before the specified time.
--
-- 'maxResults', 'listCodeRepositories_maxResults' - The maximum number of Git repositories to return in the response.
--
-- 'nameContains', 'listCodeRepositories_nameContains' - A string in the Git repositories name. This filter returns only
-- repositories whose name contains the specified string.
--
-- 'nextToken', 'listCodeRepositories_nextToken' - If the result of a @ListCodeRepositoriesOutput@ request was truncated,
-- the response includes a @NextToken@. To get the next set of Git
-- repositories, use the token in the next request.
--
-- 'sortBy', 'listCodeRepositories_sortBy' - The field to sort results by. The default is @Name@.
--
-- 'sortOrder', 'listCodeRepositories_sortOrder' - The sort order for results. The default is @Ascending@.
newListCodeRepositories ::
  ListCodeRepositories
newListCodeRepositories =
  ListCodeRepositories'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | A filter that returns only Git repositories that were created after the
-- specified time.
listCodeRepositories_creationTimeAfter :: Lens.Lens' ListCodeRepositories (Prelude.Maybe Prelude.UTCTime)
listCodeRepositories_creationTimeAfter = Lens.lens (\ListCodeRepositories' {creationTimeAfter} -> creationTimeAfter) (\s@ListCodeRepositories' {} a -> s {creationTimeAfter = a} :: ListCodeRepositories) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only Git repositories that were created before the
-- specified time.
listCodeRepositories_creationTimeBefore :: Lens.Lens' ListCodeRepositories (Prelude.Maybe Prelude.UTCTime)
listCodeRepositories_creationTimeBefore = Lens.lens (\ListCodeRepositories' {creationTimeBefore} -> creationTimeBefore) (\s@ListCodeRepositories' {} a -> s {creationTimeBefore = a} :: ListCodeRepositories) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only Git repositories that were last modified
-- after the specified time.
listCodeRepositories_lastModifiedTimeAfter :: Lens.Lens' ListCodeRepositories (Prelude.Maybe Prelude.UTCTime)
listCodeRepositories_lastModifiedTimeAfter = Lens.lens (\ListCodeRepositories' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListCodeRepositories' {} a -> s {lastModifiedTimeAfter = a} :: ListCodeRepositories) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only Git repositories that were last modified
-- before the specified time.
listCodeRepositories_lastModifiedTimeBefore :: Lens.Lens' ListCodeRepositories (Prelude.Maybe Prelude.UTCTime)
listCodeRepositories_lastModifiedTimeBefore = Lens.lens (\ListCodeRepositories' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListCodeRepositories' {} a -> s {lastModifiedTimeBefore = a} :: ListCodeRepositories) Prelude.. Lens.mapping Data._Time

-- | The maximum number of Git repositories to return in the response.
listCodeRepositories_maxResults :: Lens.Lens' ListCodeRepositories (Prelude.Maybe Prelude.Natural)
listCodeRepositories_maxResults = Lens.lens (\ListCodeRepositories' {maxResults} -> maxResults) (\s@ListCodeRepositories' {} a -> s {maxResults = a} :: ListCodeRepositories)

-- | A string in the Git repositories name. This filter returns only
-- repositories whose name contains the specified string.
listCodeRepositories_nameContains :: Lens.Lens' ListCodeRepositories (Prelude.Maybe Prelude.Text)
listCodeRepositories_nameContains = Lens.lens (\ListCodeRepositories' {nameContains} -> nameContains) (\s@ListCodeRepositories' {} a -> s {nameContains = a} :: ListCodeRepositories)

-- | If the result of a @ListCodeRepositoriesOutput@ request was truncated,
-- the response includes a @NextToken@. To get the next set of Git
-- repositories, use the token in the next request.
listCodeRepositories_nextToken :: Lens.Lens' ListCodeRepositories (Prelude.Maybe Prelude.Text)
listCodeRepositories_nextToken = Lens.lens (\ListCodeRepositories' {nextToken} -> nextToken) (\s@ListCodeRepositories' {} a -> s {nextToken = a} :: ListCodeRepositories)

-- | The field to sort results by. The default is @Name@.
listCodeRepositories_sortBy :: Lens.Lens' ListCodeRepositories (Prelude.Maybe CodeRepositorySortBy)
listCodeRepositories_sortBy = Lens.lens (\ListCodeRepositories' {sortBy} -> sortBy) (\s@ListCodeRepositories' {} a -> s {sortBy = a} :: ListCodeRepositories)

-- | The sort order for results. The default is @Ascending@.
listCodeRepositories_sortOrder :: Lens.Lens' ListCodeRepositories (Prelude.Maybe CodeRepositorySortOrder)
listCodeRepositories_sortOrder = Lens.lens (\ListCodeRepositories' {sortOrder} -> sortOrder) (\s@ListCodeRepositories' {} a -> s {sortOrder = a} :: ListCodeRepositories)

instance Core.AWSPager ListCodeRepositories where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCodeRepositoriesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listCodeRepositoriesResponse_codeRepositorySummaryList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCodeRepositories_nextToken
          Lens..~ rs
          Lens.^? listCodeRepositoriesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCodeRepositories where
  type
    AWSResponse ListCodeRepositories =
      ListCodeRepositoriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCodeRepositoriesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "CodeRepositorySummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListCodeRepositories where
  hashWithSalt _salt ListCodeRepositories' {..} =
    _salt `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListCodeRepositories where
  rnf ListCodeRepositories' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToHeaders ListCodeRepositories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListCodeRepositories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCodeRepositories where
  toJSON ListCodeRepositories' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Data..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("LastModifiedTimeBefore" Data..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListCodeRepositories where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCodeRepositories where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCodeRepositoriesResponse' smart constructor.
data ListCodeRepositoriesResponse = ListCodeRepositoriesResponse'
  { -- | If the result of a @ListCodeRepositoriesOutput@ request was truncated,
    -- the response includes a @NextToken@. To get the next set of Git
    -- repositories, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Gets a list of summaries of the Git repositories. Each summary specifies
    -- the following values for the repository:
    --
    -- -   Name
    --
    -- -   Amazon Resource Name (ARN)
    --
    -- -   Creation time
    --
    -- -   Last modified time
    --
    -- -   Configuration information, including the URL location of the
    --     repository and the ARN of the Amazon Web Services Secrets Manager
    --     secret that contains the credentials used to access the repository.
    codeRepositorySummaryList :: [CodeRepositorySummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCodeRepositoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCodeRepositoriesResponse_nextToken' - If the result of a @ListCodeRepositoriesOutput@ request was truncated,
-- the response includes a @NextToken@. To get the next set of Git
-- repositories, use the token in the next request.
--
-- 'httpStatus', 'listCodeRepositoriesResponse_httpStatus' - The response's http status code.
--
-- 'codeRepositorySummaryList', 'listCodeRepositoriesResponse_codeRepositorySummaryList' - Gets a list of summaries of the Git repositories. Each summary specifies
-- the following values for the repository:
--
-- -   Name
--
-- -   Amazon Resource Name (ARN)
--
-- -   Creation time
--
-- -   Last modified time
--
-- -   Configuration information, including the URL location of the
--     repository and the ARN of the Amazon Web Services Secrets Manager
--     secret that contains the credentials used to access the repository.
newListCodeRepositoriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCodeRepositoriesResponse
newListCodeRepositoriesResponse pHttpStatus_ =
  ListCodeRepositoriesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      codeRepositorySummaryList = Prelude.mempty
    }

-- | If the result of a @ListCodeRepositoriesOutput@ request was truncated,
-- the response includes a @NextToken@. To get the next set of Git
-- repositories, use the token in the next request.
listCodeRepositoriesResponse_nextToken :: Lens.Lens' ListCodeRepositoriesResponse (Prelude.Maybe Prelude.Text)
listCodeRepositoriesResponse_nextToken = Lens.lens (\ListCodeRepositoriesResponse' {nextToken} -> nextToken) (\s@ListCodeRepositoriesResponse' {} a -> s {nextToken = a} :: ListCodeRepositoriesResponse)

-- | The response's http status code.
listCodeRepositoriesResponse_httpStatus :: Lens.Lens' ListCodeRepositoriesResponse Prelude.Int
listCodeRepositoriesResponse_httpStatus = Lens.lens (\ListCodeRepositoriesResponse' {httpStatus} -> httpStatus) (\s@ListCodeRepositoriesResponse' {} a -> s {httpStatus = a} :: ListCodeRepositoriesResponse)

-- | Gets a list of summaries of the Git repositories. Each summary specifies
-- the following values for the repository:
--
-- -   Name
--
-- -   Amazon Resource Name (ARN)
--
-- -   Creation time
--
-- -   Last modified time
--
-- -   Configuration information, including the URL location of the
--     repository and the ARN of the Amazon Web Services Secrets Manager
--     secret that contains the credentials used to access the repository.
listCodeRepositoriesResponse_codeRepositorySummaryList :: Lens.Lens' ListCodeRepositoriesResponse [CodeRepositorySummary]
listCodeRepositoriesResponse_codeRepositorySummaryList = Lens.lens (\ListCodeRepositoriesResponse' {codeRepositorySummaryList} -> codeRepositorySummaryList) (\s@ListCodeRepositoriesResponse' {} a -> s {codeRepositorySummaryList = a} :: ListCodeRepositoriesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListCodeRepositoriesResponse where
  rnf ListCodeRepositoriesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf codeRepositorySummaryList
