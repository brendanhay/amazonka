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
-- Module      : Network.AWS.SageMaker.ListCodeRepositories
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the Git repositories in your account.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListCodeRepositories
  ( -- * Creating a Request
    ListCodeRepositories (..),
    newListCodeRepositories,

    -- * Request Lenses
    listCodeRepositories_lastModifiedTimeBefore,
    listCodeRepositories_sortOrder,
    listCodeRepositories_nextToken,
    listCodeRepositories_nameContains,
    listCodeRepositories_maxResults,
    listCodeRepositories_creationTimeBefore,
    listCodeRepositories_lastModifiedTimeAfter,
    listCodeRepositories_sortBy,
    listCodeRepositories_creationTimeAfter,

    -- * Destructuring the Response
    ListCodeRepositoriesResponse (..),
    newListCodeRepositoriesResponse,

    -- * Response Lenses
    listCodeRepositoriesResponse_nextToken,
    listCodeRepositoriesResponse_httpStatus,
    listCodeRepositoriesResponse_codeRepositorySummaryList,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListCodeRepositories' smart constructor.
data ListCodeRepositories = ListCodeRepositories'
  { -- | A filter that returns only Git repositories that were last modified
    -- before the specified time.
    lastModifiedTimeBefore :: Core.Maybe Core.POSIX,
    -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Core.Maybe CodeRepositorySortOrder,
    -- | If the result of a @ListCodeRepositoriesOutput@ request was truncated,
    -- the response includes a @NextToken@. To get the next set of Git
    -- repositories, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | A string in the Git repositories name. This filter returns only
    -- repositories whose name contains the specified string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of Git repositories to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only Git repositories that were created before the
    -- specified time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | A filter that returns only Git repositories that were last modified
    -- after the specified time.
    lastModifiedTimeAfter :: Core.Maybe Core.POSIX,
    -- | The field to sort results by. The default is @Name@.
    sortBy :: Core.Maybe CodeRepositorySortBy,
    -- | A filter that returns only Git repositories that were created after the
    -- specified time.
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCodeRepositories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTimeBefore', 'listCodeRepositories_lastModifiedTimeBefore' - A filter that returns only Git repositories that were last modified
-- before the specified time.
--
-- 'sortOrder', 'listCodeRepositories_sortOrder' - The sort order for results. The default is @Ascending@.
--
-- 'nextToken', 'listCodeRepositories_nextToken' - If the result of a @ListCodeRepositoriesOutput@ request was truncated,
-- the response includes a @NextToken@. To get the next set of Git
-- repositories, use the token in the next request.
--
-- 'nameContains', 'listCodeRepositories_nameContains' - A string in the Git repositories name. This filter returns only
-- repositories whose name contains the specified string.
--
-- 'maxResults', 'listCodeRepositories_maxResults' - The maximum number of Git repositories to return in the response.
--
-- 'creationTimeBefore', 'listCodeRepositories_creationTimeBefore' - A filter that returns only Git repositories that were created before the
-- specified time.
--
-- 'lastModifiedTimeAfter', 'listCodeRepositories_lastModifiedTimeAfter' - A filter that returns only Git repositories that were last modified
-- after the specified time.
--
-- 'sortBy', 'listCodeRepositories_sortBy' - The field to sort results by. The default is @Name@.
--
-- 'creationTimeAfter', 'listCodeRepositories_creationTimeAfter' - A filter that returns only Git repositories that were created after the
-- specified time.
newListCodeRepositories ::
  ListCodeRepositories
newListCodeRepositories =
  ListCodeRepositories'
    { lastModifiedTimeBefore =
        Core.Nothing,
      sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      lastModifiedTimeAfter = Core.Nothing,
      sortBy = Core.Nothing,
      creationTimeAfter = Core.Nothing
    }

-- | A filter that returns only Git repositories that were last modified
-- before the specified time.
listCodeRepositories_lastModifiedTimeBefore :: Lens.Lens' ListCodeRepositories (Core.Maybe Core.UTCTime)
listCodeRepositories_lastModifiedTimeBefore = Lens.lens (\ListCodeRepositories' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListCodeRepositories' {} a -> s {lastModifiedTimeBefore = a} :: ListCodeRepositories) Core.. Lens.mapping Core._Time

-- | The sort order for results. The default is @Ascending@.
listCodeRepositories_sortOrder :: Lens.Lens' ListCodeRepositories (Core.Maybe CodeRepositorySortOrder)
listCodeRepositories_sortOrder = Lens.lens (\ListCodeRepositories' {sortOrder} -> sortOrder) (\s@ListCodeRepositories' {} a -> s {sortOrder = a} :: ListCodeRepositories)

-- | If the result of a @ListCodeRepositoriesOutput@ request was truncated,
-- the response includes a @NextToken@. To get the next set of Git
-- repositories, use the token in the next request.
listCodeRepositories_nextToken :: Lens.Lens' ListCodeRepositories (Core.Maybe Core.Text)
listCodeRepositories_nextToken = Lens.lens (\ListCodeRepositories' {nextToken} -> nextToken) (\s@ListCodeRepositories' {} a -> s {nextToken = a} :: ListCodeRepositories)

-- | A string in the Git repositories name. This filter returns only
-- repositories whose name contains the specified string.
listCodeRepositories_nameContains :: Lens.Lens' ListCodeRepositories (Core.Maybe Core.Text)
listCodeRepositories_nameContains = Lens.lens (\ListCodeRepositories' {nameContains} -> nameContains) (\s@ListCodeRepositories' {} a -> s {nameContains = a} :: ListCodeRepositories)

-- | The maximum number of Git repositories to return in the response.
listCodeRepositories_maxResults :: Lens.Lens' ListCodeRepositories (Core.Maybe Core.Natural)
listCodeRepositories_maxResults = Lens.lens (\ListCodeRepositories' {maxResults} -> maxResults) (\s@ListCodeRepositories' {} a -> s {maxResults = a} :: ListCodeRepositories)

-- | A filter that returns only Git repositories that were created before the
-- specified time.
listCodeRepositories_creationTimeBefore :: Lens.Lens' ListCodeRepositories (Core.Maybe Core.UTCTime)
listCodeRepositories_creationTimeBefore = Lens.lens (\ListCodeRepositories' {creationTimeBefore} -> creationTimeBefore) (\s@ListCodeRepositories' {} a -> s {creationTimeBefore = a} :: ListCodeRepositories) Core.. Lens.mapping Core._Time

-- | A filter that returns only Git repositories that were last modified
-- after the specified time.
listCodeRepositories_lastModifiedTimeAfter :: Lens.Lens' ListCodeRepositories (Core.Maybe Core.UTCTime)
listCodeRepositories_lastModifiedTimeAfter = Lens.lens (\ListCodeRepositories' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListCodeRepositories' {} a -> s {lastModifiedTimeAfter = a} :: ListCodeRepositories) Core.. Lens.mapping Core._Time

-- | The field to sort results by. The default is @Name@.
listCodeRepositories_sortBy :: Lens.Lens' ListCodeRepositories (Core.Maybe CodeRepositorySortBy)
listCodeRepositories_sortBy = Lens.lens (\ListCodeRepositories' {sortBy} -> sortBy) (\s@ListCodeRepositories' {} a -> s {sortBy = a} :: ListCodeRepositories)

-- | A filter that returns only Git repositories that were created after the
-- specified time.
listCodeRepositories_creationTimeAfter :: Lens.Lens' ListCodeRepositories (Core.Maybe Core.UTCTime)
listCodeRepositories_creationTimeAfter = Lens.lens (\ListCodeRepositories' {creationTimeAfter} -> creationTimeAfter) (\s@ListCodeRepositories' {} a -> s {creationTimeAfter = a} :: ListCodeRepositories) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListCodeRepositories where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCodeRepositoriesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listCodeRepositoriesResponse_codeRepositorySummaryList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listCodeRepositories_nextToken
          Lens..~ rs
          Lens.^? listCodeRepositoriesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListCodeRepositories where
  type
    AWSResponse ListCodeRepositories =
      ListCodeRepositoriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCodeRepositoriesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "CodeRepositorySummaryList"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListCodeRepositories

instance Core.NFData ListCodeRepositories

instance Core.ToHeaders ListCodeRepositories where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListCodeRepositories" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListCodeRepositories where
  toJSON ListCodeRepositories' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LastModifiedTimeBefore" Core..=)
              Core.<$> lastModifiedTimeBefore,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Core.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Core..=)
              Core.<$> lastModifiedTimeAfter,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("CreationTimeAfter" Core..=)
              Core.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListCodeRepositories where
  toPath = Core.const "/"

instance Core.ToQuery ListCodeRepositories where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListCodeRepositoriesResponse' smart constructor.
data ListCodeRepositoriesResponse = ListCodeRepositoriesResponse'
  { -- | If the result of a @ListCodeRepositoriesOutput@ request was truncated,
    -- the response includes a @NextToken@. To get the next set of Git
    -- repositories, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
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
    --     repository and the ARN of the AWS Secrets Manager secret that
    --     contains the credentials used to access the repository.
    codeRepositorySummaryList :: [CodeRepositorySummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
--     repository and the ARN of the AWS Secrets Manager secret that
--     contains the credentials used to access the repository.
newListCodeRepositoriesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListCodeRepositoriesResponse
newListCodeRepositoriesResponse pHttpStatus_ =
  ListCodeRepositoriesResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      codeRepositorySummaryList = Core.mempty
    }

-- | If the result of a @ListCodeRepositoriesOutput@ request was truncated,
-- the response includes a @NextToken@. To get the next set of Git
-- repositories, use the token in the next request.
listCodeRepositoriesResponse_nextToken :: Lens.Lens' ListCodeRepositoriesResponse (Core.Maybe Core.Text)
listCodeRepositoriesResponse_nextToken = Lens.lens (\ListCodeRepositoriesResponse' {nextToken} -> nextToken) (\s@ListCodeRepositoriesResponse' {} a -> s {nextToken = a} :: ListCodeRepositoriesResponse)

-- | The response's http status code.
listCodeRepositoriesResponse_httpStatus :: Lens.Lens' ListCodeRepositoriesResponse Core.Int
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
--     repository and the ARN of the AWS Secrets Manager secret that
--     contains the credentials used to access the repository.
listCodeRepositoriesResponse_codeRepositorySummaryList :: Lens.Lens' ListCodeRepositoriesResponse [CodeRepositorySummary]
listCodeRepositoriesResponse_codeRepositorySummaryList = Lens.lens (\ListCodeRepositoriesResponse' {codeRepositorySummaryList} -> codeRepositorySummaryList) (\s@ListCodeRepositoriesResponse' {} a -> s {codeRepositorySummaryList = a} :: ListCodeRepositoriesResponse) Core.. Lens._Coerce

instance Core.NFData ListCodeRepositoriesResponse
