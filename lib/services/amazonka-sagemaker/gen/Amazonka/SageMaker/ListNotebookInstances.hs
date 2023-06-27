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
-- Module      : Amazonka.SageMaker.ListNotebookInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the SageMaker notebook instances in the requester\'s
-- account in an Amazon Web Services Region.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListNotebookInstances
  ( -- * Creating a Request
    ListNotebookInstances (..),
    newListNotebookInstances,

    -- * Request Lenses
    listNotebookInstances_additionalCodeRepositoryEquals,
    listNotebookInstances_creationTimeAfter,
    listNotebookInstances_creationTimeBefore,
    listNotebookInstances_defaultCodeRepositoryContains,
    listNotebookInstances_lastModifiedTimeAfter,
    listNotebookInstances_lastModifiedTimeBefore,
    listNotebookInstances_maxResults,
    listNotebookInstances_nameContains,
    listNotebookInstances_nextToken,
    listNotebookInstances_notebookInstanceLifecycleConfigNameContains,
    listNotebookInstances_sortBy,
    listNotebookInstances_sortOrder,
    listNotebookInstances_statusEquals,

    -- * Destructuring the Response
    ListNotebookInstancesResponse (..),
    newListNotebookInstancesResponse,

    -- * Response Lenses
    listNotebookInstancesResponse_nextToken,
    listNotebookInstancesResponse_notebookInstances,
    listNotebookInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListNotebookInstances' smart constructor.
data ListNotebookInstances = ListNotebookInstances'
  { -- | A filter that returns only notebook instances with associated with the
    -- specified git repository.
    additionalCodeRepositoryEquals :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only notebook instances that were created after
    -- the specified time (timestamp).
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only notebook instances that were created before
    -- the specified time (timestamp).
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | A string in the name or URL of a Git repository associated with this
    -- notebook instance. This filter returns only notebook instances
    -- associated with a git repository with a name that contains the specified
    -- string.
    defaultCodeRepositoryContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only notebook instances that were modified after
    -- the specified time (timestamp).
    lastModifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only notebook instances that were modified before
    -- the specified time (timestamp).
    lastModifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of notebook instances to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A string in the notebook instances\' name. This filter returns only
    -- notebook instances whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the previous call to the @ListNotebookInstances@ is truncated, the
    -- response includes a @NextToken@. You can use this token in your
    -- subsequent @ListNotebookInstances@ request to fetch the next set of
    -- notebook instances.
    --
    -- You might specify a filter or a sort order in your request. When
    -- response is truncated, you must use the same values for the filer and
    -- sort order in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A string in the name of a notebook instances lifecycle configuration
    -- associated with this notebook instance. This filter returns only
    -- notebook instances associated with a lifecycle configuration with a name
    -- that contains the specified string.
    notebookInstanceLifecycleConfigNameContains :: Prelude.Maybe Prelude.Text,
    -- | The field to sort results by. The default is @Name@.
    sortBy :: Prelude.Maybe NotebookInstanceSortKey,
    -- | The sort order for results.
    sortOrder :: Prelude.Maybe NotebookInstanceSortOrder,
    -- | A filter that returns only notebook instances with the specified status.
    statusEquals :: Prelude.Maybe NotebookInstanceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNotebookInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalCodeRepositoryEquals', 'listNotebookInstances_additionalCodeRepositoryEquals' - A filter that returns only notebook instances with associated with the
-- specified git repository.
--
-- 'creationTimeAfter', 'listNotebookInstances_creationTimeAfter' - A filter that returns only notebook instances that were created after
-- the specified time (timestamp).
--
-- 'creationTimeBefore', 'listNotebookInstances_creationTimeBefore' - A filter that returns only notebook instances that were created before
-- the specified time (timestamp).
--
-- 'defaultCodeRepositoryContains', 'listNotebookInstances_defaultCodeRepositoryContains' - A string in the name or URL of a Git repository associated with this
-- notebook instance. This filter returns only notebook instances
-- associated with a git repository with a name that contains the specified
-- string.
--
-- 'lastModifiedTimeAfter', 'listNotebookInstances_lastModifiedTimeAfter' - A filter that returns only notebook instances that were modified after
-- the specified time (timestamp).
--
-- 'lastModifiedTimeBefore', 'listNotebookInstances_lastModifiedTimeBefore' - A filter that returns only notebook instances that were modified before
-- the specified time (timestamp).
--
-- 'maxResults', 'listNotebookInstances_maxResults' - The maximum number of notebook instances to return.
--
-- 'nameContains', 'listNotebookInstances_nameContains' - A string in the notebook instances\' name. This filter returns only
-- notebook instances whose name contains the specified string.
--
-- 'nextToken', 'listNotebookInstances_nextToken' - If the previous call to the @ListNotebookInstances@ is truncated, the
-- response includes a @NextToken@. You can use this token in your
-- subsequent @ListNotebookInstances@ request to fetch the next set of
-- notebook instances.
--
-- You might specify a filter or a sort order in your request. When
-- response is truncated, you must use the same values for the filer and
-- sort order in the next request.
--
-- 'notebookInstanceLifecycleConfigNameContains', 'listNotebookInstances_notebookInstanceLifecycleConfigNameContains' - A string in the name of a notebook instances lifecycle configuration
-- associated with this notebook instance. This filter returns only
-- notebook instances associated with a lifecycle configuration with a name
-- that contains the specified string.
--
-- 'sortBy', 'listNotebookInstances_sortBy' - The field to sort results by. The default is @Name@.
--
-- 'sortOrder', 'listNotebookInstances_sortOrder' - The sort order for results.
--
-- 'statusEquals', 'listNotebookInstances_statusEquals' - A filter that returns only notebook instances with the specified status.
newListNotebookInstances ::
  ListNotebookInstances
newListNotebookInstances =
  ListNotebookInstances'
    { additionalCodeRepositoryEquals =
        Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      defaultCodeRepositoryContains = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      notebookInstanceLifecycleConfigNameContains =
        Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      statusEquals = Prelude.Nothing
    }

-- | A filter that returns only notebook instances with associated with the
-- specified git repository.
listNotebookInstances_additionalCodeRepositoryEquals :: Lens.Lens' ListNotebookInstances (Prelude.Maybe Prelude.Text)
listNotebookInstances_additionalCodeRepositoryEquals = Lens.lens (\ListNotebookInstances' {additionalCodeRepositoryEquals} -> additionalCodeRepositoryEquals) (\s@ListNotebookInstances' {} a -> s {additionalCodeRepositoryEquals = a} :: ListNotebookInstances)

-- | A filter that returns only notebook instances that were created after
-- the specified time (timestamp).
listNotebookInstances_creationTimeAfter :: Lens.Lens' ListNotebookInstances (Prelude.Maybe Prelude.UTCTime)
listNotebookInstances_creationTimeAfter = Lens.lens (\ListNotebookInstances' {creationTimeAfter} -> creationTimeAfter) (\s@ListNotebookInstances' {} a -> s {creationTimeAfter = a} :: ListNotebookInstances) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only notebook instances that were created before
-- the specified time (timestamp).
listNotebookInstances_creationTimeBefore :: Lens.Lens' ListNotebookInstances (Prelude.Maybe Prelude.UTCTime)
listNotebookInstances_creationTimeBefore = Lens.lens (\ListNotebookInstances' {creationTimeBefore} -> creationTimeBefore) (\s@ListNotebookInstances' {} a -> s {creationTimeBefore = a} :: ListNotebookInstances) Prelude.. Lens.mapping Data._Time

-- | A string in the name or URL of a Git repository associated with this
-- notebook instance. This filter returns only notebook instances
-- associated with a git repository with a name that contains the specified
-- string.
listNotebookInstances_defaultCodeRepositoryContains :: Lens.Lens' ListNotebookInstances (Prelude.Maybe Prelude.Text)
listNotebookInstances_defaultCodeRepositoryContains = Lens.lens (\ListNotebookInstances' {defaultCodeRepositoryContains} -> defaultCodeRepositoryContains) (\s@ListNotebookInstances' {} a -> s {defaultCodeRepositoryContains = a} :: ListNotebookInstances)

-- | A filter that returns only notebook instances that were modified after
-- the specified time (timestamp).
listNotebookInstances_lastModifiedTimeAfter :: Lens.Lens' ListNotebookInstances (Prelude.Maybe Prelude.UTCTime)
listNotebookInstances_lastModifiedTimeAfter = Lens.lens (\ListNotebookInstances' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListNotebookInstances' {} a -> s {lastModifiedTimeAfter = a} :: ListNotebookInstances) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only notebook instances that were modified before
-- the specified time (timestamp).
listNotebookInstances_lastModifiedTimeBefore :: Lens.Lens' ListNotebookInstances (Prelude.Maybe Prelude.UTCTime)
listNotebookInstances_lastModifiedTimeBefore = Lens.lens (\ListNotebookInstances' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListNotebookInstances' {} a -> s {lastModifiedTimeBefore = a} :: ListNotebookInstances) Prelude.. Lens.mapping Data._Time

-- | The maximum number of notebook instances to return.
listNotebookInstances_maxResults :: Lens.Lens' ListNotebookInstances (Prelude.Maybe Prelude.Natural)
listNotebookInstances_maxResults = Lens.lens (\ListNotebookInstances' {maxResults} -> maxResults) (\s@ListNotebookInstances' {} a -> s {maxResults = a} :: ListNotebookInstances)

-- | A string in the notebook instances\' name. This filter returns only
-- notebook instances whose name contains the specified string.
listNotebookInstances_nameContains :: Lens.Lens' ListNotebookInstances (Prelude.Maybe Prelude.Text)
listNotebookInstances_nameContains = Lens.lens (\ListNotebookInstances' {nameContains} -> nameContains) (\s@ListNotebookInstances' {} a -> s {nameContains = a} :: ListNotebookInstances)

-- | If the previous call to the @ListNotebookInstances@ is truncated, the
-- response includes a @NextToken@. You can use this token in your
-- subsequent @ListNotebookInstances@ request to fetch the next set of
-- notebook instances.
--
-- You might specify a filter or a sort order in your request. When
-- response is truncated, you must use the same values for the filer and
-- sort order in the next request.
listNotebookInstances_nextToken :: Lens.Lens' ListNotebookInstances (Prelude.Maybe Prelude.Text)
listNotebookInstances_nextToken = Lens.lens (\ListNotebookInstances' {nextToken} -> nextToken) (\s@ListNotebookInstances' {} a -> s {nextToken = a} :: ListNotebookInstances)

-- | A string in the name of a notebook instances lifecycle configuration
-- associated with this notebook instance. This filter returns only
-- notebook instances associated with a lifecycle configuration with a name
-- that contains the specified string.
listNotebookInstances_notebookInstanceLifecycleConfigNameContains :: Lens.Lens' ListNotebookInstances (Prelude.Maybe Prelude.Text)
listNotebookInstances_notebookInstanceLifecycleConfigNameContains = Lens.lens (\ListNotebookInstances' {notebookInstanceLifecycleConfigNameContains} -> notebookInstanceLifecycleConfigNameContains) (\s@ListNotebookInstances' {} a -> s {notebookInstanceLifecycleConfigNameContains = a} :: ListNotebookInstances)

-- | The field to sort results by. The default is @Name@.
listNotebookInstances_sortBy :: Lens.Lens' ListNotebookInstances (Prelude.Maybe NotebookInstanceSortKey)
listNotebookInstances_sortBy = Lens.lens (\ListNotebookInstances' {sortBy} -> sortBy) (\s@ListNotebookInstances' {} a -> s {sortBy = a} :: ListNotebookInstances)

-- | The sort order for results.
listNotebookInstances_sortOrder :: Lens.Lens' ListNotebookInstances (Prelude.Maybe NotebookInstanceSortOrder)
listNotebookInstances_sortOrder = Lens.lens (\ListNotebookInstances' {sortOrder} -> sortOrder) (\s@ListNotebookInstances' {} a -> s {sortOrder = a} :: ListNotebookInstances)

-- | A filter that returns only notebook instances with the specified status.
listNotebookInstances_statusEquals :: Lens.Lens' ListNotebookInstances (Prelude.Maybe NotebookInstanceStatus)
listNotebookInstances_statusEquals = Lens.lens (\ListNotebookInstances' {statusEquals} -> statusEquals) (\s@ListNotebookInstances' {} a -> s {statusEquals = a} :: ListNotebookInstances)

instance Core.AWSPager ListNotebookInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listNotebookInstancesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listNotebookInstancesResponse_notebookInstances
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listNotebookInstances_nextToken
          Lens..~ rs
          Lens.^? listNotebookInstancesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListNotebookInstances where
  type
    AWSResponse ListNotebookInstances =
      ListNotebookInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNotebookInstancesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "NotebookInstances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNotebookInstances where
  hashWithSalt _salt ListNotebookInstances' {..} =
    _salt
      `Prelude.hashWithSalt` additionalCodeRepositoryEquals
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` defaultCodeRepositoryContains
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` notebookInstanceLifecycleConfigNameContains
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` statusEquals

instance Prelude.NFData ListNotebookInstances where
  rnf ListNotebookInstances' {..} =
    Prelude.rnf additionalCodeRepositoryEquals
      `Prelude.seq` Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf defaultCodeRepositoryContains
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf
        notebookInstanceLifecycleConfigNameContains
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf statusEquals

instance Data.ToHeaders ListNotebookInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListNotebookInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListNotebookInstances where
  toJSON ListNotebookInstances' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalCodeRepositoryEquals" Data..=)
              Prelude.<$> additionalCodeRepositoryEquals,
            ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("DefaultCodeRepositoryContains" Data..=)
              Prelude.<$> defaultCodeRepositoryContains,
            ("LastModifiedTimeAfter" Data..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("LastModifiedTimeBefore" Data..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ( "NotebookInstanceLifecycleConfigNameContains"
                Data..=
            )
              Prelude.<$> notebookInstanceLifecycleConfigNameContains,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("StatusEquals" Data..=) Prelude.<$> statusEquals
          ]
      )

instance Data.ToPath ListNotebookInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery ListNotebookInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListNotebookInstancesResponse' smart constructor.
data ListNotebookInstancesResponse = ListNotebookInstancesResponse'
  { -- | If the response to the previous @ListNotebookInstances@ request was
    -- truncated, SageMaker returns this token. To retrieve the next set of
    -- notebook instances, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @NotebookInstanceSummary@ objects, one for each notebook
    -- instance.
    notebookInstances :: Prelude.Maybe [NotebookInstanceSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNotebookInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNotebookInstancesResponse_nextToken' - If the response to the previous @ListNotebookInstances@ request was
-- truncated, SageMaker returns this token. To retrieve the next set of
-- notebook instances, use the token in the next request.
--
-- 'notebookInstances', 'listNotebookInstancesResponse_notebookInstances' - An array of @NotebookInstanceSummary@ objects, one for each notebook
-- instance.
--
-- 'httpStatus', 'listNotebookInstancesResponse_httpStatus' - The response's http status code.
newListNotebookInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNotebookInstancesResponse
newListNotebookInstancesResponse pHttpStatus_ =
  ListNotebookInstancesResponse'
    { nextToken =
        Prelude.Nothing,
      notebookInstances = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response to the previous @ListNotebookInstances@ request was
-- truncated, SageMaker returns this token. To retrieve the next set of
-- notebook instances, use the token in the next request.
listNotebookInstancesResponse_nextToken :: Lens.Lens' ListNotebookInstancesResponse (Prelude.Maybe Prelude.Text)
listNotebookInstancesResponse_nextToken = Lens.lens (\ListNotebookInstancesResponse' {nextToken} -> nextToken) (\s@ListNotebookInstancesResponse' {} a -> s {nextToken = a} :: ListNotebookInstancesResponse)

-- | An array of @NotebookInstanceSummary@ objects, one for each notebook
-- instance.
listNotebookInstancesResponse_notebookInstances :: Lens.Lens' ListNotebookInstancesResponse (Prelude.Maybe [NotebookInstanceSummary])
listNotebookInstancesResponse_notebookInstances = Lens.lens (\ListNotebookInstancesResponse' {notebookInstances} -> notebookInstances) (\s@ListNotebookInstancesResponse' {} a -> s {notebookInstances = a} :: ListNotebookInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listNotebookInstancesResponse_httpStatus :: Lens.Lens' ListNotebookInstancesResponse Prelude.Int
listNotebookInstancesResponse_httpStatus = Lens.lens (\ListNotebookInstancesResponse' {httpStatus} -> httpStatus) (\s@ListNotebookInstancesResponse' {} a -> s {httpStatus = a} :: ListNotebookInstancesResponse)

instance Prelude.NFData ListNotebookInstancesResponse where
  rnf ListNotebookInstancesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf notebookInstances
      `Prelude.seq` Prelude.rnf httpStatus
