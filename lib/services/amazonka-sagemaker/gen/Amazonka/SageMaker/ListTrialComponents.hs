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
-- Module      : Amazonka.SageMaker.ListTrialComponents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the trial components in your account. You can sort the list by
-- trial component name or creation time. You can filter the list to show
-- only components that were created in a specific time range. You can also
-- filter on one of the following:
--
-- -   @ExperimentName@
--
-- -   @SourceArn@
--
-- -   @TrialName@
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListTrialComponents
  ( -- * Creating a Request
    ListTrialComponents (..),
    newListTrialComponents,

    -- * Request Lenses
    listTrialComponents_createdAfter,
    listTrialComponents_createdBefore,
    listTrialComponents_experimentName,
    listTrialComponents_maxResults,
    listTrialComponents_nextToken,
    listTrialComponents_sortBy,
    listTrialComponents_sortOrder,
    listTrialComponents_sourceArn,
    listTrialComponents_trialName,

    -- * Destructuring the Response
    ListTrialComponentsResponse (..),
    newListTrialComponentsResponse,

    -- * Response Lenses
    listTrialComponentsResponse_nextToken,
    listTrialComponentsResponse_trialComponentSummaries,
    listTrialComponentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListTrialComponents' smart constructor.
data ListTrialComponents = ListTrialComponents'
  { -- | A filter that returns only components created after the specified time.
    createdAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only components created before the specified time.
    createdBefore :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only components that are part of the specified
    -- experiment. If you specify @ExperimentName@, you can\'t filter by
    -- @SourceArn@ or @TrialName@.
    experimentName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of components to return in the response. The default
    -- value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous call to @ListTrialComponents@ didn\'t return the full
    -- set of components, the call returns a token for getting the next set of
    -- components.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Prelude.Maybe SortTrialComponentsBy,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns only components that have the specified source
    -- Amazon Resource Name (ARN). If you specify @SourceArn@, you can\'t
    -- filter by @ExperimentName@ or @TrialName@.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only components that are part of the specified
    -- trial. If you specify @TrialName@, you can\'t filter by @ExperimentName@
    -- or @SourceArn@.
    trialName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrialComponents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'listTrialComponents_createdAfter' - A filter that returns only components created after the specified time.
--
-- 'createdBefore', 'listTrialComponents_createdBefore' - A filter that returns only components created before the specified time.
--
-- 'experimentName', 'listTrialComponents_experimentName' - A filter that returns only components that are part of the specified
-- experiment. If you specify @ExperimentName@, you can\'t filter by
-- @SourceArn@ or @TrialName@.
--
-- 'maxResults', 'listTrialComponents_maxResults' - The maximum number of components to return in the response. The default
-- value is 10.
--
-- 'nextToken', 'listTrialComponents_nextToken' - If the previous call to @ListTrialComponents@ didn\'t return the full
-- set of components, the call returns a token for getting the next set of
-- components.
--
-- 'sortBy', 'listTrialComponents_sortBy' - The property used to sort results. The default value is @CreationTime@.
--
-- 'sortOrder', 'listTrialComponents_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'sourceArn', 'listTrialComponents_sourceArn' - A filter that returns only components that have the specified source
-- Amazon Resource Name (ARN). If you specify @SourceArn@, you can\'t
-- filter by @ExperimentName@ or @TrialName@.
--
-- 'trialName', 'listTrialComponents_trialName' - A filter that returns only components that are part of the specified
-- trial. If you specify @TrialName@, you can\'t filter by @ExperimentName@
-- or @SourceArn@.
newListTrialComponents ::
  ListTrialComponents
newListTrialComponents =
  ListTrialComponents'
    { createdAfter =
        Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      experimentName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      trialName = Prelude.Nothing
    }

-- | A filter that returns only components created after the specified time.
listTrialComponents_createdAfter :: Lens.Lens' ListTrialComponents (Prelude.Maybe Prelude.UTCTime)
listTrialComponents_createdAfter = Lens.lens (\ListTrialComponents' {createdAfter} -> createdAfter) (\s@ListTrialComponents' {} a -> s {createdAfter = a} :: ListTrialComponents) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only components created before the specified time.
listTrialComponents_createdBefore :: Lens.Lens' ListTrialComponents (Prelude.Maybe Prelude.UTCTime)
listTrialComponents_createdBefore = Lens.lens (\ListTrialComponents' {createdBefore} -> createdBefore) (\s@ListTrialComponents' {} a -> s {createdBefore = a} :: ListTrialComponents) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only components that are part of the specified
-- experiment. If you specify @ExperimentName@, you can\'t filter by
-- @SourceArn@ or @TrialName@.
listTrialComponents_experimentName :: Lens.Lens' ListTrialComponents (Prelude.Maybe Prelude.Text)
listTrialComponents_experimentName = Lens.lens (\ListTrialComponents' {experimentName} -> experimentName) (\s@ListTrialComponents' {} a -> s {experimentName = a} :: ListTrialComponents)

-- | The maximum number of components to return in the response. The default
-- value is 10.
listTrialComponents_maxResults :: Lens.Lens' ListTrialComponents (Prelude.Maybe Prelude.Natural)
listTrialComponents_maxResults = Lens.lens (\ListTrialComponents' {maxResults} -> maxResults) (\s@ListTrialComponents' {} a -> s {maxResults = a} :: ListTrialComponents)

-- | If the previous call to @ListTrialComponents@ didn\'t return the full
-- set of components, the call returns a token for getting the next set of
-- components.
listTrialComponents_nextToken :: Lens.Lens' ListTrialComponents (Prelude.Maybe Prelude.Text)
listTrialComponents_nextToken = Lens.lens (\ListTrialComponents' {nextToken} -> nextToken) (\s@ListTrialComponents' {} a -> s {nextToken = a} :: ListTrialComponents)

-- | The property used to sort results. The default value is @CreationTime@.
listTrialComponents_sortBy :: Lens.Lens' ListTrialComponents (Prelude.Maybe SortTrialComponentsBy)
listTrialComponents_sortBy = Lens.lens (\ListTrialComponents' {sortBy} -> sortBy) (\s@ListTrialComponents' {} a -> s {sortBy = a} :: ListTrialComponents)

-- | The sort order. The default value is @Descending@.
listTrialComponents_sortOrder :: Lens.Lens' ListTrialComponents (Prelude.Maybe SortOrder)
listTrialComponents_sortOrder = Lens.lens (\ListTrialComponents' {sortOrder} -> sortOrder) (\s@ListTrialComponents' {} a -> s {sortOrder = a} :: ListTrialComponents)

-- | A filter that returns only components that have the specified source
-- Amazon Resource Name (ARN). If you specify @SourceArn@, you can\'t
-- filter by @ExperimentName@ or @TrialName@.
listTrialComponents_sourceArn :: Lens.Lens' ListTrialComponents (Prelude.Maybe Prelude.Text)
listTrialComponents_sourceArn = Lens.lens (\ListTrialComponents' {sourceArn} -> sourceArn) (\s@ListTrialComponents' {} a -> s {sourceArn = a} :: ListTrialComponents)

-- | A filter that returns only components that are part of the specified
-- trial. If you specify @TrialName@, you can\'t filter by @ExperimentName@
-- or @SourceArn@.
listTrialComponents_trialName :: Lens.Lens' ListTrialComponents (Prelude.Maybe Prelude.Text)
listTrialComponents_trialName = Lens.lens (\ListTrialComponents' {trialName} -> trialName) (\s@ListTrialComponents' {} a -> s {trialName = a} :: ListTrialComponents)

instance Core.AWSPager ListTrialComponents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTrialComponentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTrialComponentsResponse_trialComponentSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTrialComponents_nextToken
          Lens..~ rs
          Lens.^? listTrialComponentsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTrialComponents where
  type
    AWSResponse ListTrialComponents =
      ListTrialComponentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrialComponentsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "TrialComponentSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTrialComponents where
  hashWithSalt _salt ListTrialComponents' {..} =
    _salt
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` experimentName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` trialName

instance Prelude.NFData ListTrialComponents where
  rnf ListTrialComponents' {..} =
    Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf experimentName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf trialName

instance Data.ToHeaders ListTrialComponents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListTrialComponents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTrialComponents where
  toJSON ListTrialComponents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreatedAfter" Data..=) Prelude.<$> createdAfter,
            ("CreatedBefore" Data..=) Prelude.<$> createdBefore,
            ("ExperimentName" Data..=)
              Prelude.<$> experimentName,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("SourceArn" Data..=) Prelude.<$> sourceArn,
            ("TrialName" Data..=) Prelude.<$> trialName
          ]
      )

instance Data.ToPath ListTrialComponents where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTrialComponents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTrialComponentsResponse' smart constructor.
data ListTrialComponentsResponse = ListTrialComponentsResponse'
  { -- | A token for getting the next set of components, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the summaries of your trial components.
    trialComponentSummaries :: Prelude.Maybe [TrialComponentSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrialComponentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTrialComponentsResponse_nextToken' - A token for getting the next set of components, if there are any.
--
-- 'trialComponentSummaries', 'listTrialComponentsResponse_trialComponentSummaries' - A list of the summaries of your trial components.
--
-- 'httpStatus', 'listTrialComponentsResponse_httpStatus' - The response's http status code.
newListTrialComponentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTrialComponentsResponse
newListTrialComponentsResponse pHttpStatus_ =
  ListTrialComponentsResponse'
    { nextToken =
        Prelude.Nothing,
      trialComponentSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of components, if there are any.
listTrialComponentsResponse_nextToken :: Lens.Lens' ListTrialComponentsResponse (Prelude.Maybe Prelude.Text)
listTrialComponentsResponse_nextToken = Lens.lens (\ListTrialComponentsResponse' {nextToken} -> nextToken) (\s@ListTrialComponentsResponse' {} a -> s {nextToken = a} :: ListTrialComponentsResponse)

-- | A list of the summaries of your trial components.
listTrialComponentsResponse_trialComponentSummaries :: Lens.Lens' ListTrialComponentsResponse (Prelude.Maybe [TrialComponentSummary])
listTrialComponentsResponse_trialComponentSummaries = Lens.lens (\ListTrialComponentsResponse' {trialComponentSummaries} -> trialComponentSummaries) (\s@ListTrialComponentsResponse' {} a -> s {trialComponentSummaries = a} :: ListTrialComponentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTrialComponentsResponse_httpStatus :: Lens.Lens' ListTrialComponentsResponse Prelude.Int
listTrialComponentsResponse_httpStatus = Lens.lens (\ListTrialComponentsResponse' {httpStatus} -> httpStatus) (\s@ListTrialComponentsResponse' {} a -> s {httpStatus = a} :: ListTrialComponentsResponse)

instance Prelude.NFData ListTrialComponentsResponse where
  rnf ListTrialComponentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trialComponentSummaries
      `Prelude.seq` Prelude.rnf httpStatus
