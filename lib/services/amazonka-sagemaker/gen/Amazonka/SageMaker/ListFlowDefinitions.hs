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
-- Module      : Amazonka.SageMaker.ListFlowDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the flow definitions in your account.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListFlowDefinitions
  ( -- * Creating a Request
    ListFlowDefinitions (..),
    newListFlowDefinitions,

    -- * Request Lenses
    listFlowDefinitions_creationTimeAfter,
    listFlowDefinitions_creationTimeBefore,
    listFlowDefinitions_maxResults,
    listFlowDefinitions_nextToken,
    listFlowDefinitions_sortOrder,

    -- * Destructuring the Response
    ListFlowDefinitionsResponse (..),
    newListFlowDefinitionsResponse,

    -- * Response Lenses
    listFlowDefinitionsResponse_nextToken,
    listFlowDefinitionsResponse_httpStatus,
    listFlowDefinitionsResponse_flowDefinitionSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListFlowDefinitions' smart constructor.
data ListFlowDefinitions = ListFlowDefinitions'
  { -- | A filter that returns only flow definitions with a creation time greater
    -- than or equal to the specified timestamp.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only flow definitions that were created before the
    -- specified timestamp.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The total number of items to return. If the total number of available
    -- items is more than the value specified in @MaxResults@, then a
    -- @NextToken@ will be provided in the output that you can use to resume
    -- pagination.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to resume pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional value that specifies whether you want the results sorted in
    -- @Ascending@ or @Descending@ order.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFlowDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listFlowDefinitions_creationTimeAfter' - A filter that returns only flow definitions with a creation time greater
-- than or equal to the specified timestamp.
--
-- 'creationTimeBefore', 'listFlowDefinitions_creationTimeBefore' - A filter that returns only flow definitions that were created before the
-- specified timestamp.
--
-- 'maxResults', 'listFlowDefinitions_maxResults' - The total number of items to return. If the total number of available
-- items is more than the value specified in @MaxResults@, then a
-- @NextToken@ will be provided in the output that you can use to resume
-- pagination.
--
-- 'nextToken', 'listFlowDefinitions_nextToken' - A token to resume pagination.
--
-- 'sortOrder', 'listFlowDefinitions_sortOrder' - An optional value that specifies whether you want the results sorted in
-- @Ascending@ or @Descending@ order.
newListFlowDefinitions ::
  ListFlowDefinitions
newListFlowDefinitions =
  ListFlowDefinitions'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | A filter that returns only flow definitions with a creation time greater
-- than or equal to the specified timestamp.
listFlowDefinitions_creationTimeAfter :: Lens.Lens' ListFlowDefinitions (Prelude.Maybe Prelude.UTCTime)
listFlowDefinitions_creationTimeAfter = Lens.lens (\ListFlowDefinitions' {creationTimeAfter} -> creationTimeAfter) (\s@ListFlowDefinitions' {} a -> s {creationTimeAfter = a} :: ListFlowDefinitions) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only flow definitions that were created before the
-- specified timestamp.
listFlowDefinitions_creationTimeBefore :: Lens.Lens' ListFlowDefinitions (Prelude.Maybe Prelude.UTCTime)
listFlowDefinitions_creationTimeBefore = Lens.lens (\ListFlowDefinitions' {creationTimeBefore} -> creationTimeBefore) (\s@ListFlowDefinitions' {} a -> s {creationTimeBefore = a} :: ListFlowDefinitions) Prelude.. Lens.mapping Data._Time

-- | The total number of items to return. If the total number of available
-- items is more than the value specified in @MaxResults@, then a
-- @NextToken@ will be provided in the output that you can use to resume
-- pagination.
listFlowDefinitions_maxResults :: Lens.Lens' ListFlowDefinitions (Prelude.Maybe Prelude.Natural)
listFlowDefinitions_maxResults = Lens.lens (\ListFlowDefinitions' {maxResults} -> maxResults) (\s@ListFlowDefinitions' {} a -> s {maxResults = a} :: ListFlowDefinitions)

-- | A token to resume pagination.
listFlowDefinitions_nextToken :: Lens.Lens' ListFlowDefinitions (Prelude.Maybe Prelude.Text)
listFlowDefinitions_nextToken = Lens.lens (\ListFlowDefinitions' {nextToken} -> nextToken) (\s@ListFlowDefinitions' {} a -> s {nextToken = a} :: ListFlowDefinitions)

-- | An optional value that specifies whether you want the results sorted in
-- @Ascending@ or @Descending@ order.
listFlowDefinitions_sortOrder :: Lens.Lens' ListFlowDefinitions (Prelude.Maybe SortOrder)
listFlowDefinitions_sortOrder = Lens.lens (\ListFlowDefinitions' {sortOrder} -> sortOrder) (\s@ListFlowDefinitions' {} a -> s {sortOrder = a} :: ListFlowDefinitions)

instance Core.AWSPager ListFlowDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFlowDefinitionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listFlowDefinitionsResponse_flowDefinitionSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listFlowDefinitions_nextToken
          Lens..~ rs
          Lens.^? listFlowDefinitionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListFlowDefinitions where
  type
    AWSResponse ListFlowDefinitions =
      ListFlowDefinitionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFlowDefinitionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "FlowDefinitionSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListFlowDefinitions where
  hashWithSalt _salt ListFlowDefinitions' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListFlowDefinitions where
  rnf ListFlowDefinitions' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToHeaders ListFlowDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListFlowDefinitions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFlowDefinitions where
  toJSON ListFlowDefinitions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListFlowDefinitions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFlowDefinitions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFlowDefinitionsResponse' smart constructor.
data ListFlowDefinitionsResponse = ListFlowDefinitionsResponse'
  { -- | A token to resume pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of objects describing the flow definitions.
    flowDefinitionSummaries :: [FlowDefinitionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFlowDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFlowDefinitionsResponse_nextToken' - A token to resume pagination.
--
-- 'httpStatus', 'listFlowDefinitionsResponse_httpStatus' - The response's http status code.
--
-- 'flowDefinitionSummaries', 'listFlowDefinitionsResponse_flowDefinitionSummaries' - An array of objects describing the flow definitions.
newListFlowDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFlowDefinitionsResponse
newListFlowDefinitionsResponse pHttpStatus_ =
  ListFlowDefinitionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      flowDefinitionSummaries = Prelude.mempty
    }

-- | A token to resume pagination.
listFlowDefinitionsResponse_nextToken :: Lens.Lens' ListFlowDefinitionsResponse (Prelude.Maybe Prelude.Text)
listFlowDefinitionsResponse_nextToken = Lens.lens (\ListFlowDefinitionsResponse' {nextToken} -> nextToken) (\s@ListFlowDefinitionsResponse' {} a -> s {nextToken = a} :: ListFlowDefinitionsResponse)

-- | The response's http status code.
listFlowDefinitionsResponse_httpStatus :: Lens.Lens' ListFlowDefinitionsResponse Prelude.Int
listFlowDefinitionsResponse_httpStatus = Lens.lens (\ListFlowDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListFlowDefinitionsResponse' {} a -> s {httpStatus = a} :: ListFlowDefinitionsResponse)

-- | An array of objects describing the flow definitions.
listFlowDefinitionsResponse_flowDefinitionSummaries :: Lens.Lens' ListFlowDefinitionsResponse [FlowDefinitionSummary]
listFlowDefinitionsResponse_flowDefinitionSummaries = Lens.lens (\ListFlowDefinitionsResponse' {flowDefinitionSummaries} -> flowDefinitionSummaries) (\s@ListFlowDefinitionsResponse' {} a -> s {flowDefinitionSummaries = a} :: ListFlowDefinitionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListFlowDefinitionsResponse where
  rnf ListFlowDefinitionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf flowDefinitionSummaries
