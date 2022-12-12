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
-- Module      : Amazonka.SageMakerA2IRuntime.ListHumanLoops
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about human loops, given the specified parameters.
-- If a human loop was deleted, it will not be included.
--
-- This operation returns paginated results.
module Amazonka.SageMakerA2IRuntime.ListHumanLoops
  ( -- * Creating a Request
    ListHumanLoops (..),
    newListHumanLoops,

    -- * Request Lenses
    listHumanLoops_creationTimeAfter,
    listHumanLoops_creationTimeBefore,
    listHumanLoops_maxResults,
    listHumanLoops_nextToken,
    listHumanLoops_sortOrder,
    listHumanLoops_flowDefinitionArn,

    -- * Destructuring the Response
    ListHumanLoopsResponse (..),
    newListHumanLoopsResponse,

    -- * Response Lenses
    listHumanLoopsResponse_nextToken,
    listHumanLoopsResponse_httpStatus,
    listHumanLoopsResponse_humanLoopSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerA2IRuntime.Types

-- | /See:/ 'newListHumanLoops' smart constructor.
data ListHumanLoops = ListHumanLoops'
  { -- | (Optional) The timestamp of the date when you want the human loops to
    -- begin in ISO 8601 format. For example, @2020-02-24@.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | (Optional) The timestamp of the date before which you want the human
    -- loops to begin in ISO 8601 format. For example, @2020-02-24@.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The total number of items to return. If the total number of available
    -- items is more than the value specified in @MaxResults@, then a
    -- @NextToken@ is returned in the output. You can use this token to display
    -- the next page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to display the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Optional. The order for displaying results. Valid values: @Ascending@
    -- and @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The Amazon Resource Name (ARN) of a flow definition.
    flowDefinitionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHumanLoops' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listHumanLoops_creationTimeAfter' - (Optional) The timestamp of the date when you want the human loops to
-- begin in ISO 8601 format. For example, @2020-02-24@.
--
-- 'creationTimeBefore', 'listHumanLoops_creationTimeBefore' - (Optional) The timestamp of the date before which you want the human
-- loops to begin in ISO 8601 format. For example, @2020-02-24@.
--
-- 'maxResults', 'listHumanLoops_maxResults' - The total number of items to return. If the total number of available
-- items is more than the value specified in @MaxResults@, then a
-- @NextToken@ is returned in the output. You can use this token to display
-- the next page of results.
--
-- 'nextToken', 'listHumanLoops_nextToken' - A token to display the next page of results.
--
-- 'sortOrder', 'listHumanLoops_sortOrder' - Optional. The order for displaying results. Valid values: @Ascending@
-- and @Descending@.
--
-- 'flowDefinitionArn', 'listHumanLoops_flowDefinitionArn' - The Amazon Resource Name (ARN) of a flow definition.
newListHumanLoops ::
  -- | 'flowDefinitionArn'
  Prelude.Text ->
  ListHumanLoops
newListHumanLoops pFlowDefinitionArn_ =
  ListHumanLoops'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      flowDefinitionArn = pFlowDefinitionArn_
    }

-- | (Optional) The timestamp of the date when you want the human loops to
-- begin in ISO 8601 format. For example, @2020-02-24@.
listHumanLoops_creationTimeAfter :: Lens.Lens' ListHumanLoops (Prelude.Maybe Prelude.UTCTime)
listHumanLoops_creationTimeAfter = Lens.lens (\ListHumanLoops' {creationTimeAfter} -> creationTimeAfter) (\s@ListHumanLoops' {} a -> s {creationTimeAfter = a} :: ListHumanLoops) Prelude.. Lens.mapping Data._Time

-- | (Optional) The timestamp of the date before which you want the human
-- loops to begin in ISO 8601 format. For example, @2020-02-24@.
listHumanLoops_creationTimeBefore :: Lens.Lens' ListHumanLoops (Prelude.Maybe Prelude.UTCTime)
listHumanLoops_creationTimeBefore = Lens.lens (\ListHumanLoops' {creationTimeBefore} -> creationTimeBefore) (\s@ListHumanLoops' {} a -> s {creationTimeBefore = a} :: ListHumanLoops) Prelude.. Lens.mapping Data._Time

-- | The total number of items to return. If the total number of available
-- items is more than the value specified in @MaxResults@, then a
-- @NextToken@ is returned in the output. You can use this token to display
-- the next page of results.
listHumanLoops_maxResults :: Lens.Lens' ListHumanLoops (Prelude.Maybe Prelude.Natural)
listHumanLoops_maxResults = Lens.lens (\ListHumanLoops' {maxResults} -> maxResults) (\s@ListHumanLoops' {} a -> s {maxResults = a} :: ListHumanLoops)

-- | A token to display the next page of results.
listHumanLoops_nextToken :: Lens.Lens' ListHumanLoops (Prelude.Maybe Prelude.Text)
listHumanLoops_nextToken = Lens.lens (\ListHumanLoops' {nextToken} -> nextToken) (\s@ListHumanLoops' {} a -> s {nextToken = a} :: ListHumanLoops)

-- | Optional. The order for displaying results. Valid values: @Ascending@
-- and @Descending@.
listHumanLoops_sortOrder :: Lens.Lens' ListHumanLoops (Prelude.Maybe SortOrder)
listHumanLoops_sortOrder = Lens.lens (\ListHumanLoops' {sortOrder} -> sortOrder) (\s@ListHumanLoops' {} a -> s {sortOrder = a} :: ListHumanLoops)

-- | The Amazon Resource Name (ARN) of a flow definition.
listHumanLoops_flowDefinitionArn :: Lens.Lens' ListHumanLoops Prelude.Text
listHumanLoops_flowDefinitionArn = Lens.lens (\ListHumanLoops' {flowDefinitionArn} -> flowDefinitionArn) (\s@ListHumanLoops' {} a -> s {flowDefinitionArn = a} :: ListHumanLoops)

instance Core.AWSPager ListHumanLoops where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHumanLoopsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listHumanLoopsResponse_humanLoopSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listHumanLoops_nextToken
          Lens..~ rs
          Lens.^? listHumanLoopsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListHumanLoops where
  type
    AWSResponse ListHumanLoops =
      ListHumanLoopsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHumanLoopsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "HumanLoopSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListHumanLoops where
  hashWithSalt _salt ListHumanLoops' {..} =
    _salt `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` flowDefinitionArn

instance Prelude.NFData ListHumanLoops where
  rnf ListHumanLoops' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf flowDefinitionArn

instance Data.ToHeaders ListHumanLoops where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListHumanLoops where
  toPath = Prelude.const "/human-loops"

instance Data.ToQuery ListHumanLoops where
  toQuery ListHumanLoops' {..} =
    Prelude.mconcat
      [ "CreationTimeAfter" Data.=: creationTimeAfter,
        "CreationTimeBefore" Data.=: creationTimeBefore,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "SortOrder" Data.=: sortOrder,
        "FlowDefinitionArn" Data.=: flowDefinitionArn
      ]

-- | /See:/ 'newListHumanLoopsResponse' smart constructor.
data ListHumanLoopsResponse = ListHumanLoopsResponse'
  { -- | A token to display the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of objects that contain information about the human loops.
    humanLoopSummaries :: [HumanLoopSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHumanLoopsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHumanLoopsResponse_nextToken' - A token to display the next page of results.
--
-- 'httpStatus', 'listHumanLoopsResponse_httpStatus' - The response's http status code.
--
-- 'humanLoopSummaries', 'listHumanLoopsResponse_humanLoopSummaries' - An array of objects that contain information about the human loops.
newListHumanLoopsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListHumanLoopsResponse
newListHumanLoopsResponse pHttpStatus_ =
  ListHumanLoopsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      humanLoopSummaries = Prelude.mempty
    }

-- | A token to display the next page of results.
listHumanLoopsResponse_nextToken :: Lens.Lens' ListHumanLoopsResponse (Prelude.Maybe Prelude.Text)
listHumanLoopsResponse_nextToken = Lens.lens (\ListHumanLoopsResponse' {nextToken} -> nextToken) (\s@ListHumanLoopsResponse' {} a -> s {nextToken = a} :: ListHumanLoopsResponse)

-- | The response's http status code.
listHumanLoopsResponse_httpStatus :: Lens.Lens' ListHumanLoopsResponse Prelude.Int
listHumanLoopsResponse_httpStatus = Lens.lens (\ListHumanLoopsResponse' {httpStatus} -> httpStatus) (\s@ListHumanLoopsResponse' {} a -> s {httpStatus = a} :: ListHumanLoopsResponse)

-- | An array of objects that contain information about the human loops.
listHumanLoopsResponse_humanLoopSummaries :: Lens.Lens' ListHumanLoopsResponse [HumanLoopSummary]
listHumanLoopsResponse_humanLoopSummaries = Lens.lens (\ListHumanLoopsResponse' {humanLoopSummaries} -> humanLoopSummaries) (\s@ListHumanLoopsResponse' {} a -> s {humanLoopSummaries = a} :: ListHumanLoopsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListHumanLoopsResponse where
  rnf ListHumanLoopsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf humanLoopSummaries
