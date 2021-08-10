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
-- Module      : Network.AWS.SSM.GetOpsSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- View a summary of OpsItems based on specified filters and aggregators.
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetOpsSummary
  ( -- * Creating a Request
    GetOpsSummary (..),
    newGetOpsSummary,

    -- * Request Lenses
    getOpsSummary_nextToken,
    getOpsSummary_maxResults,
    getOpsSummary_syncName,
    getOpsSummary_resultAttributes,
    getOpsSummary_filters,
    getOpsSummary_aggregators,

    -- * Destructuring the Response
    GetOpsSummaryResponse (..),
    newGetOpsSummaryResponse,

    -- * Response Lenses
    getOpsSummaryResponse_nextToken,
    getOpsSummaryResponse_entities,
    getOpsSummaryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetOpsSummary' smart constructor.
data GetOpsSummary = GetOpsSummary'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the name of a resource data sync to get.
    syncName :: Prelude.Maybe Prelude.Text,
    -- | The OpsItem data type to return.
    resultAttributes :: Prelude.Maybe (Prelude.NonEmpty OpsResultAttribute),
    -- | Optional filters used to scope down the returned OpsItems.
    filters :: Prelude.Maybe (Prelude.NonEmpty OpsFilter),
    -- | Optional aggregators that return counts of OpsItems based on one or more
    -- expressions.
    aggregators :: Prelude.Maybe (Prelude.NonEmpty OpsAggregator)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOpsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getOpsSummary_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'getOpsSummary_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'syncName', 'getOpsSummary_syncName' - Specify the name of a resource data sync to get.
--
-- 'resultAttributes', 'getOpsSummary_resultAttributes' - The OpsItem data type to return.
--
-- 'filters', 'getOpsSummary_filters' - Optional filters used to scope down the returned OpsItems.
--
-- 'aggregators', 'getOpsSummary_aggregators' - Optional aggregators that return counts of OpsItems based on one or more
-- expressions.
newGetOpsSummary ::
  GetOpsSummary
newGetOpsSummary =
  GetOpsSummary'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      syncName = Prelude.Nothing,
      resultAttributes = Prelude.Nothing,
      filters = Prelude.Nothing,
      aggregators = Prelude.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
getOpsSummary_nextToken :: Lens.Lens' GetOpsSummary (Prelude.Maybe Prelude.Text)
getOpsSummary_nextToken = Lens.lens (\GetOpsSummary' {nextToken} -> nextToken) (\s@GetOpsSummary' {} a -> s {nextToken = a} :: GetOpsSummary)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
getOpsSummary_maxResults :: Lens.Lens' GetOpsSummary (Prelude.Maybe Prelude.Natural)
getOpsSummary_maxResults = Lens.lens (\GetOpsSummary' {maxResults} -> maxResults) (\s@GetOpsSummary' {} a -> s {maxResults = a} :: GetOpsSummary)

-- | Specify the name of a resource data sync to get.
getOpsSummary_syncName :: Lens.Lens' GetOpsSummary (Prelude.Maybe Prelude.Text)
getOpsSummary_syncName = Lens.lens (\GetOpsSummary' {syncName} -> syncName) (\s@GetOpsSummary' {} a -> s {syncName = a} :: GetOpsSummary)

-- | The OpsItem data type to return.
getOpsSummary_resultAttributes :: Lens.Lens' GetOpsSummary (Prelude.Maybe (Prelude.NonEmpty OpsResultAttribute))
getOpsSummary_resultAttributes = Lens.lens (\GetOpsSummary' {resultAttributes} -> resultAttributes) (\s@GetOpsSummary' {} a -> s {resultAttributes = a} :: GetOpsSummary) Prelude.. Lens.mapping Lens._Coerce

-- | Optional filters used to scope down the returned OpsItems.
getOpsSummary_filters :: Lens.Lens' GetOpsSummary (Prelude.Maybe (Prelude.NonEmpty OpsFilter))
getOpsSummary_filters = Lens.lens (\GetOpsSummary' {filters} -> filters) (\s@GetOpsSummary' {} a -> s {filters = a} :: GetOpsSummary) Prelude.. Lens.mapping Lens._Coerce

-- | Optional aggregators that return counts of OpsItems based on one or more
-- expressions.
getOpsSummary_aggregators :: Lens.Lens' GetOpsSummary (Prelude.Maybe (Prelude.NonEmpty OpsAggregator))
getOpsSummary_aggregators = Lens.lens (\GetOpsSummary' {aggregators} -> aggregators) (\s@GetOpsSummary' {} a -> s {aggregators = a} :: GetOpsSummary) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager GetOpsSummary where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getOpsSummaryResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getOpsSummaryResponse_entities Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getOpsSummary_nextToken
          Lens..~ rs
          Lens.^? getOpsSummaryResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetOpsSummary where
  type
    AWSResponse GetOpsSummary =
      GetOpsSummaryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOpsSummaryResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Entities" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOpsSummary

instance Prelude.NFData GetOpsSummary

instance Core.ToHeaders GetOpsSummary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.GetOpsSummary" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetOpsSummary where
  toJSON GetOpsSummary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SyncName" Core..=) Prelude.<$> syncName,
            ("ResultAttributes" Core..=)
              Prelude.<$> resultAttributes,
            ("Filters" Core..=) Prelude.<$> filters,
            ("Aggregators" Core..=) Prelude.<$> aggregators
          ]
      )

instance Core.ToPath GetOpsSummary where
  toPath = Prelude.const "/"

instance Core.ToQuery GetOpsSummary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOpsSummaryResponse' smart constructor.
data GetOpsSummaryResponse = GetOpsSummaryResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of aggregated and filtered OpsItems.
    entities :: Prelude.Maybe [OpsEntity],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOpsSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getOpsSummaryResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'entities', 'getOpsSummaryResponse_entities' - The list of aggregated and filtered OpsItems.
--
-- 'httpStatus', 'getOpsSummaryResponse_httpStatus' - The response's http status code.
newGetOpsSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOpsSummaryResponse
newGetOpsSummaryResponse pHttpStatus_ =
  GetOpsSummaryResponse'
    { nextToken = Prelude.Nothing,
      entities = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
getOpsSummaryResponse_nextToken :: Lens.Lens' GetOpsSummaryResponse (Prelude.Maybe Prelude.Text)
getOpsSummaryResponse_nextToken = Lens.lens (\GetOpsSummaryResponse' {nextToken} -> nextToken) (\s@GetOpsSummaryResponse' {} a -> s {nextToken = a} :: GetOpsSummaryResponse)

-- | The list of aggregated and filtered OpsItems.
getOpsSummaryResponse_entities :: Lens.Lens' GetOpsSummaryResponse (Prelude.Maybe [OpsEntity])
getOpsSummaryResponse_entities = Lens.lens (\GetOpsSummaryResponse' {entities} -> entities) (\s@GetOpsSummaryResponse' {} a -> s {entities = a} :: GetOpsSummaryResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getOpsSummaryResponse_httpStatus :: Lens.Lens' GetOpsSummaryResponse Prelude.Int
getOpsSummaryResponse_httpStatus = Lens.lens (\GetOpsSummaryResponse' {httpStatus} -> httpStatus) (\s@GetOpsSummaryResponse' {} a -> s {httpStatus = a} :: GetOpsSummaryResponse)

instance Prelude.NFData GetOpsSummaryResponse
