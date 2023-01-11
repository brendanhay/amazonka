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
-- Module      : Amazonka.Connect.ListTrafficDistributionGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists traffic distribution groups.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListTrafficDistributionGroups
  ( -- * Creating a Request
    ListTrafficDistributionGroups (..),
    newListTrafficDistributionGroups,

    -- * Request Lenses
    listTrafficDistributionGroups_instanceId,
    listTrafficDistributionGroups_maxResults,
    listTrafficDistributionGroups_nextToken,

    -- * Destructuring the Response
    ListTrafficDistributionGroupsResponse (..),
    newListTrafficDistributionGroupsResponse,

    -- * Response Lenses
    listTrafficDistributionGroupsResponse_nextToken,
    listTrafficDistributionGroupsResponse_trafficDistributionGroupSummaryList,
    listTrafficDistributionGroupsResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTrafficDistributionGroups' smart constructor.
data ListTrafficDistributionGroups = ListTrafficDistributionGroups'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrafficDistributionGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'listTrafficDistributionGroups_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'maxResults', 'listTrafficDistributionGroups_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listTrafficDistributionGroups_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
newListTrafficDistributionGroups ::
  ListTrafficDistributionGroups
newListTrafficDistributionGroups =
  ListTrafficDistributionGroups'
    { instanceId =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
listTrafficDistributionGroups_instanceId :: Lens.Lens' ListTrafficDistributionGroups (Prelude.Maybe Prelude.Text)
listTrafficDistributionGroups_instanceId = Lens.lens (\ListTrafficDistributionGroups' {instanceId} -> instanceId) (\s@ListTrafficDistributionGroups' {} a -> s {instanceId = a} :: ListTrafficDistributionGroups)

-- | The maximum number of results to return per page.
listTrafficDistributionGroups_maxResults :: Lens.Lens' ListTrafficDistributionGroups (Prelude.Maybe Prelude.Natural)
listTrafficDistributionGroups_maxResults = Lens.lens (\ListTrafficDistributionGroups' {maxResults} -> maxResults) (\s@ListTrafficDistributionGroups' {} a -> s {maxResults = a} :: ListTrafficDistributionGroups)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listTrafficDistributionGroups_nextToken :: Lens.Lens' ListTrafficDistributionGroups (Prelude.Maybe Prelude.Text)
listTrafficDistributionGroups_nextToken = Lens.lens (\ListTrafficDistributionGroups' {nextToken} -> nextToken) (\s@ListTrafficDistributionGroups' {} a -> s {nextToken = a} :: ListTrafficDistributionGroups)

instance Core.AWSPager ListTrafficDistributionGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTrafficDistributionGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTrafficDistributionGroupsResponse_trafficDistributionGroupSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTrafficDistributionGroups_nextToken
          Lens..~ rs
          Lens.^? listTrafficDistributionGroupsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListTrafficDistributionGroups
  where
  type
    AWSResponse ListTrafficDistributionGroups =
      ListTrafficDistributionGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrafficDistributionGroupsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "TrafficDistributionGroupSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListTrafficDistributionGroups
  where
  hashWithSalt _salt ListTrafficDistributionGroups' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListTrafficDistributionGroups where
  rnf ListTrafficDistributionGroups' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListTrafficDistributionGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTrafficDistributionGroups where
  toPath = Prelude.const "/traffic-distribution-groups"

instance Data.ToQuery ListTrafficDistributionGroups where
  toQuery ListTrafficDistributionGroups' {..} =
    Prelude.mconcat
      [ "instanceId" Data.=: instanceId,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListTrafficDistributionGroupsResponse' smart constructor.
data ListTrafficDistributionGroupsResponse = ListTrafficDistributionGroupsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of traffic distribution groups.
    trafficDistributionGroupSummaryList :: Prelude.Maybe [TrafficDistributionGroupSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrafficDistributionGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTrafficDistributionGroupsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'trafficDistributionGroupSummaryList', 'listTrafficDistributionGroupsResponse_trafficDistributionGroupSummaryList' - A list of traffic distribution groups.
--
-- 'httpStatus', 'listTrafficDistributionGroupsResponse_httpStatus' - The response's http status code.
newListTrafficDistributionGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTrafficDistributionGroupsResponse
newListTrafficDistributionGroupsResponse pHttpStatus_ =
  ListTrafficDistributionGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      trafficDistributionGroupSummaryList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listTrafficDistributionGroupsResponse_nextToken :: Lens.Lens' ListTrafficDistributionGroupsResponse (Prelude.Maybe Prelude.Text)
listTrafficDistributionGroupsResponse_nextToken = Lens.lens (\ListTrafficDistributionGroupsResponse' {nextToken} -> nextToken) (\s@ListTrafficDistributionGroupsResponse' {} a -> s {nextToken = a} :: ListTrafficDistributionGroupsResponse)

-- | A list of traffic distribution groups.
listTrafficDistributionGroupsResponse_trafficDistributionGroupSummaryList :: Lens.Lens' ListTrafficDistributionGroupsResponse (Prelude.Maybe [TrafficDistributionGroupSummary])
listTrafficDistributionGroupsResponse_trafficDistributionGroupSummaryList = Lens.lens (\ListTrafficDistributionGroupsResponse' {trafficDistributionGroupSummaryList} -> trafficDistributionGroupSummaryList) (\s@ListTrafficDistributionGroupsResponse' {} a -> s {trafficDistributionGroupSummaryList = a} :: ListTrafficDistributionGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTrafficDistributionGroupsResponse_httpStatus :: Lens.Lens' ListTrafficDistributionGroupsResponse Prelude.Int
listTrafficDistributionGroupsResponse_httpStatus = Lens.lens (\ListTrafficDistributionGroupsResponse' {httpStatus} -> httpStatus) (\s@ListTrafficDistributionGroupsResponse' {} a -> s {httpStatus = a} :: ListTrafficDistributionGroupsResponse)

instance
  Prelude.NFData
    ListTrafficDistributionGroupsResponse
  where
  rnf ListTrafficDistributionGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trafficDistributionGroupSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
