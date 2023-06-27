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
-- Module      : Amazonka.Connect.ListRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all rules for the specified Amazon Connect instance.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListRules
  ( -- * Creating a Request
    ListRules (..),
    newListRules,

    -- * Request Lenses
    listRules_eventSourceName,
    listRules_maxResults,
    listRules_nextToken,
    listRules_publishStatus,
    listRules_instanceId,

    -- * Destructuring the Response
    ListRulesResponse (..),
    newListRulesResponse,

    -- * Response Lenses
    listRulesResponse_nextToken,
    listRulesResponse_httpStatus,
    listRulesResponse_ruleSummaryList,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRules' smart constructor.
data ListRules = ListRules'
  { -- | The name of the event source.
    eventSourceName :: Prelude.Maybe EventSourceName,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The publish status of the rule.
    publishStatus :: Prelude.Maybe RulePublishStatus,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSourceName', 'listRules_eventSourceName' - The name of the event source.
--
-- 'maxResults', 'listRules_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listRules_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'publishStatus', 'listRules_publishStatus' - The publish status of the rule.
--
-- 'instanceId', 'listRules_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
newListRules ::
  -- | 'instanceId'
  Prelude.Text ->
  ListRules
newListRules pInstanceId_ =
  ListRules'
    { eventSourceName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      publishStatus = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The name of the event source.
listRules_eventSourceName :: Lens.Lens' ListRules (Prelude.Maybe EventSourceName)
listRules_eventSourceName = Lens.lens (\ListRules' {eventSourceName} -> eventSourceName) (\s@ListRules' {} a -> s {eventSourceName = a} :: ListRules)

-- | The maximum number of results to return per page.
listRules_maxResults :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Natural)
listRules_maxResults = Lens.lens (\ListRules' {maxResults} -> maxResults) (\s@ListRules' {} a -> s {maxResults = a} :: ListRules)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listRules_nextToken :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Text)
listRules_nextToken = Lens.lens (\ListRules' {nextToken} -> nextToken) (\s@ListRules' {} a -> s {nextToken = a} :: ListRules)

-- | The publish status of the rule.
listRules_publishStatus :: Lens.Lens' ListRules (Prelude.Maybe RulePublishStatus)
listRules_publishStatus = Lens.lens (\ListRules' {publishStatus} -> publishStatus) (\s@ListRules' {} a -> s {publishStatus = a} :: ListRules)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
listRules_instanceId :: Lens.Lens' ListRules Prelude.Text
listRules_instanceId = Lens.lens (\ListRules' {instanceId} -> instanceId) (\s@ListRules' {} a -> s {instanceId = a} :: ListRules)

instance Core.AWSPager ListRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRulesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listRulesResponse_ruleSummaryList) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listRules_nextToken
          Lens..~ rs
          Lens.^? listRulesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListRules where
  type AWSResponse ListRules = ListRulesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRulesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "RuleSummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListRules where
  hashWithSalt _salt ListRules' {..} =
    _salt
      `Prelude.hashWithSalt` eventSourceName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` publishStatus
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ListRules where
  rnf ListRules' {..} =
    Prelude.rnf eventSourceName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf publishStatus
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders ListRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRules where
  toPath ListRules' {..} =
    Prelude.mconcat ["/rules/", Data.toBS instanceId]

instance Data.ToQuery ListRules where
  toQuery ListRules' {..} =
    Prelude.mconcat
      [ "eventSourceName" Data.=: eventSourceName,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "publishStatus" Data.=: publishStatus
      ]

-- | /See:/ 'newListRulesResponse' smart constructor.
data ListRulesResponse = ListRulesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Summary information about a rule.
    ruleSummaryList :: [RuleSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRulesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listRulesResponse_httpStatus' - The response's http status code.
--
-- 'ruleSummaryList', 'listRulesResponse_ruleSummaryList' - Summary information about a rule.
newListRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRulesResponse
newListRulesResponse pHttpStatus_ =
  ListRulesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      ruleSummaryList = Prelude.mempty
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listRulesResponse_nextToken :: Lens.Lens' ListRulesResponse (Prelude.Maybe Prelude.Text)
listRulesResponse_nextToken = Lens.lens (\ListRulesResponse' {nextToken} -> nextToken) (\s@ListRulesResponse' {} a -> s {nextToken = a} :: ListRulesResponse)

-- | The response's http status code.
listRulesResponse_httpStatus :: Lens.Lens' ListRulesResponse Prelude.Int
listRulesResponse_httpStatus = Lens.lens (\ListRulesResponse' {httpStatus} -> httpStatus) (\s@ListRulesResponse' {} a -> s {httpStatus = a} :: ListRulesResponse)

-- | Summary information about a rule.
listRulesResponse_ruleSummaryList :: Lens.Lens' ListRulesResponse [RuleSummary]
listRulesResponse_ruleSummaryList = Lens.lens (\ListRulesResponse' {ruleSummaryList} -> ruleSummaryList) (\s@ListRulesResponse' {} a -> s {ruleSummaryList = a} :: ListRulesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListRulesResponse where
  rnf ListRulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf ruleSummaryList
