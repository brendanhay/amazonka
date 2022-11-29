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
-- Module      : Amazonka.Connect.ListContactFlows
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the flows for the specified Amazon Connect
-- instance.
--
-- You can also create and update flows using the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language>.
--
-- For more information about flows, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-contact-flows.html Flows>
-- in the /Amazon Connect Administrator Guide/.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListContactFlows
  ( -- * Creating a Request
    ListContactFlows (..),
    newListContactFlows,

    -- * Request Lenses
    listContactFlows_nextToken,
    listContactFlows_maxResults,
    listContactFlows_contactFlowTypes,
    listContactFlows_instanceId,

    -- * Destructuring the Response
    ListContactFlowsResponse (..),
    newListContactFlowsResponse,

    -- * Response Lenses
    listContactFlowsResponse_nextToken,
    listContactFlowsResponse_contactFlowSummaryList,
    listContactFlowsResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListContactFlows' smart constructor.
data ListContactFlows = ListContactFlows'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page. The default MaxResult
    -- size is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The type of flow.
    contactFlowTypes :: Prelude.Maybe [ContactFlowType],
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContactFlows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContactFlows_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listContactFlows_maxResults' - The maximum number of results to return per page. The default MaxResult
-- size is 100.
--
-- 'contactFlowTypes', 'listContactFlows_contactFlowTypes' - The type of flow.
--
-- 'instanceId', 'listContactFlows_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newListContactFlows ::
  -- | 'instanceId'
  Prelude.Text ->
  ListContactFlows
newListContactFlows pInstanceId_ =
  ListContactFlows'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      contactFlowTypes = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listContactFlows_nextToken :: Lens.Lens' ListContactFlows (Prelude.Maybe Prelude.Text)
listContactFlows_nextToken = Lens.lens (\ListContactFlows' {nextToken} -> nextToken) (\s@ListContactFlows' {} a -> s {nextToken = a} :: ListContactFlows)

-- | The maximum number of results to return per page. The default MaxResult
-- size is 100.
listContactFlows_maxResults :: Lens.Lens' ListContactFlows (Prelude.Maybe Prelude.Natural)
listContactFlows_maxResults = Lens.lens (\ListContactFlows' {maxResults} -> maxResults) (\s@ListContactFlows' {} a -> s {maxResults = a} :: ListContactFlows)

-- | The type of flow.
listContactFlows_contactFlowTypes :: Lens.Lens' ListContactFlows (Prelude.Maybe [ContactFlowType])
listContactFlows_contactFlowTypes = Lens.lens (\ListContactFlows' {contactFlowTypes} -> contactFlowTypes) (\s@ListContactFlows' {} a -> s {contactFlowTypes = a} :: ListContactFlows) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
listContactFlows_instanceId :: Lens.Lens' ListContactFlows Prelude.Text
listContactFlows_instanceId = Lens.lens (\ListContactFlows' {instanceId} -> instanceId) (\s@ListContactFlows' {} a -> s {instanceId = a} :: ListContactFlows)

instance Core.AWSPager ListContactFlows where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listContactFlowsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listContactFlowsResponse_contactFlowSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listContactFlows_nextToken
          Lens..~ rs
          Lens.^? listContactFlowsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListContactFlows where
  type
    AWSResponse ListContactFlows =
      ListContactFlowsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContactFlowsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ContactFlowSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListContactFlows where
  hashWithSalt _salt ListContactFlows' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` contactFlowTypes
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ListContactFlows where
  rnf ListContactFlows' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf contactFlowTypes
      `Prelude.seq` Prelude.rnf instanceId

instance Core.ToHeaders ListContactFlows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListContactFlows where
  toPath ListContactFlows' {..} =
    Prelude.mconcat
      ["/contact-flows-summary/", Core.toBS instanceId]

instance Core.ToQuery ListContactFlows where
  toQuery ListContactFlows' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "contactFlowTypes"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> contactFlowTypes
            )
      ]

-- | /See:/ 'newListContactFlowsResponse' smart constructor.
data ListContactFlowsResponse = ListContactFlowsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the flows.
    contactFlowSummaryList :: Prelude.Maybe [ContactFlowSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContactFlowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContactFlowsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'contactFlowSummaryList', 'listContactFlowsResponse_contactFlowSummaryList' - Information about the flows.
--
-- 'httpStatus', 'listContactFlowsResponse_httpStatus' - The response's http status code.
newListContactFlowsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListContactFlowsResponse
newListContactFlowsResponse pHttpStatus_ =
  ListContactFlowsResponse'
    { nextToken =
        Prelude.Nothing,
      contactFlowSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listContactFlowsResponse_nextToken :: Lens.Lens' ListContactFlowsResponse (Prelude.Maybe Prelude.Text)
listContactFlowsResponse_nextToken = Lens.lens (\ListContactFlowsResponse' {nextToken} -> nextToken) (\s@ListContactFlowsResponse' {} a -> s {nextToken = a} :: ListContactFlowsResponse)

-- | Information about the flows.
listContactFlowsResponse_contactFlowSummaryList :: Lens.Lens' ListContactFlowsResponse (Prelude.Maybe [ContactFlowSummary])
listContactFlowsResponse_contactFlowSummaryList = Lens.lens (\ListContactFlowsResponse' {contactFlowSummaryList} -> contactFlowSummaryList) (\s@ListContactFlowsResponse' {} a -> s {contactFlowSummaryList = a} :: ListContactFlowsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listContactFlowsResponse_httpStatus :: Lens.Lens' ListContactFlowsResponse Prelude.Int
listContactFlowsResponse_httpStatus = Lens.lens (\ListContactFlowsResponse' {httpStatus} -> httpStatus) (\s@ListContactFlowsResponse' {} a -> s {httpStatus = a} :: ListContactFlowsResponse)

instance Prelude.NFData ListContactFlowsResponse where
  rnf ListContactFlowsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf contactFlowSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
