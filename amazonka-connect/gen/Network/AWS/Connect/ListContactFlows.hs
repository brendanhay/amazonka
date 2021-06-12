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
-- Module      : Network.AWS.Connect.ListContactFlows
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the contact flows for the specified Amazon
-- Connect instance.
--
-- You can also create and update contact flows using the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language>.
--
-- For more information about contact flows, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-contact-flows.html Contact Flows>
-- in the /Amazon Connect Administrator Guide/.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListContactFlows
  ( -- * Creating a Request
    ListContactFlows (..),
    newListContactFlows,

    -- * Request Lenses
    listContactFlows_nextToken,
    listContactFlows_contactFlowTypes,
    listContactFlows_maxResults,
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

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListContactFlows' smart constructor.
data ListContactFlows = ListContactFlows'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The type of contact flow.
    contactFlowTypes :: Core.Maybe [ContactFlowType],
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'contactFlowTypes', 'listContactFlows_contactFlowTypes' - The type of contact flow.
--
-- 'maxResults', 'listContactFlows_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listContactFlows_instanceId' - The identifier of the Amazon Connect instance.
newListContactFlows ::
  -- | 'instanceId'
  Core.Text ->
  ListContactFlows
newListContactFlows pInstanceId_ =
  ListContactFlows'
    { nextToken = Core.Nothing,
      contactFlowTypes = Core.Nothing,
      maxResults = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listContactFlows_nextToken :: Lens.Lens' ListContactFlows (Core.Maybe Core.Text)
listContactFlows_nextToken = Lens.lens (\ListContactFlows' {nextToken} -> nextToken) (\s@ListContactFlows' {} a -> s {nextToken = a} :: ListContactFlows)

-- | The type of contact flow.
listContactFlows_contactFlowTypes :: Lens.Lens' ListContactFlows (Core.Maybe [ContactFlowType])
listContactFlows_contactFlowTypes = Lens.lens (\ListContactFlows' {contactFlowTypes} -> contactFlowTypes) (\s@ListContactFlows' {} a -> s {contactFlowTypes = a} :: ListContactFlows) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to return per page.
listContactFlows_maxResults :: Lens.Lens' ListContactFlows (Core.Maybe Core.Natural)
listContactFlows_maxResults = Lens.lens (\ListContactFlows' {maxResults} -> maxResults) (\s@ListContactFlows' {} a -> s {maxResults = a} :: ListContactFlows)

-- | The identifier of the Amazon Connect instance.
listContactFlows_instanceId :: Lens.Lens' ListContactFlows Core.Text
listContactFlows_instanceId = Lens.lens (\ListContactFlows' {instanceId} -> instanceId) (\s@ListContactFlows' {} a -> s {instanceId = a} :: ListContactFlows)

instance Core.AWSPager ListContactFlows where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listContactFlowsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listContactFlowsResponse_contactFlowSummaryList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listContactFlows_nextToken
          Lens..~ rs
          Lens.^? listContactFlowsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListContactFlows where
  type
    AWSResponse ListContactFlows =
      ListContactFlowsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContactFlowsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ContactFlowSummaryList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListContactFlows

instance Core.NFData ListContactFlows

instance Core.ToHeaders ListContactFlows where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListContactFlows where
  toPath ListContactFlows' {..} =
    Core.mconcat
      ["/contact-flows-summary/", Core.toBS instanceId]

instance Core.ToQuery ListContactFlows where
  toQuery ListContactFlows' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "contactFlowTypes"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> contactFlowTypes
            ),
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListContactFlowsResponse' smart constructor.
data ListContactFlowsResponse = ListContactFlowsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the contact flows.
    contactFlowSummaryList :: Core.Maybe [ContactFlowSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'contactFlowSummaryList', 'listContactFlowsResponse_contactFlowSummaryList' - Information about the contact flows.
--
-- 'httpStatus', 'listContactFlowsResponse_httpStatus' - The response's http status code.
newListContactFlowsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListContactFlowsResponse
newListContactFlowsResponse pHttpStatus_ =
  ListContactFlowsResponse'
    { nextToken = Core.Nothing,
      contactFlowSummaryList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listContactFlowsResponse_nextToken :: Lens.Lens' ListContactFlowsResponse (Core.Maybe Core.Text)
listContactFlowsResponse_nextToken = Lens.lens (\ListContactFlowsResponse' {nextToken} -> nextToken) (\s@ListContactFlowsResponse' {} a -> s {nextToken = a} :: ListContactFlowsResponse)

-- | Information about the contact flows.
listContactFlowsResponse_contactFlowSummaryList :: Lens.Lens' ListContactFlowsResponse (Core.Maybe [ContactFlowSummary])
listContactFlowsResponse_contactFlowSummaryList = Lens.lens (\ListContactFlowsResponse' {contactFlowSummaryList} -> contactFlowSummaryList) (\s@ListContactFlowsResponse' {} a -> s {contactFlowSummaryList = a} :: ListContactFlowsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listContactFlowsResponse_httpStatus :: Lens.Lens' ListContactFlowsResponse Core.Int
listContactFlowsResponse_httpStatus = Lens.lens (\ListContactFlowsResponse' {httpStatus} -> httpStatus) (\s@ListContactFlowsResponse' {} a -> s {httpStatus = a} :: ListContactFlowsResponse)

instance Core.NFData ListContactFlowsResponse
