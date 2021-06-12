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
-- Module      : Network.AWS.Connect.ListPrompts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the prompts for the specified Amazon Connect
-- instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListPrompts
  ( -- * Creating a Request
    ListPrompts (..),
    newListPrompts,

    -- * Request Lenses
    listPrompts_nextToken,
    listPrompts_maxResults,
    listPrompts_instanceId,

    -- * Destructuring the Response
    ListPromptsResponse (..),
    newListPromptsResponse,

    -- * Response Lenses
    listPromptsResponse_nextToken,
    listPromptsResponse_promptSummaryList,
    listPromptsResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPrompts' smart constructor.
data ListPrompts = ListPrompts'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPrompts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPrompts_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listPrompts_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listPrompts_instanceId' - The identifier of the Amazon Connect instance.
newListPrompts ::
  -- | 'instanceId'
  Core.Text ->
  ListPrompts
newListPrompts pInstanceId_ =
  ListPrompts'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listPrompts_nextToken :: Lens.Lens' ListPrompts (Core.Maybe Core.Text)
listPrompts_nextToken = Lens.lens (\ListPrompts' {nextToken} -> nextToken) (\s@ListPrompts' {} a -> s {nextToken = a} :: ListPrompts)

-- | The maximum number of results to return per page.
listPrompts_maxResults :: Lens.Lens' ListPrompts (Core.Maybe Core.Natural)
listPrompts_maxResults = Lens.lens (\ListPrompts' {maxResults} -> maxResults) (\s@ListPrompts' {} a -> s {maxResults = a} :: ListPrompts)

-- | The identifier of the Amazon Connect instance.
listPrompts_instanceId :: Lens.Lens' ListPrompts Core.Text
listPrompts_instanceId = Lens.lens (\ListPrompts' {instanceId} -> instanceId) (\s@ListPrompts' {} a -> s {instanceId = a} :: ListPrompts)

instance Core.AWSPager ListPrompts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPromptsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPromptsResponse_promptSummaryList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPrompts_nextToken
          Lens..~ rs
          Lens.^? listPromptsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListPrompts where
  type AWSResponse ListPrompts = ListPromptsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPromptsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "PromptSummaryList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPrompts

instance Core.NFData ListPrompts

instance Core.ToHeaders ListPrompts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListPrompts where
  toPath ListPrompts' {..} =
    Core.mconcat
      ["/prompts-summary/", Core.toBS instanceId]

instance Core.ToQuery ListPrompts where
  toQuery ListPrompts' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListPromptsResponse' smart constructor.
data ListPromptsResponse = ListPromptsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the prompts.
    promptSummaryList :: Core.Maybe [PromptSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPromptsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPromptsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'promptSummaryList', 'listPromptsResponse_promptSummaryList' - Information about the prompts.
--
-- 'httpStatus', 'listPromptsResponse_httpStatus' - The response's http status code.
newListPromptsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPromptsResponse
newListPromptsResponse pHttpStatus_ =
  ListPromptsResponse'
    { nextToken = Core.Nothing,
      promptSummaryList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listPromptsResponse_nextToken :: Lens.Lens' ListPromptsResponse (Core.Maybe Core.Text)
listPromptsResponse_nextToken = Lens.lens (\ListPromptsResponse' {nextToken} -> nextToken) (\s@ListPromptsResponse' {} a -> s {nextToken = a} :: ListPromptsResponse)

-- | Information about the prompts.
listPromptsResponse_promptSummaryList :: Lens.Lens' ListPromptsResponse (Core.Maybe [PromptSummary])
listPromptsResponse_promptSummaryList = Lens.lens (\ListPromptsResponse' {promptSummaryList} -> promptSummaryList) (\s@ListPromptsResponse' {} a -> s {promptSummaryList = a} :: ListPromptsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPromptsResponse_httpStatus :: Lens.Lens' ListPromptsResponse Core.Int
listPromptsResponse_httpStatus = Lens.lens (\ListPromptsResponse' {httpStatus} -> httpStatus) (\s@ListPromptsResponse' {} a -> s {httpStatus = a} :: ListPromptsResponse)

instance Core.NFData ListPromptsResponse
