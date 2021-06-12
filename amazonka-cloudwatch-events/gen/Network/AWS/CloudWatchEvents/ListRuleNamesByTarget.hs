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
-- Module      : Network.AWS.CloudWatchEvents.ListRuleNamesByTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the rules for the specified target. You can see which of the rules
-- in Amazon EventBridge can invoke a specific target in your account.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchEvents.ListRuleNamesByTarget
  ( -- * Creating a Request
    ListRuleNamesByTarget (..),
    newListRuleNamesByTarget,

    -- * Request Lenses
    listRuleNamesByTarget_nextToken,
    listRuleNamesByTarget_eventBusName,
    listRuleNamesByTarget_limit,
    listRuleNamesByTarget_targetArn,

    -- * Destructuring the Response
    ListRuleNamesByTargetResponse (..),
    newListRuleNamesByTargetResponse,

    -- * Response Lenses
    listRuleNamesByTargetResponse_nextToken,
    listRuleNamesByTargetResponse_ruleNames,
    listRuleNamesByTargetResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRuleNamesByTarget' smart constructor.
data ListRuleNamesByTarget = ListRuleNamesByTarget'
  { -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The name or ARN of the event bus to list rules for. If you omit this,
    -- the default event bus is used.
    eventBusName :: Core.Maybe Core.Text,
    -- | The maximum number of results to return.
    limit :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the target resource.
    targetArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRuleNamesByTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRuleNamesByTarget_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'eventBusName', 'listRuleNamesByTarget_eventBusName' - The name or ARN of the event bus to list rules for. If you omit this,
-- the default event bus is used.
--
-- 'limit', 'listRuleNamesByTarget_limit' - The maximum number of results to return.
--
-- 'targetArn', 'listRuleNamesByTarget_targetArn' - The Amazon Resource Name (ARN) of the target resource.
newListRuleNamesByTarget ::
  -- | 'targetArn'
  Core.Text ->
  ListRuleNamesByTarget
newListRuleNamesByTarget pTargetArn_ =
  ListRuleNamesByTarget'
    { nextToken = Core.Nothing,
      eventBusName = Core.Nothing,
      limit = Core.Nothing,
      targetArn = pTargetArn_
    }

-- | The token returned by a previous call to retrieve the next set of
-- results.
listRuleNamesByTarget_nextToken :: Lens.Lens' ListRuleNamesByTarget (Core.Maybe Core.Text)
listRuleNamesByTarget_nextToken = Lens.lens (\ListRuleNamesByTarget' {nextToken} -> nextToken) (\s@ListRuleNamesByTarget' {} a -> s {nextToken = a} :: ListRuleNamesByTarget)

-- | The name or ARN of the event bus to list rules for. If you omit this,
-- the default event bus is used.
listRuleNamesByTarget_eventBusName :: Lens.Lens' ListRuleNamesByTarget (Core.Maybe Core.Text)
listRuleNamesByTarget_eventBusName = Lens.lens (\ListRuleNamesByTarget' {eventBusName} -> eventBusName) (\s@ListRuleNamesByTarget' {} a -> s {eventBusName = a} :: ListRuleNamesByTarget)

-- | The maximum number of results to return.
listRuleNamesByTarget_limit :: Lens.Lens' ListRuleNamesByTarget (Core.Maybe Core.Natural)
listRuleNamesByTarget_limit = Lens.lens (\ListRuleNamesByTarget' {limit} -> limit) (\s@ListRuleNamesByTarget' {} a -> s {limit = a} :: ListRuleNamesByTarget)

-- | The Amazon Resource Name (ARN) of the target resource.
listRuleNamesByTarget_targetArn :: Lens.Lens' ListRuleNamesByTarget Core.Text
listRuleNamesByTarget_targetArn = Lens.lens (\ListRuleNamesByTarget' {targetArn} -> targetArn) (\s@ListRuleNamesByTarget' {} a -> s {targetArn = a} :: ListRuleNamesByTarget)

instance Core.AWSPager ListRuleNamesByTarget where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRuleNamesByTargetResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listRuleNamesByTargetResponse_ruleNames
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listRuleNamesByTarget_nextToken
          Lens..~ rs
          Lens.^? listRuleNamesByTargetResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListRuleNamesByTarget where
  type
    AWSResponse ListRuleNamesByTarget =
      ListRuleNamesByTargetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRuleNamesByTargetResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "RuleNames" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListRuleNamesByTarget

instance Core.NFData ListRuleNamesByTarget

instance Core.ToHeaders ListRuleNamesByTarget where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSEvents.ListRuleNamesByTarget" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListRuleNamesByTarget where
  toJSON ListRuleNamesByTarget' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("EventBusName" Core..=) Core.<$> eventBusName,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("TargetArn" Core..= targetArn)
          ]
      )

instance Core.ToPath ListRuleNamesByTarget where
  toPath = Core.const "/"

instance Core.ToQuery ListRuleNamesByTarget where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListRuleNamesByTargetResponse' smart constructor.
data ListRuleNamesByTargetResponse = ListRuleNamesByTargetResponse'
  { -- | Indicates whether there are additional results to retrieve. If there are
    -- no more results, the value is null.
    nextToken :: Core.Maybe Core.Text,
    -- | The names of the rules that can invoke the given target.
    ruleNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRuleNamesByTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRuleNamesByTargetResponse_nextToken' - Indicates whether there are additional results to retrieve. If there are
-- no more results, the value is null.
--
-- 'ruleNames', 'listRuleNamesByTargetResponse_ruleNames' - The names of the rules that can invoke the given target.
--
-- 'httpStatus', 'listRuleNamesByTargetResponse_httpStatus' - The response's http status code.
newListRuleNamesByTargetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListRuleNamesByTargetResponse
newListRuleNamesByTargetResponse pHttpStatus_ =
  ListRuleNamesByTargetResponse'
    { nextToken =
        Core.Nothing,
      ruleNames = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether there are additional results to retrieve. If there are
-- no more results, the value is null.
listRuleNamesByTargetResponse_nextToken :: Lens.Lens' ListRuleNamesByTargetResponse (Core.Maybe Core.Text)
listRuleNamesByTargetResponse_nextToken = Lens.lens (\ListRuleNamesByTargetResponse' {nextToken} -> nextToken) (\s@ListRuleNamesByTargetResponse' {} a -> s {nextToken = a} :: ListRuleNamesByTargetResponse)

-- | The names of the rules that can invoke the given target.
listRuleNamesByTargetResponse_ruleNames :: Lens.Lens' ListRuleNamesByTargetResponse (Core.Maybe [Core.Text])
listRuleNamesByTargetResponse_ruleNames = Lens.lens (\ListRuleNamesByTargetResponse' {ruleNames} -> ruleNames) (\s@ListRuleNamesByTargetResponse' {} a -> s {ruleNames = a} :: ListRuleNamesByTargetResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listRuleNamesByTargetResponse_httpStatus :: Lens.Lens' ListRuleNamesByTargetResponse Core.Int
listRuleNamesByTargetResponse_httpStatus = Lens.lens (\ListRuleNamesByTargetResponse' {httpStatus} -> httpStatus) (\s@ListRuleNamesByTargetResponse' {} a -> s {httpStatus = a} :: ListRuleNamesByTargetResponse)

instance Core.NFData ListRuleNamesByTargetResponse
