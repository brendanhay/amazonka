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
-- Module      : Network.AWS.CloudWatchEvents.ListRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Amazon EventBridge rules. You can either list all the rules
-- or you can provide a prefix to match to the rule names.
--
-- ListRules does not list the targets of a rule. To see the targets
-- associated with a rule, use ListTargetsByRule.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchEvents.ListRules
  ( -- * Creating a Request
    ListRules (..),
    newListRules,

    -- * Request Lenses
    listRules_nextToken,
    listRules_eventBusName,
    listRules_namePrefix,
    listRules_limit,

    -- * Destructuring the Response
    ListRulesResponse (..),
    newListRulesResponse,

    -- * Response Lenses
    listRulesResponse_nextToken,
    listRulesResponse_rules,
    listRulesResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRules' smart constructor.
data ListRules = ListRules'
  { -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The name or ARN of the event bus to list the rules for. If you omit
    -- this, the default event bus is used.
    eventBusName :: Core.Maybe Core.Text,
    -- | The prefix matching the rule name.
    namePrefix :: Core.Maybe Core.Text,
    -- | The maximum number of results to return.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRules_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'eventBusName', 'listRules_eventBusName' - The name or ARN of the event bus to list the rules for. If you omit
-- this, the default event bus is used.
--
-- 'namePrefix', 'listRules_namePrefix' - The prefix matching the rule name.
--
-- 'limit', 'listRules_limit' - The maximum number of results to return.
newListRules ::
  ListRules
newListRules =
  ListRules'
    { nextToken = Core.Nothing,
      eventBusName = Core.Nothing,
      namePrefix = Core.Nothing,
      limit = Core.Nothing
    }

-- | The token returned by a previous call to retrieve the next set of
-- results.
listRules_nextToken :: Lens.Lens' ListRules (Core.Maybe Core.Text)
listRules_nextToken = Lens.lens (\ListRules' {nextToken} -> nextToken) (\s@ListRules' {} a -> s {nextToken = a} :: ListRules)

-- | The name or ARN of the event bus to list the rules for. If you omit
-- this, the default event bus is used.
listRules_eventBusName :: Lens.Lens' ListRules (Core.Maybe Core.Text)
listRules_eventBusName = Lens.lens (\ListRules' {eventBusName} -> eventBusName) (\s@ListRules' {} a -> s {eventBusName = a} :: ListRules)

-- | The prefix matching the rule name.
listRules_namePrefix :: Lens.Lens' ListRules (Core.Maybe Core.Text)
listRules_namePrefix = Lens.lens (\ListRules' {namePrefix} -> namePrefix) (\s@ListRules' {} a -> s {namePrefix = a} :: ListRules)

-- | The maximum number of results to return.
listRules_limit :: Lens.Lens' ListRules (Core.Maybe Core.Natural)
listRules_limit = Lens.lens (\ListRules' {limit} -> limit) (\s@ListRules' {} a -> s {limit = a} :: ListRules)

instance Core.AWSPager ListRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRulesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listRulesResponse_rules Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listRules_nextToken
          Lens..~ rs
          Lens.^? listRulesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListRules where
  type AWSResponse ListRules = ListRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRulesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Rules" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListRules

instance Core.NFData ListRules

instance Core.ToHeaders ListRules where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.ListRules" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListRules where
  toJSON ListRules' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("EventBusName" Core..=) Core.<$> eventBusName,
            ("NamePrefix" Core..=) Core.<$> namePrefix,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListRules where
  toPath = Core.const "/"

instance Core.ToQuery ListRules where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListRulesResponse' smart constructor.
data ListRulesResponse = ListRulesResponse'
  { -- | Indicates whether there are additional results to retrieve. If there are
    -- no more results, the value is null.
    nextToken :: Core.Maybe Core.Text,
    -- | The rules that match the specified criteria.
    rules :: Core.Maybe [Rule],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRulesResponse_nextToken' - Indicates whether there are additional results to retrieve. If there are
-- no more results, the value is null.
--
-- 'rules', 'listRulesResponse_rules' - The rules that match the specified criteria.
--
-- 'httpStatus', 'listRulesResponse_httpStatus' - The response's http status code.
newListRulesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListRulesResponse
newListRulesResponse pHttpStatus_ =
  ListRulesResponse'
    { nextToken = Core.Nothing,
      rules = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether there are additional results to retrieve. If there are
-- no more results, the value is null.
listRulesResponse_nextToken :: Lens.Lens' ListRulesResponse (Core.Maybe Core.Text)
listRulesResponse_nextToken = Lens.lens (\ListRulesResponse' {nextToken} -> nextToken) (\s@ListRulesResponse' {} a -> s {nextToken = a} :: ListRulesResponse)

-- | The rules that match the specified criteria.
listRulesResponse_rules :: Lens.Lens' ListRulesResponse (Core.Maybe [Rule])
listRulesResponse_rules = Lens.lens (\ListRulesResponse' {rules} -> rules) (\s@ListRulesResponse' {} a -> s {rules = a} :: ListRulesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listRulesResponse_httpStatus :: Lens.Lens' ListRulesResponse Core.Int
listRulesResponse_httpStatus = Lens.lens (\ListRulesResponse' {httpStatus} -> httpStatus) (\s@ListRulesResponse' {} a -> s {httpStatus = a} :: ListRulesResponse)

instance Core.NFData ListRulesResponse
