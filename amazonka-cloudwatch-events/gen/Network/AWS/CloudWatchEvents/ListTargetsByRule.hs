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
-- Module      : Network.AWS.CloudWatchEvents.ListTargetsByRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the targets assigned to the specified rule.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchEvents.ListTargetsByRule
  ( -- * Creating a Request
    ListTargetsByRule (..),
    newListTargetsByRule,

    -- * Request Lenses
    listTargetsByRule_nextToken,
    listTargetsByRule_eventBusName,
    listTargetsByRule_limit,
    listTargetsByRule_rule,

    -- * Destructuring the Response
    ListTargetsByRuleResponse (..),
    newListTargetsByRuleResponse,

    -- * Response Lenses
    listTargetsByRuleResponse_nextToken,
    listTargetsByRuleResponse_targets,
    listTargetsByRuleResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTargetsByRule' smart constructor.
data ListTargetsByRule = ListTargetsByRule'
  { -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the event bus associated with the rule. If you omit
    -- this, the default event bus is used.
    eventBusName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the rule.
    rule :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTargetsByRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTargetsByRule_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'eventBusName', 'listTargetsByRule_eventBusName' - The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
--
-- 'limit', 'listTargetsByRule_limit' - The maximum number of results to return.
--
-- 'rule', 'listTargetsByRule_rule' - The name of the rule.
newListTargetsByRule ::
  -- | 'rule'
  Prelude.Text ->
  ListTargetsByRule
newListTargetsByRule pRule_ =
  ListTargetsByRule'
    { nextToken = Prelude.Nothing,
      eventBusName = Prelude.Nothing,
      limit = Prelude.Nothing,
      rule = pRule_
    }

-- | The token returned by a previous call to retrieve the next set of
-- results.
listTargetsByRule_nextToken :: Lens.Lens' ListTargetsByRule (Prelude.Maybe Prelude.Text)
listTargetsByRule_nextToken = Lens.lens (\ListTargetsByRule' {nextToken} -> nextToken) (\s@ListTargetsByRule' {} a -> s {nextToken = a} :: ListTargetsByRule)

-- | The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
listTargetsByRule_eventBusName :: Lens.Lens' ListTargetsByRule (Prelude.Maybe Prelude.Text)
listTargetsByRule_eventBusName = Lens.lens (\ListTargetsByRule' {eventBusName} -> eventBusName) (\s@ListTargetsByRule' {} a -> s {eventBusName = a} :: ListTargetsByRule)

-- | The maximum number of results to return.
listTargetsByRule_limit :: Lens.Lens' ListTargetsByRule (Prelude.Maybe Prelude.Natural)
listTargetsByRule_limit = Lens.lens (\ListTargetsByRule' {limit} -> limit) (\s@ListTargetsByRule' {} a -> s {limit = a} :: ListTargetsByRule)

-- | The name of the rule.
listTargetsByRule_rule :: Lens.Lens' ListTargetsByRule Prelude.Text
listTargetsByRule_rule = Lens.lens (\ListTargetsByRule' {rule} -> rule) (\s@ListTargetsByRule' {} a -> s {rule = a} :: ListTargetsByRule)

instance Core.AWSPager ListTargetsByRule where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTargetsByRuleResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTargetsByRuleResponse_targets
              Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTargetsByRule_nextToken
          Lens..~ rs
          Lens.^? listTargetsByRuleResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListTargetsByRule where
  type
    AWSResponse ListTargetsByRule =
      ListTargetsByRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTargetsByRuleResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Targets")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTargetsByRule

instance Prelude.NFData ListTargetsByRule

instance Core.ToHeaders ListTargetsByRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSEvents.ListTargetsByRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTargetsByRule where
  toJSON ListTargetsByRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("EventBusName" Core..=) Prelude.<$> eventBusName,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just ("Rule" Core..= rule)
          ]
      )

instance Core.ToPath ListTargetsByRule where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTargetsByRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTargetsByRuleResponse' smart constructor.
data ListTargetsByRuleResponse = ListTargetsByRuleResponse'
  { -- | Indicates whether there are additional results to retrieve. If there are
    -- no more results, the value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The targets assigned to the rule.
    targets :: Prelude.Maybe (Prelude.NonEmpty Target),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTargetsByRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTargetsByRuleResponse_nextToken' - Indicates whether there are additional results to retrieve. If there are
-- no more results, the value is null.
--
-- 'targets', 'listTargetsByRuleResponse_targets' - The targets assigned to the rule.
--
-- 'httpStatus', 'listTargetsByRuleResponse_httpStatus' - The response's http status code.
newListTargetsByRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTargetsByRuleResponse
newListTargetsByRuleResponse pHttpStatus_ =
  ListTargetsByRuleResponse'
    { nextToken =
        Prelude.Nothing,
      targets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether there are additional results to retrieve. If there are
-- no more results, the value is null.
listTargetsByRuleResponse_nextToken :: Lens.Lens' ListTargetsByRuleResponse (Prelude.Maybe Prelude.Text)
listTargetsByRuleResponse_nextToken = Lens.lens (\ListTargetsByRuleResponse' {nextToken} -> nextToken) (\s@ListTargetsByRuleResponse' {} a -> s {nextToken = a} :: ListTargetsByRuleResponse)

-- | The targets assigned to the rule.
listTargetsByRuleResponse_targets :: Lens.Lens' ListTargetsByRuleResponse (Prelude.Maybe (Prelude.NonEmpty Target))
listTargetsByRuleResponse_targets = Lens.lens (\ListTargetsByRuleResponse' {targets} -> targets) (\s@ListTargetsByRuleResponse' {} a -> s {targets = a} :: ListTargetsByRuleResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTargetsByRuleResponse_httpStatus :: Lens.Lens' ListTargetsByRuleResponse Prelude.Int
listTargetsByRuleResponse_httpStatus = Lens.lens (\ListTargetsByRuleResponse' {httpStatus} -> httpStatus) (\s@ListTargetsByRuleResponse' {} a -> s {httpStatus = a} :: ListTargetsByRuleResponse)

instance Prelude.NFData ListTargetsByRuleResponse
