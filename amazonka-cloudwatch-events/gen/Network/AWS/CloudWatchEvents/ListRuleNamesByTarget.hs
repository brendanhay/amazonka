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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRuleNamesByTarget' smart constructor.
data ListRuleNamesByTarget = ListRuleNamesByTarget'
  { -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the event bus to list rules for. If you omit this,
    -- the default event bus is used.
    eventBusName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the target resource.
    targetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListRuleNamesByTarget
newListRuleNamesByTarget pTargetArn_ =
  ListRuleNamesByTarget'
    { nextToken = Prelude.Nothing,
      eventBusName = Prelude.Nothing,
      limit = Prelude.Nothing,
      targetArn = pTargetArn_
    }

-- | The token returned by a previous call to retrieve the next set of
-- results.
listRuleNamesByTarget_nextToken :: Lens.Lens' ListRuleNamesByTarget (Prelude.Maybe Prelude.Text)
listRuleNamesByTarget_nextToken = Lens.lens (\ListRuleNamesByTarget' {nextToken} -> nextToken) (\s@ListRuleNamesByTarget' {} a -> s {nextToken = a} :: ListRuleNamesByTarget)

-- | The name or ARN of the event bus to list rules for. If you omit this,
-- the default event bus is used.
listRuleNamesByTarget_eventBusName :: Lens.Lens' ListRuleNamesByTarget (Prelude.Maybe Prelude.Text)
listRuleNamesByTarget_eventBusName = Lens.lens (\ListRuleNamesByTarget' {eventBusName} -> eventBusName) (\s@ListRuleNamesByTarget' {} a -> s {eventBusName = a} :: ListRuleNamesByTarget)

-- | The maximum number of results to return.
listRuleNamesByTarget_limit :: Lens.Lens' ListRuleNamesByTarget (Prelude.Maybe Prelude.Natural)
listRuleNamesByTarget_limit = Lens.lens (\ListRuleNamesByTarget' {limit} -> limit) (\s@ListRuleNamesByTarget' {} a -> s {limit = a} :: ListRuleNamesByTarget)

-- | The Amazon Resource Name (ARN) of the target resource.
listRuleNamesByTarget_targetArn :: Lens.Lens' ListRuleNamesByTarget Prelude.Text
listRuleNamesByTarget_targetArn = Lens.lens (\ListRuleNamesByTarget' {targetArn} -> targetArn) (\s@ListRuleNamesByTarget' {} a -> s {targetArn = a} :: ListRuleNamesByTarget)

instance Core.AWSPager ListRuleNamesByTarget where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRuleNamesByTargetResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRuleNamesByTargetResponse_ruleNames
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRuleNamesByTarget_nextToken
          Lens..~ rs
          Lens.^? listRuleNamesByTargetResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListRuleNamesByTarget where
  type
    AWSResponse ListRuleNamesByTarget =
      ListRuleNamesByTargetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRuleNamesByTargetResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "RuleNames" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRuleNamesByTarget

instance Prelude.NFData ListRuleNamesByTarget

instance Core.ToHeaders ListRuleNamesByTarget where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSEvents.ListRuleNamesByTarget" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListRuleNamesByTarget where
  toJSON ListRuleNamesByTarget' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("EventBusName" Core..=) Prelude.<$> eventBusName,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just ("TargetArn" Core..= targetArn)
          ]
      )

instance Core.ToPath ListRuleNamesByTarget where
  toPath = Prelude.const "/"

instance Core.ToQuery ListRuleNamesByTarget where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRuleNamesByTargetResponse' smart constructor.
data ListRuleNamesByTargetResponse = ListRuleNamesByTargetResponse'
  { -- | Indicates whether there are additional results to retrieve. If there are
    -- no more results, the value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The names of the rules that can invoke the given target.
    ruleNames :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListRuleNamesByTargetResponse
newListRuleNamesByTargetResponse pHttpStatus_ =
  ListRuleNamesByTargetResponse'
    { nextToken =
        Prelude.Nothing,
      ruleNames = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether there are additional results to retrieve. If there are
-- no more results, the value is null.
listRuleNamesByTargetResponse_nextToken :: Lens.Lens' ListRuleNamesByTargetResponse (Prelude.Maybe Prelude.Text)
listRuleNamesByTargetResponse_nextToken = Lens.lens (\ListRuleNamesByTargetResponse' {nextToken} -> nextToken) (\s@ListRuleNamesByTargetResponse' {} a -> s {nextToken = a} :: ListRuleNamesByTargetResponse)

-- | The names of the rules that can invoke the given target.
listRuleNamesByTargetResponse_ruleNames :: Lens.Lens' ListRuleNamesByTargetResponse (Prelude.Maybe [Prelude.Text])
listRuleNamesByTargetResponse_ruleNames = Lens.lens (\ListRuleNamesByTargetResponse' {ruleNames} -> ruleNames) (\s@ListRuleNamesByTargetResponse' {} a -> s {ruleNames = a} :: ListRuleNamesByTargetResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listRuleNamesByTargetResponse_httpStatus :: Lens.Lens' ListRuleNamesByTargetResponse Prelude.Int
listRuleNamesByTargetResponse_httpStatus = Lens.lens (\ListRuleNamesByTargetResponse' {httpStatus} -> httpStatus) (\s@ListRuleNamesByTargetResponse' {} a -> s {httpStatus = a} :: ListRuleNamesByTargetResponse)

instance Prelude.NFData ListRuleNamesByTargetResponse
