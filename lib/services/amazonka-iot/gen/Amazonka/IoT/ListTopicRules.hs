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
-- Module      : Amazonka.IoT.ListTopicRules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the rules for the specific topic.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListTopicRules>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListTopicRules
  ( -- * Creating a Request
    ListTopicRules (..),
    newListTopicRules,

    -- * Request Lenses
    listTopicRules_nextToken,
    listTopicRules_maxResults,
    listTopicRules_topic,
    listTopicRules_ruleDisabled,

    -- * Destructuring the Response
    ListTopicRulesResponse (..),
    newListTopicRulesResponse,

    -- * Response Lenses
    listTopicRulesResponse_nextToken,
    listTopicRulesResponse_rules,
    listTopicRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the ListTopicRules operation.
--
-- /See:/ 'newListTopicRules' smart constructor.
data ListTopicRules = ListTopicRules'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The topic.
    topic :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the rule is disabled.
    ruleDisabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTopicRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTopicRules_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listTopicRules_maxResults' - The maximum number of results to return.
--
-- 'topic', 'listTopicRules_topic' - The topic.
--
-- 'ruleDisabled', 'listTopicRules_ruleDisabled' - Specifies whether the rule is disabled.
newListTopicRules ::
  ListTopicRules
newListTopicRules =
  ListTopicRules'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      topic = Prelude.Nothing,
      ruleDisabled = Prelude.Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listTopicRules_nextToken :: Lens.Lens' ListTopicRules (Prelude.Maybe Prelude.Text)
listTopicRules_nextToken = Lens.lens (\ListTopicRules' {nextToken} -> nextToken) (\s@ListTopicRules' {} a -> s {nextToken = a} :: ListTopicRules)

-- | The maximum number of results to return.
listTopicRules_maxResults :: Lens.Lens' ListTopicRules (Prelude.Maybe Prelude.Natural)
listTopicRules_maxResults = Lens.lens (\ListTopicRules' {maxResults} -> maxResults) (\s@ListTopicRules' {} a -> s {maxResults = a} :: ListTopicRules)

-- | The topic.
listTopicRules_topic :: Lens.Lens' ListTopicRules (Prelude.Maybe Prelude.Text)
listTopicRules_topic = Lens.lens (\ListTopicRules' {topic} -> topic) (\s@ListTopicRules' {} a -> s {topic = a} :: ListTopicRules)

-- | Specifies whether the rule is disabled.
listTopicRules_ruleDisabled :: Lens.Lens' ListTopicRules (Prelude.Maybe Prelude.Bool)
listTopicRules_ruleDisabled = Lens.lens (\ListTopicRules' {ruleDisabled} -> ruleDisabled) (\s@ListTopicRules' {} a -> s {ruleDisabled = a} :: ListTopicRules)

instance Core.AWSPager ListTopicRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTopicRulesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTopicRulesResponse_rules Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTopicRules_nextToken
          Lens..~ rs
          Lens.^? listTopicRulesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListTopicRules where
  type
    AWSResponse ListTopicRules =
      ListTopicRulesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTopicRulesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "rules" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTopicRules where
  hashWithSalt _salt ListTopicRules' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` topic
      `Prelude.hashWithSalt` ruleDisabled

instance Prelude.NFData ListTopicRules where
  rnf ListTopicRules' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf topic
      `Prelude.seq` Prelude.rnf ruleDisabled

instance Data.ToHeaders ListTopicRules where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListTopicRules where
  toPath = Prelude.const "/rules"

instance Data.ToQuery ListTopicRules where
  toQuery ListTopicRules' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults,
        "topic" Data.=: topic,
        "ruleDisabled" Data.=: ruleDisabled
      ]

-- | The output from the ListTopicRules operation.
--
-- /See:/ 'newListTopicRulesResponse' smart constructor.
data ListTopicRulesResponse = ListTopicRulesResponse'
  { -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The rules.
    rules :: Prelude.Maybe [TopicRuleListItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTopicRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTopicRulesResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'rules', 'listTopicRulesResponse_rules' - The rules.
--
-- 'httpStatus', 'listTopicRulesResponse_httpStatus' - The response's http status code.
newListTopicRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTopicRulesResponse
newListTopicRulesResponse pHttpStatus_ =
  ListTopicRulesResponse'
    { nextToken =
        Prelude.Nothing,
      rules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listTopicRulesResponse_nextToken :: Lens.Lens' ListTopicRulesResponse (Prelude.Maybe Prelude.Text)
listTopicRulesResponse_nextToken = Lens.lens (\ListTopicRulesResponse' {nextToken} -> nextToken) (\s@ListTopicRulesResponse' {} a -> s {nextToken = a} :: ListTopicRulesResponse)

-- | The rules.
listTopicRulesResponse_rules :: Lens.Lens' ListTopicRulesResponse (Prelude.Maybe [TopicRuleListItem])
listTopicRulesResponse_rules = Lens.lens (\ListTopicRulesResponse' {rules} -> rules) (\s@ListTopicRulesResponse' {} a -> s {rules = a} :: ListTopicRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTopicRulesResponse_httpStatus :: Lens.Lens' ListTopicRulesResponse Prelude.Int
listTopicRulesResponse_httpStatus = Lens.lens (\ListTopicRulesResponse' {httpStatus} -> httpStatus) (\s@ListTopicRulesResponse' {} a -> s {httpStatus = a} :: ListTopicRulesResponse)

instance Prelude.NFData ListTopicRulesResponse where
  rnf ListTopicRulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf httpStatus
