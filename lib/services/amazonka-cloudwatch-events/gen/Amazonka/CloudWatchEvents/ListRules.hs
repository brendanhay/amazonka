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
-- Module      : Amazonka.CloudWatchEvents.ListRules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Amazon EventBridge rules. You can either list all the rules
-- or you can provide a prefix to match to the rule names.
--
-- ListRules does not list the targets of a rule. To see the targets
-- associated with a rule, use
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_ListTargetsByRule.html ListTargetsByRule>.
--
-- This operation returns paginated results.
module Amazonka.CloudWatchEvents.ListRules
  ( -- * Creating a Request
    ListRules (..),
    newListRules,

    -- * Request Lenses
    listRules_nextToken,
    listRules_eventBusName,
    listRules_limit,
    listRules_namePrefix,

    -- * Destructuring the Response
    ListRulesResponse (..),
    newListRulesResponse,

    -- * Response Lenses
    listRulesResponse_nextToken,
    listRulesResponse_rules,
    listRulesResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRules' smart constructor.
data ListRules = ListRules'
  { -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the event bus to list the rules for. If you omit
    -- this, the default event bus is used.
    eventBusName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The prefix matching the rule name.
    namePrefix :: Prelude.Maybe Prelude.Text
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
-- 'nextToken', 'listRules_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'eventBusName', 'listRules_eventBusName' - The name or ARN of the event bus to list the rules for. If you omit
-- this, the default event bus is used.
--
-- 'limit', 'listRules_limit' - The maximum number of results to return.
--
-- 'namePrefix', 'listRules_namePrefix' - The prefix matching the rule name.
newListRules ::
  ListRules
newListRules =
  ListRules'
    { nextToken = Prelude.Nothing,
      eventBusName = Prelude.Nothing,
      limit = Prelude.Nothing,
      namePrefix = Prelude.Nothing
    }

-- | The token returned by a previous call to retrieve the next set of
-- results.
listRules_nextToken :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Text)
listRules_nextToken = Lens.lens (\ListRules' {nextToken} -> nextToken) (\s@ListRules' {} a -> s {nextToken = a} :: ListRules)

-- | The name or ARN of the event bus to list the rules for. If you omit
-- this, the default event bus is used.
listRules_eventBusName :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Text)
listRules_eventBusName = Lens.lens (\ListRules' {eventBusName} -> eventBusName) (\s@ListRules' {} a -> s {eventBusName = a} :: ListRules)

-- | The maximum number of results to return.
listRules_limit :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Natural)
listRules_limit = Lens.lens (\ListRules' {limit} -> limit) (\s@ListRules' {} a -> s {limit = a} :: ListRules)

-- | The prefix matching the rule name.
listRules_namePrefix :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Text)
listRules_namePrefix = Lens.lens (\ListRules' {namePrefix} -> namePrefix) (\s@ListRules' {} a -> s {namePrefix = a} :: ListRules)

instance Core.AWSPager ListRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRulesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRulesResponse_rules Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRules_nextToken
          Lens..~ rs
          Lens.^? listRulesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListRules where
  type AWSResponse ListRules = ListRulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRulesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Rules" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRules where
  hashWithSalt _salt ListRules' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` eventBusName
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` namePrefix

instance Prelude.NFData ListRules where
  rnf ListRules' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf eventBusName
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf namePrefix

instance Data.ToHeaders ListRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.ListRules" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRules where
  toJSON ListRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("EventBusName" Data..=) Prelude.<$> eventBusName,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NamePrefix" Data..=) Prelude.<$> namePrefix
          ]
      )

instance Data.ToPath ListRules where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRulesResponse' smart constructor.
data ListRulesResponse = ListRulesResponse'
  { -- | Indicates whether there are additional results to retrieve. If there are
    -- no more results, the value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The rules that match the specified criteria.
    rules :: Prelude.Maybe [Rule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'nextToken', 'listRulesResponse_nextToken' - Indicates whether there are additional results to retrieve. If there are
-- no more results, the value is null.
--
-- 'rules', 'listRulesResponse_rules' - The rules that match the specified criteria.
--
-- 'httpStatus', 'listRulesResponse_httpStatus' - The response's http status code.
newListRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRulesResponse
newListRulesResponse pHttpStatus_ =
  ListRulesResponse'
    { nextToken = Prelude.Nothing,
      rules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether there are additional results to retrieve. If there are
-- no more results, the value is null.
listRulesResponse_nextToken :: Lens.Lens' ListRulesResponse (Prelude.Maybe Prelude.Text)
listRulesResponse_nextToken = Lens.lens (\ListRulesResponse' {nextToken} -> nextToken) (\s@ListRulesResponse' {} a -> s {nextToken = a} :: ListRulesResponse)

-- | The rules that match the specified criteria.
listRulesResponse_rules :: Lens.Lens' ListRulesResponse (Prelude.Maybe [Rule])
listRulesResponse_rules = Lens.lens (\ListRulesResponse' {rules} -> rules) (\s@ListRulesResponse' {} a -> s {rules = a} :: ListRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRulesResponse_httpStatus :: Lens.Lens' ListRulesResponse Prelude.Int
listRulesResponse_httpStatus = Lens.lens (\ListRulesResponse' {httpStatus} -> httpStatus) (\s@ListRulesResponse' {} a -> s {httpStatus = a} :: ListRulesResponse)

instance Prelude.NFData ListRulesResponse where
  rnf ListRulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf httpStatus
