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
-- Module      : Amazonka.SecurityHub.ListAutomationRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of automation rules and their metadata for the calling account.
module Amazonka.SecurityHub.ListAutomationRules
  ( -- * Creating a Request
    ListAutomationRules (..),
    newListAutomationRules,

    -- * Request Lenses
    listAutomationRules_maxResults,
    listAutomationRules_nextToken,

    -- * Destructuring the Response
    ListAutomationRulesResponse (..),
    newListAutomationRulesResponse,

    -- * Response Lenses
    listAutomationRulesResponse_automationRulesMetadata,
    listAutomationRulesResponse_nextToken,
    listAutomationRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newListAutomationRules' smart constructor.
data ListAutomationRules = ListAutomationRules'
  { -- | The maximum number of rules to return in the response. This currently
    -- ranges from 1 to 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to specify where to start paginating the response. This is the
    -- @NextToken@ from a previously truncated response. On your first call to
    -- the @ListAutomationRules@ API, set the value of this parameter to
    -- @NULL@.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAutomationRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAutomationRules_maxResults' - The maximum number of rules to return in the response. This currently
-- ranges from 1 to 100.
--
-- 'nextToken', 'listAutomationRules_nextToken' - A token to specify where to start paginating the response. This is the
-- @NextToken@ from a previously truncated response. On your first call to
-- the @ListAutomationRules@ API, set the value of this parameter to
-- @NULL@.
newListAutomationRules ::
  ListAutomationRules
newListAutomationRules =
  ListAutomationRules'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of rules to return in the response. This currently
-- ranges from 1 to 100.
listAutomationRules_maxResults :: Lens.Lens' ListAutomationRules (Prelude.Maybe Prelude.Natural)
listAutomationRules_maxResults = Lens.lens (\ListAutomationRules' {maxResults} -> maxResults) (\s@ListAutomationRules' {} a -> s {maxResults = a} :: ListAutomationRules)

-- | A token to specify where to start paginating the response. This is the
-- @NextToken@ from a previously truncated response. On your first call to
-- the @ListAutomationRules@ API, set the value of this parameter to
-- @NULL@.
listAutomationRules_nextToken :: Lens.Lens' ListAutomationRules (Prelude.Maybe Prelude.Text)
listAutomationRules_nextToken = Lens.lens (\ListAutomationRules' {nextToken} -> nextToken) (\s@ListAutomationRules' {} a -> s {nextToken = a} :: ListAutomationRules)

instance Core.AWSRequest ListAutomationRules where
  type
    AWSResponse ListAutomationRules =
      ListAutomationRulesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAutomationRulesResponse'
            Prelude.<$> ( x
                            Data..?> "AutomationRulesMetadata"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAutomationRules where
  hashWithSalt _salt ListAutomationRules' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListAutomationRules where
  rnf ListAutomationRules' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListAutomationRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAutomationRules where
  toPath = Prelude.const "/automationrules/list"

instance Data.ToQuery ListAutomationRules where
  toQuery ListAutomationRules' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListAutomationRulesResponse' smart constructor.
data ListAutomationRulesResponse = ListAutomationRulesResponse'
  { -- | Metadata for rules in the calling account. The response includes rules
    -- with a @RuleStatus@ of @ENABLED@ and @DISABLED@.
    automationRulesMetadata :: Prelude.Maybe [AutomationRulesMetadata],
    -- | A pagination token for the response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAutomationRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automationRulesMetadata', 'listAutomationRulesResponse_automationRulesMetadata' - Metadata for rules in the calling account. The response includes rules
-- with a @RuleStatus@ of @ENABLED@ and @DISABLED@.
--
-- 'nextToken', 'listAutomationRulesResponse_nextToken' - A pagination token for the response.
--
-- 'httpStatus', 'listAutomationRulesResponse_httpStatus' - The response's http status code.
newListAutomationRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAutomationRulesResponse
newListAutomationRulesResponse pHttpStatus_ =
  ListAutomationRulesResponse'
    { automationRulesMetadata =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metadata for rules in the calling account. The response includes rules
-- with a @RuleStatus@ of @ENABLED@ and @DISABLED@.
listAutomationRulesResponse_automationRulesMetadata :: Lens.Lens' ListAutomationRulesResponse (Prelude.Maybe [AutomationRulesMetadata])
listAutomationRulesResponse_automationRulesMetadata = Lens.lens (\ListAutomationRulesResponse' {automationRulesMetadata} -> automationRulesMetadata) (\s@ListAutomationRulesResponse' {} a -> s {automationRulesMetadata = a} :: ListAutomationRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token for the response.
listAutomationRulesResponse_nextToken :: Lens.Lens' ListAutomationRulesResponse (Prelude.Maybe Prelude.Text)
listAutomationRulesResponse_nextToken = Lens.lens (\ListAutomationRulesResponse' {nextToken} -> nextToken) (\s@ListAutomationRulesResponse' {} a -> s {nextToken = a} :: ListAutomationRulesResponse)

-- | The response's http status code.
listAutomationRulesResponse_httpStatus :: Lens.Lens' ListAutomationRulesResponse Prelude.Int
listAutomationRulesResponse_httpStatus = Lens.lens (\ListAutomationRulesResponse' {httpStatus} -> httpStatus) (\s@ListAutomationRulesResponse' {} a -> s {httpStatus = a} :: ListAutomationRulesResponse)

instance Prelude.NFData ListAutomationRulesResponse where
  rnf ListAutomationRulesResponse' {..} =
    Prelude.rnf automationRulesMetadata
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
