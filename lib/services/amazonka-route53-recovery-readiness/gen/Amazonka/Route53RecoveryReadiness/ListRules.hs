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
-- Module      : Amazonka.Route53RecoveryReadiness.ListRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of rules that are applied as part of Readiness
-- Checks.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.ListRules
  ( -- * Creating a Request
    ListRules (..),
    newListRules,

    -- * Request Lenses
    listRules_resourceType,
    listRules_nextToken,
    listRules_maxResults,

    -- * Destructuring the Response
    ListRulesResponse (..),
    newListRulesResponse,

    -- * Response Lenses
    listRulesResponse_nextToken,
    listRulesResponse_rules,
    listRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newListRules' smart constructor.
data ListRules = ListRules'
  { -- | Filter parameter which specifies the rules to return given a resource
    -- type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Upper bound on number of records to return.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'resourceType', 'listRules_resourceType' - Filter parameter which specifies the rules to return given a resource
-- type.
--
-- 'nextToken', 'listRules_nextToken' - A token used to resume pagination from the end of a previous request.
--
-- 'maxResults', 'listRules_maxResults' - Upper bound on number of records to return.
newListRules ::
  ListRules
newListRules =
  ListRules'
    { resourceType = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Filter parameter which specifies the rules to return given a resource
-- type.
listRules_resourceType :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Text)
listRules_resourceType = Lens.lens (\ListRules' {resourceType} -> resourceType) (\s@ListRules' {} a -> s {resourceType = a} :: ListRules)

-- | A token used to resume pagination from the end of a previous request.
listRules_nextToken :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Text)
listRules_nextToken = Lens.lens (\ListRules' {nextToken} -> nextToken) (\s@ListRules' {} a -> s {nextToken = a} :: ListRules)

-- | Upper bound on number of records to return.
listRules_maxResults :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Natural)
listRules_maxResults = Lens.lens (\ListRules' {maxResults} -> maxResults) (\s@ListRules' {} a -> s {maxResults = a} :: ListRules)

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRulesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "rules" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRules where
  hashWithSalt _salt ListRules' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListRules where
  rnf ListRules' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListRules where
  toPath = Prelude.const "/rules"

instance Core.ToQuery ListRules where
  toQuery ListRules' {..} =
    Prelude.mconcat
      [ "resourceType" Core.=: resourceType,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListRulesResponse' smart constructor.
data ListRulesResponse = ListRulesResponse'
  { -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of rules
    rules :: Prelude.Maybe [ListRulesOutput],
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
-- 'nextToken', 'listRulesResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'rules', 'listRulesResponse_rules' - A list of rules
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

-- | A token that can be used to resume pagination from the end of the
-- collection.
listRulesResponse_nextToken :: Lens.Lens' ListRulesResponse (Prelude.Maybe Prelude.Text)
listRulesResponse_nextToken = Lens.lens (\ListRulesResponse' {nextToken} -> nextToken) (\s@ListRulesResponse' {} a -> s {nextToken = a} :: ListRulesResponse)

-- | A list of rules
listRulesResponse_rules :: Lens.Lens' ListRulesResponse (Prelude.Maybe [ListRulesOutput])
listRulesResponse_rules = Lens.lens (\ListRulesResponse' {rules} -> rules) (\s@ListRulesResponse' {} a -> s {rules = a} :: ListRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRulesResponse_httpStatus :: Lens.Lens' ListRulesResponse Prelude.Int
listRulesResponse_httpStatus = Lens.lens (\ListRulesResponse' {httpStatus} -> httpStatus) (\s@ListRulesResponse' {} a -> s {httpStatus = a} :: ListRulesResponse)

instance Prelude.NFData ListRulesResponse where
  rnf ListRulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf httpStatus
