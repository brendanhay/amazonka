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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all readiness rules, or lists the readiness rules for a specific
-- resource type.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.ListRules
  ( -- * Creating a Request
    ListRules (..),
    newListRules,

    -- * Request Lenses
    listRules_maxResults,
    listRules_nextToken,
    listRules_resourceType,

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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newListRules' smart constructor.
data ListRules = ListRules'
  { -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The resource type that a readiness rule applies to.
    resourceType :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listRules_maxResults' - The number of objects that you want to return with this call.
--
-- 'nextToken', 'listRules_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'resourceType', 'listRules_resourceType' - The resource type that a readiness rule applies to.
newListRules ::
  ListRules
newListRules =
  ListRules'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The number of objects that you want to return with this call.
listRules_maxResults :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Natural)
listRules_maxResults = Lens.lens (\ListRules' {maxResults} -> maxResults) (\s@ListRules' {} a -> s {maxResults = a} :: ListRules)

-- | The token that identifies which batch of results you want to see.
listRules_nextToken :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Text)
listRules_nextToken = Lens.lens (\ListRules' {nextToken} -> nextToken) (\s@ListRules' {} a -> s {nextToken = a} :: ListRules)

-- | The resource type that a readiness rule applies to.
listRules_resourceType :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Text)
listRules_resourceType = Lens.lens (\ListRules' {resourceType} -> resourceType) (\s@ListRules' {} a -> s {resourceType = a} :: ListRules)

instance Core.AWSPager ListRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRulesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRulesResponse_rules
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listRules_nextToken
              Lens..~ rs
              Lens.^? listRulesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListRules where
  type AWSResponse ListRules = ListRulesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRulesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "rules" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRules where
  hashWithSalt _salt ListRules' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ListRules where
  rnf ListRules' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf resourceType

instance Data.ToHeaders ListRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRules where
  toPath = Prelude.const "/rules"

instance Data.ToQuery ListRules where
  toQuery ListRules' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "resourceType" Data.=: resourceType
      ]

-- | /See:/ 'newListRulesResponse' smart constructor.
data ListRulesResponse = ListRulesResponse'
  { -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of readiness rules for a specific resource type.
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
-- 'nextToken', 'listRulesResponse_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'rules', 'listRulesResponse_rules' - A list of readiness rules for a specific resource type.
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

-- | The token that identifies which batch of results you want to see.
listRulesResponse_nextToken :: Lens.Lens' ListRulesResponse (Prelude.Maybe Prelude.Text)
listRulesResponse_nextToken = Lens.lens (\ListRulesResponse' {nextToken} -> nextToken) (\s@ListRulesResponse' {} a -> s {nextToken = a} :: ListRulesResponse)

-- | A list of readiness rules for a specific resource type.
listRulesResponse_rules :: Lens.Lens' ListRulesResponse (Prelude.Maybe [ListRulesOutput])
listRulesResponse_rules = Lens.lens (\ListRulesResponse' {rules} -> rules) (\s@ListRulesResponse' {} a -> s {rules = a} :: ListRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRulesResponse_httpStatus :: Lens.Lens' ListRulesResponse Prelude.Int
listRulesResponse_httpStatus = Lens.lens (\ListRulesResponse' {httpStatus} -> httpStatus) (\s@ListRulesResponse' {} a -> s {httpStatus = a} :: ListRulesResponse)

instance Prelude.NFData ListRulesResponse where
  rnf ListRulesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf rules `Prelude.seq`
        Prelude.rnf httpStatus
