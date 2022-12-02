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
-- Module      : Amazonka.Chime.ListSipRules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the SIP rules under the administrator\'s AWS account.
module Amazonka.Chime.ListSipRules
  ( -- * Creating a Request
    ListSipRules (..),
    newListSipRules,

    -- * Request Lenses
    listSipRules_nextToken,
    listSipRules_maxResults,
    listSipRules_sipMediaApplicationId,

    -- * Destructuring the Response
    ListSipRulesResponse (..),
    newListSipRulesResponse,

    -- * Response Lenses
    listSipRulesResponse_nextToken,
    listSipRulesResponse_sipRules,
    listSipRulesResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSipRules' smart constructor.
data ListSipRules = ListSipRules'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call. Defaults to
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The SIP media application ID.
    sipMediaApplicationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSipRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSipRules_nextToken' - The token to use to retrieve the next page of results.
--
-- 'maxResults', 'listSipRules_maxResults' - The maximum number of results to return in a single call. Defaults to
-- 100.
--
-- 'sipMediaApplicationId', 'listSipRules_sipMediaApplicationId' - The SIP media application ID.
newListSipRules ::
  ListSipRules
newListSipRules =
  ListSipRules'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sipMediaApplicationId = Prelude.Nothing
    }

-- | The token to use to retrieve the next page of results.
listSipRules_nextToken :: Lens.Lens' ListSipRules (Prelude.Maybe Prelude.Text)
listSipRules_nextToken = Lens.lens (\ListSipRules' {nextToken} -> nextToken) (\s@ListSipRules' {} a -> s {nextToken = a} :: ListSipRules)

-- | The maximum number of results to return in a single call. Defaults to
-- 100.
listSipRules_maxResults :: Lens.Lens' ListSipRules (Prelude.Maybe Prelude.Natural)
listSipRules_maxResults = Lens.lens (\ListSipRules' {maxResults} -> maxResults) (\s@ListSipRules' {} a -> s {maxResults = a} :: ListSipRules)

-- | The SIP media application ID.
listSipRules_sipMediaApplicationId :: Lens.Lens' ListSipRules (Prelude.Maybe Prelude.Text)
listSipRules_sipMediaApplicationId = Lens.lens (\ListSipRules' {sipMediaApplicationId} -> sipMediaApplicationId) (\s@ListSipRules' {} a -> s {sipMediaApplicationId = a} :: ListSipRules)

instance Core.AWSRequest ListSipRules where
  type AWSResponse ListSipRules = ListSipRulesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSipRulesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SipRules" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSipRules where
  hashWithSalt _salt ListSipRules' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` sipMediaApplicationId

instance Prelude.NFData ListSipRules where
  rnf ListSipRules' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf sipMediaApplicationId

instance Data.ToHeaders ListSipRules where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListSipRules where
  toPath = Prelude.const "/sip-rules"

instance Data.ToQuery ListSipRules where
  toQuery ListSipRules' {..} =
    Prelude.mconcat
      [ "next-token" Data.=: nextToken,
        "max-results" Data.=: maxResults,
        "sip-media-application"
          Data.=: sipMediaApplicationId
      ]

-- | /See:/ 'newListSipRulesResponse' smart constructor.
data ListSipRulesResponse = ListSipRulesResponse'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of SIP rules and rule details.
    sipRules :: Prelude.Maybe [SipRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSipRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSipRulesResponse_nextToken' - The token to use to retrieve the next page of results.
--
-- 'sipRules', 'listSipRulesResponse_sipRules' - List of SIP rules and rule details.
--
-- 'httpStatus', 'listSipRulesResponse_httpStatus' - The response's http status code.
newListSipRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSipRulesResponse
newListSipRulesResponse pHttpStatus_ =
  ListSipRulesResponse'
    { nextToken = Prelude.Nothing,
      sipRules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results.
listSipRulesResponse_nextToken :: Lens.Lens' ListSipRulesResponse (Prelude.Maybe Prelude.Text)
listSipRulesResponse_nextToken = Lens.lens (\ListSipRulesResponse' {nextToken} -> nextToken) (\s@ListSipRulesResponse' {} a -> s {nextToken = a} :: ListSipRulesResponse)

-- | List of SIP rules and rule details.
listSipRulesResponse_sipRules :: Lens.Lens' ListSipRulesResponse (Prelude.Maybe [SipRule])
listSipRulesResponse_sipRules = Lens.lens (\ListSipRulesResponse' {sipRules} -> sipRules) (\s@ListSipRulesResponse' {} a -> s {sipRules = a} :: ListSipRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSipRulesResponse_httpStatus :: Lens.Lens' ListSipRulesResponse Prelude.Int
listSipRulesResponse_httpStatus = Lens.lens (\ListSipRulesResponse' {httpStatus} -> httpStatus) (\s@ListSipRulesResponse' {} a -> s {httpStatus = a} :: ListSipRulesResponse)

instance Prelude.NFData ListSipRulesResponse where
  rnf ListSipRulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sipRules
      `Prelude.seq` Prelude.rnf httpStatus
