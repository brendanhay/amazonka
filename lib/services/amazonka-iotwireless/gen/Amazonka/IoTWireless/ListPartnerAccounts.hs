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
-- Module      : Amazonka.IoTWireless.ListPartnerAccounts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the partner accounts associated with your AWS account.
module Amazonka.IoTWireless.ListPartnerAccounts
  ( -- * Creating a Request
    ListPartnerAccounts (..),
    newListPartnerAccounts,

    -- * Request Lenses
    listPartnerAccounts_maxResults,
    listPartnerAccounts_nextToken,

    -- * Destructuring the Response
    ListPartnerAccountsResponse (..),
    newListPartnerAccountsResponse,

    -- * Response Lenses
    listPartnerAccountsResponse_nextToken,
    listPartnerAccountsResponse_sidewalk,
    listPartnerAccountsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPartnerAccounts' smart constructor.
data ListPartnerAccounts = ListPartnerAccounts'
  { -- | The maximum number of results to return in this operation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPartnerAccounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPartnerAccounts_maxResults' - The maximum number of results to return in this operation.
--
-- 'nextToken', 'listPartnerAccounts_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
newListPartnerAccounts ::
  ListPartnerAccounts
newListPartnerAccounts =
  ListPartnerAccounts'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in this operation.
listPartnerAccounts_maxResults :: Lens.Lens' ListPartnerAccounts (Prelude.Maybe Prelude.Natural)
listPartnerAccounts_maxResults = Lens.lens (\ListPartnerAccounts' {maxResults} -> maxResults) (\s@ListPartnerAccounts' {} a -> s {maxResults = a} :: ListPartnerAccounts)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listPartnerAccounts_nextToken :: Lens.Lens' ListPartnerAccounts (Prelude.Maybe Prelude.Text)
listPartnerAccounts_nextToken = Lens.lens (\ListPartnerAccounts' {nextToken} -> nextToken) (\s@ListPartnerAccounts' {} a -> s {nextToken = a} :: ListPartnerAccounts)

instance Core.AWSRequest ListPartnerAccounts where
  type
    AWSResponse ListPartnerAccounts =
      ListPartnerAccountsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPartnerAccountsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Sidewalk" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPartnerAccounts where
  hashWithSalt _salt ListPartnerAccounts' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListPartnerAccounts where
  rnf ListPartnerAccounts' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListPartnerAccounts where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListPartnerAccounts where
  toPath = Prelude.const "/partner-accounts"

instance Data.ToQuery ListPartnerAccounts where
  toQuery ListPartnerAccounts' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListPartnerAccountsResponse' smart constructor.
data ListPartnerAccountsResponse = ListPartnerAccountsResponse'
  { -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Sidewalk account credentials.
    sidewalk :: Prelude.Maybe [SidewalkAccountInfoWithFingerprint],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPartnerAccountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPartnerAccountsResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'sidewalk', 'listPartnerAccountsResponse_sidewalk' - The Sidewalk account credentials.
--
-- 'httpStatus', 'listPartnerAccountsResponse_httpStatus' - The response's http status code.
newListPartnerAccountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPartnerAccountsResponse
newListPartnerAccountsResponse pHttpStatus_ =
  ListPartnerAccountsResponse'
    { nextToken =
        Prelude.Nothing,
      sidewalk = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listPartnerAccountsResponse_nextToken :: Lens.Lens' ListPartnerAccountsResponse (Prelude.Maybe Prelude.Text)
listPartnerAccountsResponse_nextToken = Lens.lens (\ListPartnerAccountsResponse' {nextToken} -> nextToken) (\s@ListPartnerAccountsResponse' {} a -> s {nextToken = a} :: ListPartnerAccountsResponse)

-- | The Sidewalk account credentials.
listPartnerAccountsResponse_sidewalk :: Lens.Lens' ListPartnerAccountsResponse (Prelude.Maybe [SidewalkAccountInfoWithFingerprint])
listPartnerAccountsResponse_sidewalk = Lens.lens (\ListPartnerAccountsResponse' {sidewalk} -> sidewalk) (\s@ListPartnerAccountsResponse' {} a -> s {sidewalk = a} :: ListPartnerAccountsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPartnerAccountsResponse_httpStatus :: Lens.Lens' ListPartnerAccountsResponse Prelude.Int
listPartnerAccountsResponse_httpStatus = Lens.lens (\ListPartnerAccountsResponse' {httpStatus} -> httpStatus) (\s@ListPartnerAccountsResponse' {} a -> s {httpStatus = a} :: ListPartnerAccountsResponse)

instance Prelude.NFData ListPartnerAccountsResponse where
  rnf ListPartnerAccountsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sidewalk
      `Prelude.seq` Prelude.rnf httpStatus
