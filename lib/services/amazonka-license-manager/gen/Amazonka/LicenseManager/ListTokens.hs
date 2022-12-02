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
-- Module      : Amazonka.LicenseManager.ListTokens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your tokens.
module Amazonka.LicenseManager.ListTokens
  ( -- * Creating a Request
    ListTokens (..),
    newListTokens,

    -- * Request Lenses
    listTokens_nextToken,
    listTokens_filters,
    listTokens_tokenIds,
    listTokens_maxResults,

    -- * Destructuring the Response
    ListTokensResponse (..),
    newListTokensResponse,

    -- * Response Lenses
    listTokensResponse_nextToken,
    listTokensResponse_tokens,
    listTokensResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTokens' smart constructor.
data ListTokens = ListTokens'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters to scope the results. The following filter is supported:
    --
    -- -   @LicenseArns@
    filters :: Prelude.Maybe [Filter],
    -- | Token IDs.
    tokenIds :: Prelude.Maybe [Prelude.Text],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTokens' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTokens_nextToken' - Token for the next set of results.
--
-- 'filters', 'listTokens_filters' - Filters to scope the results. The following filter is supported:
--
-- -   @LicenseArns@
--
-- 'tokenIds', 'listTokens_tokenIds' - Token IDs.
--
-- 'maxResults', 'listTokens_maxResults' - Maximum number of results to return in a single call.
newListTokens ::
  ListTokens
newListTokens =
  ListTokens'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      tokenIds = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Token for the next set of results.
listTokens_nextToken :: Lens.Lens' ListTokens (Prelude.Maybe Prelude.Text)
listTokens_nextToken = Lens.lens (\ListTokens' {nextToken} -> nextToken) (\s@ListTokens' {} a -> s {nextToken = a} :: ListTokens)

-- | Filters to scope the results. The following filter is supported:
--
-- -   @LicenseArns@
listTokens_filters :: Lens.Lens' ListTokens (Prelude.Maybe [Filter])
listTokens_filters = Lens.lens (\ListTokens' {filters} -> filters) (\s@ListTokens' {} a -> s {filters = a} :: ListTokens) Prelude.. Lens.mapping Lens.coerced

-- | Token IDs.
listTokens_tokenIds :: Lens.Lens' ListTokens (Prelude.Maybe [Prelude.Text])
listTokens_tokenIds = Lens.lens (\ListTokens' {tokenIds} -> tokenIds) (\s@ListTokens' {} a -> s {tokenIds = a} :: ListTokens) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listTokens_maxResults :: Lens.Lens' ListTokens (Prelude.Maybe Prelude.Natural)
listTokens_maxResults = Lens.lens (\ListTokens' {maxResults} -> maxResults) (\s@ListTokens' {} a -> s {maxResults = a} :: ListTokens)

instance Core.AWSRequest ListTokens where
  type AWSResponse ListTokens = ListTokensResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTokensResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Tokens" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTokens where
  hashWithSalt _salt ListTokens' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` tokenIds
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListTokens where
  rnf ListTokens' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf tokenIds
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListTokens where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.ListTokens" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTokens where
  toJSON ListTokens' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filters" Data..=) Prelude.<$> filters,
            ("TokenIds" Data..=) Prelude.<$> tokenIds,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListTokens where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTokens where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTokensResponse' smart constructor.
data ListTokensResponse = ListTokensResponse'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Received token details.
    tokens :: Prelude.Maybe [TokenData],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTokensResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTokensResponse_nextToken' - Token for the next set of results.
--
-- 'tokens', 'listTokensResponse_tokens' - Received token details.
--
-- 'httpStatus', 'listTokensResponse_httpStatus' - The response's http status code.
newListTokensResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTokensResponse
newListTokensResponse pHttpStatus_ =
  ListTokensResponse'
    { nextToken = Prelude.Nothing,
      tokens = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token for the next set of results.
listTokensResponse_nextToken :: Lens.Lens' ListTokensResponse (Prelude.Maybe Prelude.Text)
listTokensResponse_nextToken = Lens.lens (\ListTokensResponse' {nextToken} -> nextToken) (\s@ListTokensResponse' {} a -> s {nextToken = a} :: ListTokensResponse)

-- | Received token details.
listTokensResponse_tokens :: Lens.Lens' ListTokensResponse (Prelude.Maybe [TokenData])
listTokensResponse_tokens = Lens.lens (\ListTokensResponse' {tokens} -> tokens) (\s@ListTokensResponse' {} a -> s {tokens = a} :: ListTokensResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTokensResponse_httpStatus :: Lens.Lens' ListTokensResponse Prelude.Int
listTokensResponse_httpStatus = Lens.lens (\ListTokensResponse' {httpStatus} -> httpStatus) (\s@ListTokensResponse' {} a -> s {httpStatus = a} :: ListTokensResponse)

instance Prelude.NFData ListTokensResponse where
  rnf ListTokensResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tokens
      `Prelude.seq` Prelude.rnf httpStatus
