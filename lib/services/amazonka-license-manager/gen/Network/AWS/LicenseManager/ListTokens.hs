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
-- Module      : Network.AWS.LicenseManager.ListTokens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your tokens.
module Network.AWS.LicenseManager.ListTokens
  ( -- * Creating a Request
    ListTokens (..),
    newListTokens,

    -- * Request Lenses
    listTokens_tokenIds,
    listTokens_filters,
    listTokens_nextToken,
    listTokens_maxResults,

    -- * Destructuring the Response
    ListTokensResponse (..),
    newListTokensResponse,

    -- * Response Lenses
    listTokensResponse_tokens,
    listTokensResponse_nextToken,
    listTokensResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LicenseManager.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTokens' smart constructor.
data ListTokens = ListTokens'
  { -- | Token IDs.
    tokenIds :: Prelude.Maybe [Prelude.Text],
    -- | Filters to scope the results. The following filter is supported:
    --
    -- -   @LicenseArns@
    filters :: Prelude.Maybe [Filter],
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'tokenIds', 'listTokens_tokenIds' - Token IDs.
--
-- 'filters', 'listTokens_filters' - Filters to scope the results. The following filter is supported:
--
-- -   @LicenseArns@
--
-- 'nextToken', 'listTokens_nextToken' - Token for the next set of results.
--
-- 'maxResults', 'listTokens_maxResults' - Maximum number of results to return in a single call.
newListTokens ::
  ListTokens
newListTokens =
  ListTokens'
    { tokenIds = Prelude.Nothing,
      filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Token IDs.
listTokens_tokenIds :: Lens.Lens' ListTokens (Prelude.Maybe [Prelude.Text])
listTokens_tokenIds = Lens.lens (\ListTokens' {tokenIds} -> tokenIds) (\s@ListTokens' {} a -> s {tokenIds = a} :: ListTokens) Prelude.. Lens.mapping Lens.coerced

-- | Filters to scope the results. The following filter is supported:
--
-- -   @LicenseArns@
listTokens_filters :: Lens.Lens' ListTokens (Prelude.Maybe [Filter])
listTokens_filters = Lens.lens (\ListTokens' {filters} -> filters) (\s@ListTokens' {} a -> s {filters = a} :: ListTokens) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next set of results.
listTokens_nextToken :: Lens.Lens' ListTokens (Prelude.Maybe Prelude.Text)
listTokens_nextToken = Lens.lens (\ListTokens' {nextToken} -> nextToken) (\s@ListTokens' {} a -> s {nextToken = a} :: ListTokens)

-- | Maximum number of results to return in a single call.
listTokens_maxResults :: Lens.Lens' ListTokens (Prelude.Maybe Prelude.Natural)
listTokens_maxResults = Lens.lens (\ListTokens' {maxResults} -> maxResults) (\s@ListTokens' {} a -> s {maxResults = a} :: ListTokens)

instance Core.AWSRequest ListTokens where
  type AWSResponse ListTokens = ListTokensResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTokensResponse'
            Prelude.<$> (x Core..?> "Tokens" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTokens

instance Prelude.NFData ListTokens

instance Core.ToHeaders ListTokens where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.ListTokens" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTokens where
  toJSON ListTokens' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TokenIds" Core..=) Prelude.<$> tokenIds,
            ("Filters" Core..=) Prelude.<$> filters,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListTokens where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTokens where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTokensResponse' smart constructor.
data ListTokensResponse = ListTokensResponse'
  { -- | Received token details.
    tokens :: Prelude.Maybe [TokenData],
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'tokens', 'listTokensResponse_tokens' - Received token details.
--
-- 'nextToken', 'listTokensResponse_nextToken' - Token for the next set of results.
--
-- 'httpStatus', 'listTokensResponse_httpStatus' - The response's http status code.
newListTokensResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTokensResponse
newListTokensResponse pHttpStatus_ =
  ListTokensResponse'
    { tokens = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Received token details.
listTokensResponse_tokens :: Lens.Lens' ListTokensResponse (Prelude.Maybe [TokenData])
listTokensResponse_tokens = Lens.lens (\ListTokensResponse' {tokens} -> tokens) (\s@ListTokensResponse' {} a -> s {tokens = a} :: ListTokensResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next set of results.
listTokensResponse_nextToken :: Lens.Lens' ListTokensResponse (Prelude.Maybe Prelude.Text)
listTokensResponse_nextToken = Lens.lens (\ListTokensResponse' {nextToken} -> nextToken) (\s@ListTokensResponse' {} a -> s {nextToken = a} :: ListTokensResponse)

-- | The response's http status code.
listTokensResponse_httpStatus :: Lens.Lens' ListTokensResponse Prelude.Int
listTokensResponse_httpStatus = Lens.lens (\ListTokensResponse' {httpStatus} -> httpStatus) (\s@ListTokensResponse' {} a -> s {httpStatus = a} :: ListTokensResponse)

instance Prelude.NFData ListTokensResponse
