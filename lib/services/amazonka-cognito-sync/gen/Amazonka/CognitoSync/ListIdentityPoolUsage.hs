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
-- Module      : Amazonka.CognitoSync.ListIdentityPoolUsage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of identity pools registered with Cognito.
--
-- ListIdentityPoolUsage can only be called with developer credentials. You
-- cannot make this API call with the temporary user credentials provided
-- by Cognito Identity.
module Amazonka.CognitoSync.ListIdentityPoolUsage
  ( -- * Creating a Request
    ListIdentityPoolUsage (..),
    newListIdentityPoolUsage,

    -- * Request Lenses
    listIdentityPoolUsage_maxResults,
    listIdentityPoolUsage_nextToken,

    -- * Destructuring the Response
    ListIdentityPoolUsageResponse (..),
    newListIdentityPoolUsageResponse,

    -- * Response Lenses
    listIdentityPoolUsageResponse_count,
    listIdentityPoolUsageResponse_identityPoolUsages,
    listIdentityPoolUsageResponse_maxResults,
    listIdentityPoolUsageResponse_nextToken,
    listIdentityPoolUsageResponse_httpStatus,
  )
where

import Amazonka.CognitoSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request for usage information on an identity pool.
--
-- /See:/ 'newListIdentityPoolUsage' smart constructor.
data ListIdentityPoolUsage = ListIdentityPoolUsage'
  { -- | The maximum number of results to be returned.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A pagination token for obtaining the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentityPoolUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listIdentityPoolUsage_maxResults' - The maximum number of results to be returned.
--
-- 'nextToken', 'listIdentityPoolUsage_nextToken' - A pagination token for obtaining the next page of results.
newListIdentityPoolUsage ::
  ListIdentityPoolUsage
newListIdentityPoolUsage =
  ListIdentityPoolUsage'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be returned.
listIdentityPoolUsage_maxResults :: Lens.Lens' ListIdentityPoolUsage (Prelude.Maybe Prelude.Int)
listIdentityPoolUsage_maxResults = Lens.lens (\ListIdentityPoolUsage' {maxResults} -> maxResults) (\s@ListIdentityPoolUsage' {} a -> s {maxResults = a} :: ListIdentityPoolUsage)

-- | A pagination token for obtaining the next page of results.
listIdentityPoolUsage_nextToken :: Lens.Lens' ListIdentityPoolUsage (Prelude.Maybe Prelude.Text)
listIdentityPoolUsage_nextToken = Lens.lens (\ListIdentityPoolUsage' {nextToken} -> nextToken) (\s@ListIdentityPoolUsage' {} a -> s {nextToken = a} :: ListIdentityPoolUsage)

instance Core.AWSRequest ListIdentityPoolUsage where
  type
    AWSResponse ListIdentityPoolUsage =
      ListIdentityPoolUsageResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIdentityPoolUsageResponse'
            Prelude.<$> (x Data..?> "Count")
            Prelude.<*> ( x Data..?> "IdentityPoolUsages"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "MaxResults")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIdentityPoolUsage where
  hashWithSalt _salt ListIdentityPoolUsage' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListIdentityPoolUsage where
  rnf ListIdentityPoolUsage' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListIdentityPoolUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListIdentityPoolUsage where
  toPath = Prelude.const "/identitypools"

instance Data.ToQuery ListIdentityPoolUsage where
  toQuery ListIdentityPoolUsage' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | Returned for a successful ListIdentityPoolUsage request.
--
-- /See:/ 'newListIdentityPoolUsageResponse' smart constructor.
data ListIdentityPoolUsageResponse = ListIdentityPoolUsageResponse'
  { -- | Total number of identities for the identity pool.
    count :: Prelude.Maybe Prelude.Int,
    -- | Usage information for the identity pools.
    identityPoolUsages :: Prelude.Maybe [IdentityPoolUsage],
    -- | The maximum number of results to be returned.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A pagination token for obtaining the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentityPoolUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'listIdentityPoolUsageResponse_count' - Total number of identities for the identity pool.
--
-- 'identityPoolUsages', 'listIdentityPoolUsageResponse_identityPoolUsages' - Usage information for the identity pools.
--
-- 'maxResults', 'listIdentityPoolUsageResponse_maxResults' - The maximum number of results to be returned.
--
-- 'nextToken', 'listIdentityPoolUsageResponse_nextToken' - A pagination token for obtaining the next page of results.
--
-- 'httpStatus', 'listIdentityPoolUsageResponse_httpStatus' - The response's http status code.
newListIdentityPoolUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIdentityPoolUsageResponse
newListIdentityPoolUsageResponse pHttpStatus_ =
  ListIdentityPoolUsageResponse'
    { count =
        Prelude.Nothing,
      identityPoolUsages = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Total number of identities for the identity pool.
listIdentityPoolUsageResponse_count :: Lens.Lens' ListIdentityPoolUsageResponse (Prelude.Maybe Prelude.Int)
listIdentityPoolUsageResponse_count = Lens.lens (\ListIdentityPoolUsageResponse' {count} -> count) (\s@ListIdentityPoolUsageResponse' {} a -> s {count = a} :: ListIdentityPoolUsageResponse)

-- | Usage information for the identity pools.
listIdentityPoolUsageResponse_identityPoolUsages :: Lens.Lens' ListIdentityPoolUsageResponse (Prelude.Maybe [IdentityPoolUsage])
listIdentityPoolUsageResponse_identityPoolUsages = Lens.lens (\ListIdentityPoolUsageResponse' {identityPoolUsages} -> identityPoolUsages) (\s@ListIdentityPoolUsageResponse' {} a -> s {identityPoolUsages = a} :: ListIdentityPoolUsageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to be returned.
listIdentityPoolUsageResponse_maxResults :: Lens.Lens' ListIdentityPoolUsageResponse (Prelude.Maybe Prelude.Int)
listIdentityPoolUsageResponse_maxResults = Lens.lens (\ListIdentityPoolUsageResponse' {maxResults} -> maxResults) (\s@ListIdentityPoolUsageResponse' {} a -> s {maxResults = a} :: ListIdentityPoolUsageResponse)

-- | A pagination token for obtaining the next page of results.
listIdentityPoolUsageResponse_nextToken :: Lens.Lens' ListIdentityPoolUsageResponse (Prelude.Maybe Prelude.Text)
listIdentityPoolUsageResponse_nextToken = Lens.lens (\ListIdentityPoolUsageResponse' {nextToken} -> nextToken) (\s@ListIdentityPoolUsageResponse' {} a -> s {nextToken = a} :: ListIdentityPoolUsageResponse)

-- | The response's http status code.
listIdentityPoolUsageResponse_httpStatus :: Lens.Lens' ListIdentityPoolUsageResponse Prelude.Int
listIdentityPoolUsageResponse_httpStatus = Lens.lens (\ListIdentityPoolUsageResponse' {httpStatus} -> httpStatus) (\s@ListIdentityPoolUsageResponse' {} a -> s {httpStatus = a} :: ListIdentityPoolUsageResponse)

instance Prelude.NFData ListIdentityPoolUsageResponse where
  rnf ListIdentityPoolUsageResponse' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf identityPoolUsages
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
