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
-- Module      : Amazonka.CognitoIdentity.ListIdentityPools
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the Cognito identity pools registered for your account.
--
-- You must use AWS Developer credentials to call this API.
--
-- This operation returns paginated results.
module Amazonka.CognitoIdentity.ListIdentityPools
  ( -- * Creating a Request
    ListIdentityPools (..),
    newListIdentityPools,

    -- * Request Lenses
    listIdentityPools_nextToken,
    listIdentityPools_maxResults,

    -- * Destructuring the Response
    ListIdentityPoolsResponse (..),
    newListIdentityPoolsResponse,

    -- * Response Lenses
    listIdentityPoolsResponse_identityPools,
    listIdentityPoolsResponse_nextToken,
    listIdentityPoolsResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input to the ListIdentityPools action.
--
-- /See:/ 'newListIdentityPools' smart constructor.
data ListIdentityPools = ListIdentityPools'
  { -- | A pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of identities to return.
    maxResults :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentityPools' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIdentityPools_nextToken' - A pagination token.
--
-- 'maxResults', 'listIdentityPools_maxResults' - The maximum number of identities to return.
newListIdentityPools ::
  -- | 'maxResults'
  Prelude.Natural ->
  ListIdentityPools
newListIdentityPools pMaxResults_ =
  ListIdentityPools'
    { nextToken = Prelude.Nothing,
      maxResults = pMaxResults_
    }

-- | A pagination token.
listIdentityPools_nextToken :: Lens.Lens' ListIdentityPools (Prelude.Maybe Prelude.Text)
listIdentityPools_nextToken = Lens.lens (\ListIdentityPools' {nextToken} -> nextToken) (\s@ListIdentityPools' {} a -> s {nextToken = a} :: ListIdentityPools)

-- | The maximum number of identities to return.
listIdentityPools_maxResults :: Lens.Lens' ListIdentityPools Prelude.Natural
listIdentityPools_maxResults = Lens.lens (\ListIdentityPools' {maxResults} -> maxResults) (\s@ListIdentityPools' {} a -> s {maxResults = a} :: ListIdentityPools)

instance Core.AWSPager ListIdentityPools where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIdentityPoolsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listIdentityPoolsResponse_identityPools
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listIdentityPools_nextToken
          Lens..~ rs
          Lens.^? listIdentityPoolsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListIdentityPools where
  type
    AWSResponse ListIdentityPools =
      ListIdentityPoolsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIdentityPoolsResponse'
            Prelude.<$> (x Data..?> "IdentityPools" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIdentityPools where
  hashWithSalt _salt ListIdentityPools' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListIdentityPools where
  rnf ListIdentityPools' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListIdentityPools where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityService.ListIdentityPools" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListIdentityPools where
  toJSON ListIdentityPools' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("MaxResults" Data..= maxResults)
          ]
      )

instance Data.ToPath ListIdentityPools where
  toPath = Prelude.const "/"

instance Data.ToQuery ListIdentityPools where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a successful ListIdentityPools action.
--
-- /See:/ 'newListIdentityPoolsResponse' smart constructor.
data ListIdentityPoolsResponse = ListIdentityPoolsResponse'
  { -- | The identity pools returned by the ListIdentityPools action.
    identityPools :: Prelude.Maybe [IdentityPoolShortDescription],
    -- | A pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentityPoolsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPools', 'listIdentityPoolsResponse_identityPools' - The identity pools returned by the ListIdentityPools action.
--
-- 'nextToken', 'listIdentityPoolsResponse_nextToken' - A pagination token.
--
-- 'httpStatus', 'listIdentityPoolsResponse_httpStatus' - The response's http status code.
newListIdentityPoolsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIdentityPoolsResponse
newListIdentityPoolsResponse pHttpStatus_ =
  ListIdentityPoolsResponse'
    { identityPools =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identity pools returned by the ListIdentityPools action.
listIdentityPoolsResponse_identityPools :: Lens.Lens' ListIdentityPoolsResponse (Prelude.Maybe [IdentityPoolShortDescription])
listIdentityPoolsResponse_identityPools = Lens.lens (\ListIdentityPoolsResponse' {identityPools} -> identityPools) (\s@ListIdentityPoolsResponse' {} a -> s {identityPools = a} :: ListIdentityPoolsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token.
listIdentityPoolsResponse_nextToken :: Lens.Lens' ListIdentityPoolsResponse (Prelude.Maybe Prelude.Text)
listIdentityPoolsResponse_nextToken = Lens.lens (\ListIdentityPoolsResponse' {nextToken} -> nextToken) (\s@ListIdentityPoolsResponse' {} a -> s {nextToken = a} :: ListIdentityPoolsResponse)

-- | The response's http status code.
listIdentityPoolsResponse_httpStatus :: Lens.Lens' ListIdentityPoolsResponse Prelude.Int
listIdentityPoolsResponse_httpStatus = Lens.lens (\ListIdentityPoolsResponse' {httpStatus} -> httpStatus) (\s@ListIdentityPoolsResponse' {} a -> s {httpStatus = a} :: ListIdentityPoolsResponse)

instance Prelude.NFData ListIdentityPoolsResponse where
  rnf ListIdentityPoolsResponse' {..} =
    Prelude.rnf identityPools
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
