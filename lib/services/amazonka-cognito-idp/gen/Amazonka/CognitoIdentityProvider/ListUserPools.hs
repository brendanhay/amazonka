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
-- Module      : Amazonka.CognitoIdentityProvider.ListUserPools
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the user pools associated with an Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.CognitoIdentityProvider.ListUserPools
  ( -- * Creating a Request
    ListUserPools (..),
    newListUserPools,

    -- * Request Lenses
    listUserPools_nextToken,
    listUserPools_maxResults,

    -- * Destructuring the Response
    ListUserPoolsResponse (..),
    newListUserPoolsResponse,

    -- * Response Lenses
    listUserPoolsResponse_nextToken,
    listUserPoolsResponse_userPools,
    listUserPoolsResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to list user pools.
--
-- /See:/ 'newListUserPools' smart constructor.
data ListUserPools = ListUserPools'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results you want the request to return when
    -- listing the user pools.
    maxResults :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserPools' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUserPools_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'maxResults', 'listUserPools_maxResults' - The maximum number of results you want the request to return when
-- listing the user pools.
newListUserPools ::
  -- | 'maxResults'
  Prelude.Natural ->
  ListUserPools
newListUserPools pMaxResults_ =
  ListUserPools'
    { nextToken = Prelude.Nothing,
      maxResults = pMaxResults_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUserPools_nextToken :: Lens.Lens' ListUserPools (Prelude.Maybe Prelude.Text)
listUserPools_nextToken = Lens.lens (\ListUserPools' {nextToken} -> nextToken) (\s@ListUserPools' {} a -> s {nextToken = a} :: ListUserPools)

-- | The maximum number of results you want the request to return when
-- listing the user pools.
listUserPools_maxResults :: Lens.Lens' ListUserPools Prelude.Natural
listUserPools_maxResults = Lens.lens (\ListUserPools' {maxResults} -> maxResults) (\s@ListUserPools' {} a -> s {maxResults = a} :: ListUserPools)

instance Core.AWSPager ListUserPools where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUserPoolsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listUserPoolsResponse_userPools
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listUserPools_nextToken
              Lens..~ rs
              Lens.^? listUserPoolsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListUserPools where
  type
    AWSResponse ListUserPools =
      ListUserPoolsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUserPoolsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "UserPools" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListUserPools where
  hashWithSalt _salt ListUserPools' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListUserPools where
  rnf ListUserPools' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf maxResults

instance Data.ToHeaders ListUserPools where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.ListUserPools" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListUserPools where
  toJSON ListUserPools' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("MaxResults" Data..= maxResults)
          ]
      )

instance Data.ToPath ListUserPools where
  toPath = Prelude.const "/"

instance Data.ToQuery ListUserPools where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response to list user pools.
--
-- /See:/ 'newListUserPoolsResponse' smart constructor.
data ListUserPoolsResponse = ListUserPoolsResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The user pools from the response to list users.
    userPools :: Prelude.Maybe [UserPoolDescriptionType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserPoolsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUserPoolsResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'userPools', 'listUserPoolsResponse_userPools' - The user pools from the response to list users.
--
-- 'httpStatus', 'listUserPoolsResponse_httpStatus' - The response's http status code.
newListUserPoolsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUserPoolsResponse
newListUserPoolsResponse pHttpStatus_ =
  ListUserPoolsResponse'
    { nextToken = Prelude.Nothing,
      userPools = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUserPoolsResponse_nextToken :: Lens.Lens' ListUserPoolsResponse (Prelude.Maybe Prelude.Text)
listUserPoolsResponse_nextToken = Lens.lens (\ListUserPoolsResponse' {nextToken} -> nextToken) (\s@ListUserPoolsResponse' {} a -> s {nextToken = a} :: ListUserPoolsResponse)

-- | The user pools from the response to list users.
listUserPoolsResponse_userPools :: Lens.Lens' ListUserPoolsResponse (Prelude.Maybe [UserPoolDescriptionType])
listUserPoolsResponse_userPools = Lens.lens (\ListUserPoolsResponse' {userPools} -> userPools) (\s@ListUserPoolsResponse' {} a -> s {userPools = a} :: ListUserPoolsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listUserPoolsResponse_httpStatus :: Lens.Lens' ListUserPoolsResponse Prelude.Int
listUserPoolsResponse_httpStatus = Lens.lens (\ListUserPoolsResponse' {httpStatus} -> httpStatus) (\s@ListUserPoolsResponse' {} a -> s {httpStatus = a} :: ListUserPoolsResponse)

instance Prelude.NFData ListUserPoolsResponse where
  rnf ListUserPoolsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf userPools `Prelude.seq`
        Prelude.rnf httpStatus
