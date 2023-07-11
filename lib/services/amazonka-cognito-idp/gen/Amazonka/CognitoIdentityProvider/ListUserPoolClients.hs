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
-- Module      : Amazonka.CognitoIdentityProvider.ListUserPoolClients
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the clients that have been created for the specified user pool.
--
-- This operation returns paginated results.
module Amazonka.CognitoIdentityProvider.ListUserPoolClients
  ( -- * Creating a Request
    ListUserPoolClients (..),
    newListUserPoolClients,

    -- * Request Lenses
    listUserPoolClients_maxResults,
    listUserPoolClients_nextToken,
    listUserPoolClients_userPoolId,

    -- * Destructuring the Response
    ListUserPoolClientsResponse (..),
    newListUserPoolClientsResponse,

    -- * Response Lenses
    listUserPoolClientsResponse_nextToken,
    listUserPoolClientsResponse_userPoolClients,
    listUserPoolClientsResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to list the user pool clients.
--
-- /See:/ 'newListUserPoolClients' smart constructor.
data ListUserPoolClients = ListUserPoolClients'
  { -- | The maximum number of results you want the request to return when
    -- listing the user pool clients.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The user pool ID for the user pool where you want to list user pool
    -- clients.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserPoolClients' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listUserPoolClients_maxResults' - The maximum number of results you want the request to return when
-- listing the user pool clients.
--
-- 'nextToken', 'listUserPoolClients_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'userPoolId', 'listUserPoolClients_userPoolId' - The user pool ID for the user pool where you want to list user pool
-- clients.
newListUserPoolClients ::
  -- | 'userPoolId'
  Prelude.Text ->
  ListUserPoolClients
newListUserPoolClients pUserPoolId_ =
  ListUserPoolClients'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | The maximum number of results you want the request to return when
-- listing the user pool clients.
listUserPoolClients_maxResults :: Lens.Lens' ListUserPoolClients (Prelude.Maybe Prelude.Natural)
listUserPoolClients_maxResults = Lens.lens (\ListUserPoolClients' {maxResults} -> maxResults) (\s@ListUserPoolClients' {} a -> s {maxResults = a} :: ListUserPoolClients)

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUserPoolClients_nextToken :: Lens.Lens' ListUserPoolClients (Prelude.Maybe Prelude.Text)
listUserPoolClients_nextToken = Lens.lens (\ListUserPoolClients' {nextToken} -> nextToken) (\s@ListUserPoolClients' {} a -> s {nextToken = a} :: ListUserPoolClients)

-- | The user pool ID for the user pool where you want to list user pool
-- clients.
listUserPoolClients_userPoolId :: Lens.Lens' ListUserPoolClients Prelude.Text
listUserPoolClients_userPoolId = Lens.lens (\ListUserPoolClients' {userPoolId} -> userPoolId) (\s@ListUserPoolClients' {} a -> s {userPoolId = a} :: ListUserPoolClients)

instance Core.AWSPager ListUserPoolClients where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUserPoolClientsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listUserPoolClientsResponse_userPoolClients
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listUserPoolClients_nextToken
          Lens..~ rs
          Lens.^? listUserPoolClientsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListUserPoolClients where
  type
    AWSResponse ListUserPoolClients =
      ListUserPoolClientsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUserPoolClientsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "UserPoolClients"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListUserPoolClients where
  hashWithSalt _salt ListUserPoolClients' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData ListUserPoolClients where
  rnf ListUserPoolClients' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf userPoolId

instance Data.ToHeaders ListUserPoolClients where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.ListUserPoolClients" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListUserPoolClients where
  toJSON ListUserPoolClients' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("UserPoolId" Data..= userPoolId)
          ]
      )

instance Data.ToPath ListUserPoolClients where
  toPath = Prelude.const "/"

instance Data.ToQuery ListUserPoolClients where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server that lists user pool clients.
--
-- /See:/ 'newListUserPoolClientsResponse' smart constructor.
data ListUserPoolClientsResponse = ListUserPoolClientsResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The user pool clients in the response that lists user pool clients.
    userPoolClients :: Prelude.Maybe [UserPoolClientDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserPoolClientsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUserPoolClientsResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'userPoolClients', 'listUserPoolClientsResponse_userPoolClients' - The user pool clients in the response that lists user pool clients.
--
-- 'httpStatus', 'listUserPoolClientsResponse_httpStatus' - The response's http status code.
newListUserPoolClientsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUserPoolClientsResponse
newListUserPoolClientsResponse pHttpStatus_ =
  ListUserPoolClientsResponse'
    { nextToken =
        Prelude.Nothing,
      userPoolClients = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUserPoolClientsResponse_nextToken :: Lens.Lens' ListUserPoolClientsResponse (Prelude.Maybe Prelude.Text)
listUserPoolClientsResponse_nextToken = Lens.lens (\ListUserPoolClientsResponse' {nextToken} -> nextToken) (\s@ListUserPoolClientsResponse' {} a -> s {nextToken = a} :: ListUserPoolClientsResponse)

-- | The user pool clients in the response that lists user pool clients.
listUserPoolClientsResponse_userPoolClients :: Lens.Lens' ListUserPoolClientsResponse (Prelude.Maybe [UserPoolClientDescription])
listUserPoolClientsResponse_userPoolClients = Lens.lens (\ListUserPoolClientsResponse' {userPoolClients} -> userPoolClients) (\s@ListUserPoolClientsResponse' {} a -> s {userPoolClients = a} :: ListUserPoolClientsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listUserPoolClientsResponse_httpStatus :: Lens.Lens' ListUserPoolClientsResponse Prelude.Int
listUserPoolClientsResponse_httpStatus = Lens.lens (\ListUserPoolClientsResponse' {httpStatus} -> httpStatus) (\s@ListUserPoolClientsResponse' {} a -> s {httpStatus = a} :: ListUserPoolClientsResponse)

instance Prelude.NFData ListUserPoolClientsResponse where
  rnf ListUserPoolClientsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf userPoolClients
      `Prelude.seq` Prelude.rnf httpStatus
