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
-- Module      : Network.AWS.CognitoIdentityProvider.ListUserPoolClients
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the clients that have been created for the specified user pool.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListUserPoolClients
  ( -- * Creating a Request
    ListUserPoolClients (..),
    newListUserPoolClients,

    -- * Request Lenses
    listUserPoolClients_nextToken,
    listUserPoolClients_maxResults,
    listUserPoolClients_userPoolId,

    -- * Destructuring the Response
    ListUserPoolClientsResponse (..),
    newListUserPoolClientsResponse,

    -- * Response Lenses
    listUserPoolClientsResponse_userPoolClients,
    listUserPoolClientsResponse_nextToken,
    listUserPoolClientsResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list the user pool clients.
--
-- /See:/ 'newListUserPoolClients' smart constructor.
data ListUserPoolClients = ListUserPoolClients'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results you want the request to return when
    -- listing the user pool clients.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'listUserPoolClients_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'maxResults', 'listUserPoolClients_maxResults' - The maximum number of results you want the request to return when
-- listing the user pool clients.
--
-- 'userPoolId', 'listUserPoolClients_userPoolId' - The user pool ID for the user pool where you want to list user pool
-- clients.
newListUserPoolClients ::
  -- | 'userPoolId'
  Prelude.Text ->
  ListUserPoolClients
newListUserPoolClients pUserPoolId_ =
  ListUserPoolClients'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUserPoolClients_nextToken :: Lens.Lens' ListUserPoolClients (Prelude.Maybe Prelude.Text)
listUserPoolClients_nextToken = Lens.lens (\ListUserPoolClients' {nextToken} -> nextToken) (\s@ListUserPoolClients' {} a -> s {nextToken = a} :: ListUserPoolClients)

-- | The maximum number of results you want the request to return when
-- listing the user pool clients.
listUserPoolClients_maxResults :: Lens.Lens' ListUserPoolClients (Prelude.Maybe Prelude.Natural)
listUserPoolClients_maxResults = Lens.lens (\ListUserPoolClients' {maxResults} -> maxResults) (\s@ListUserPoolClients' {} a -> s {maxResults = a} :: ListUserPoolClients)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& listUserPoolClients_nextToken
          Lens..~ rs
          Lens.^? listUserPoolClientsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListUserPoolClients where
  type
    AWSResponse ListUserPoolClients =
      ListUserPoolClientsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUserPoolClientsResponse'
            Prelude.<$> ( x Core..?> "UserPoolClients"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListUserPoolClients

instance Prelude.NFData ListUserPoolClients

instance Core.ToHeaders ListUserPoolClients where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ListUserPoolClients" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListUserPoolClients where
  toJSON ListUserPoolClients' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath ListUserPoolClients where
  toPath = Prelude.const "/"

instance Core.ToQuery ListUserPoolClients where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server that lists user pool clients.
--
-- /See:/ 'newListUserPoolClientsResponse' smart constructor.
data ListUserPoolClientsResponse = ListUserPoolClientsResponse'
  { -- | The user pool clients in the response that lists user pool clients.
    userPoolClients :: Prelude.Maybe [UserPoolClientDescription],
    -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'userPoolClients', 'listUserPoolClientsResponse_userPoolClients' - The user pool clients in the response that lists user pool clients.
--
-- 'nextToken', 'listUserPoolClientsResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'httpStatus', 'listUserPoolClientsResponse_httpStatus' - The response's http status code.
newListUserPoolClientsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUserPoolClientsResponse
newListUserPoolClientsResponse pHttpStatus_ =
  ListUserPoolClientsResponse'
    { userPoolClients =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user pool clients in the response that lists user pool clients.
listUserPoolClientsResponse_userPoolClients :: Lens.Lens' ListUserPoolClientsResponse (Prelude.Maybe [UserPoolClientDescription])
listUserPoolClientsResponse_userPoolClients = Lens.lens (\ListUserPoolClientsResponse' {userPoolClients} -> userPoolClients) (\s@ListUserPoolClientsResponse' {} a -> s {userPoolClients = a} :: ListUserPoolClientsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUserPoolClientsResponse_nextToken :: Lens.Lens' ListUserPoolClientsResponse (Prelude.Maybe Prelude.Text)
listUserPoolClientsResponse_nextToken = Lens.lens (\ListUserPoolClientsResponse' {nextToken} -> nextToken) (\s@ListUserPoolClientsResponse' {} a -> s {nextToken = a} :: ListUserPoolClientsResponse)

-- | The response's http status code.
listUserPoolClientsResponse_httpStatus :: Lens.Lens' ListUserPoolClientsResponse Prelude.Int
listUserPoolClientsResponse_httpStatus = Lens.lens (\ListUserPoolClientsResponse' {httpStatus} -> httpStatus) (\s@ListUserPoolClientsResponse' {} a -> s {httpStatus = a} :: ListUserPoolClientsResponse)

instance Prelude.NFData ListUserPoolClientsResponse
