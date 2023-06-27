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
-- Module      : Amazonka.FMS.ListAdminsManagingAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the accounts that are managing the specified Organizations member
-- account. This is useful for any member account so that they can view the
-- accounts who are managing their account. This operation only returns the
-- managing administrators that have the requested account within their
-- AdminScope.
--
-- This operation returns paginated results.
module Amazonka.FMS.ListAdminsManagingAccount
  ( -- * Creating a Request
    ListAdminsManagingAccount (..),
    newListAdminsManagingAccount,

    -- * Request Lenses
    listAdminsManagingAccount_maxResults,
    listAdminsManagingAccount_nextToken,

    -- * Destructuring the Response
    ListAdminsManagingAccountResponse (..),
    newListAdminsManagingAccountResponse,

    -- * Response Lenses
    listAdminsManagingAccountResponse_adminAccounts,
    listAdminsManagingAccountResponse_nextToken,
    listAdminsManagingAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAdminsManagingAccount' smart constructor.
data ListAdminsManagingAccount = ListAdminsManagingAccount'
  { -- | The maximum number of objects that you want Firewall Manager to return
    -- for this request. If more objects are available, in the response,
    -- Firewall Manager provides a @NextToken@ value that you can use in a
    -- subsequent call to get the next batch of objects.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Firewall Manager returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAdminsManagingAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAdminsManagingAccount_maxResults' - The maximum number of objects that you want Firewall Manager to return
-- for this request. If more objects are available, in the response,
-- Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
--
-- 'nextToken', 'listAdminsManagingAccount_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
newListAdminsManagingAccount ::
  ListAdminsManagingAccount
newListAdminsManagingAccount =
  ListAdminsManagingAccount'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of objects that you want Firewall Manager to return
-- for this request. If more objects are available, in the response,
-- Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
listAdminsManagingAccount_maxResults :: Lens.Lens' ListAdminsManagingAccount (Prelude.Maybe Prelude.Natural)
listAdminsManagingAccount_maxResults = Lens.lens (\ListAdminsManagingAccount' {maxResults} -> maxResults) (\s@ListAdminsManagingAccount' {} a -> s {maxResults = a} :: ListAdminsManagingAccount)

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listAdminsManagingAccount_nextToken :: Lens.Lens' ListAdminsManagingAccount (Prelude.Maybe Prelude.Text)
listAdminsManagingAccount_nextToken = Lens.lens (\ListAdminsManagingAccount' {nextToken} -> nextToken) (\s@ListAdminsManagingAccount' {} a -> s {nextToken = a} :: ListAdminsManagingAccount)

instance Core.AWSPager ListAdminsManagingAccount where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAdminsManagingAccountResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAdminsManagingAccountResponse_adminAccounts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAdminsManagingAccount_nextToken
          Lens..~ rs
          Lens.^? listAdminsManagingAccountResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAdminsManagingAccount where
  type
    AWSResponse ListAdminsManagingAccount =
      ListAdminsManagingAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAdminsManagingAccountResponse'
            Prelude.<$> (x Data..?> "AdminAccounts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAdminsManagingAccount where
  hashWithSalt _salt ListAdminsManagingAccount' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListAdminsManagingAccount where
  rnf ListAdminsManagingAccount' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListAdminsManagingAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.ListAdminsManagingAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAdminsManagingAccount where
  toJSON ListAdminsManagingAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListAdminsManagingAccount where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAdminsManagingAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAdminsManagingAccountResponse' smart constructor.
data ListAdminsManagingAccountResponse = ListAdminsManagingAccountResponse'
  { -- | The list of accounts who manage member accounts within their AdminScope.
    adminAccounts :: Prelude.Maybe [Prelude.Text],
    -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Firewall Manager returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAdminsManagingAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccounts', 'listAdminsManagingAccountResponse_adminAccounts' - The list of accounts who manage member accounts within their AdminScope.
--
-- 'nextToken', 'listAdminsManagingAccountResponse_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
--
-- 'httpStatus', 'listAdminsManagingAccountResponse_httpStatus' - The response's http status code.
newListAdminsManagingAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAdminsManagingAccountResponse
newListAdminsManagingAccountResponse pHttpStatus_ =
  ListAdminsManagingAccountResponse'
    { adminAccounts =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of accounts who manage member accounts within their AdminScope.
listAdminsManagingAccountResponse_adminAccounts :: Lens.Lens' ListAdminsManagingAccountResponse (Prelude.Maybe [Prelude.Text])
listAdminsManagingAccountResponse_adminAccounts = Lens.lens (\ListAdminsManagingAccountResponse' {adminAccounts} -> adminAccounts) (\s@ListAdminsManagingAccountResponse' {} a -> s {adminAccounts = a} :: ListAdminsManagingAccountResponse) Prelude.. Lens.mapping Lens.coerced

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listAdminsManagingAccountResponse_nextToken :: Lens.Lens' ListAdminsManagingAccountResponse (Prelude.Maybe Prelude.Text)
listAdminsManagingAccountResponse_nextToken = Lens.lens (\ListAdminsManagingAccountResponse' {nextToken} -> nextToken) (\s@ListAdminsManagingAccountResponse' {} a -> s {nextToken = a} :: ListAdminsManagingAccountResponse)

-- | The response's http status code.
listAdminsManagingAccountResponse_httpStatus :: Lens.Lens' ListAdminsManagingAccountResponse Prelude.Int
listAdminsManagingAccountResponse_httpStatus = Lens.lens (\ListAdminsManagingAccountResponse' {httpStatus} -> httpStatus) (\s@ListAdminsManagingAccountResponse' {} a -> s {httpStatus = a} :: ListAdminsManagingAccountResponse)

instance
  Prelude.NFData
    ListAdminsManagingAccountResponse
  where
  rnf ListAdminsManagingAccountResponse' {..} =
    Prelude.rnf adminAccounts
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
