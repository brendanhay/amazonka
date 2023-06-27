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
-- Module      : Amazonka.FMS.ListAdminAccountsForOrganization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a @AdminAccounts@ object that lists the Firewall Manager
-- administrators within the organization that are onboarded to Firewall
-- Manager by AssociateAdminAccount.
--
-- This operation can be called only from the organization\'s management
-- account.
--
-- This operation returns paginated results.
module Amazonka.FMS.ListAdminAccountsForOrganization
  ( -- * Creating a Request
    ListAdminAccountsForOrganization (..),
    newListAdminAccountsForOrganization,

    -- * Request Lenses
    listAdminAccountsForOrganization_maxResults,
    listAdminAccountsForOrganization_nextToken,

    -- * Destructuring the Response
    ListAdminAccountsForOrganizationResponse (..),
    newListAdminAccountsForOrganizationResponse,

    -- * Response Lenses
    listAdminAccountsForOrganizationResponse_adminAccounts,
    listAdminAccountsForOrganizationResponse_nextToken,
    listAdminAccountsForOrganizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAdminAccountsForOrganization' smart constructor.
data ListAdminAccountsForOrganization = ListAdminAccountsForOrganization'
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
-- Create a value of 'ListAdminAccountsForOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAdminAccountsForOrganization_maxResults' - The maximum number of objects that you want Firewall Manager to return
-- for this request. If more objects are available, in the response,
-- Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
--
-- 'nextToken', 'listAdminAccountsForOrganization_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
newListAdminAccountsForOrganization ::
  ListAdminAccountsForOrganization
newListAdminAccountsForOrganization =
  ListAdminAccountsForOrganization'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of objects that you want Firewall Manager to return
-- for this request. If more objects are available, in the response,
-- Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
listAdminAccountsForOrganization_maxResults :: Lens.Lens' ListAdminAccountsForOrganization (Prelude.Maybe Prelude.Natural)
listAdminAccountsForOrganization_maxResults = Lens.lens (\ListAdminAccountsForOrganization' {maxResults} -> maxResults) (\s@ListAdminAccountsForOrganization' {} a -> s {maxResults = a} :: ListAdminAccountsForOrganization)

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listAdminAccountsForOrganization_nextToken :: Lens.Lens' ListAdminAccountsForOrganization (Prelude.Maybe Prelude.Text)
listAdminAccountsForOrganization_nextToken = Lens.lens (\ListAdminAccountsForOrganization' {nextToken} -> nextToken) (\s@ListAdminAccountsForOrganization' {} a -> s {nextToken = a} :: ListAdminAccountsForOrganization)

instance
  Core.AWSPager
    ListAdminAccountsForOrganization
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAdminAccountsForOrganizationResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAdminAccountsForOrganizationResponse_adminAccounts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAdminAccountsForOrganization_nextToken
          Lens..~ rs
          Lens.^? listAdminAccountsForOrganizationResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListAdminAccountsForOrganization
  where
  type
    AWSResponse ListAdminAccountsForOrganization =
      ListAdminAccountsForOrganizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAdminAccountsForOrganizationResponse'
            Prelude.<$> (x Data..?> "AdminAccounts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAdminAccountsForOrganization
  where
  hashWithSalt
    _salt
    ListAdminAccountsForOrganization' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListAdminAccountsForOrganization
  where
  rnf ListAdminAccountsForOrganization' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    ListAdminAccountsForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.ListAdminAccountsForOrganization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAdminAccountsForOrganization where
  toJSON ListAdminAccountsForOrganization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListAdminAccountsForOrganization where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListAdminAccountsForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAdminAccountsForOrganizationResponse' smart constructor.
data ListAdminAccountsForOrganizationResponse = ListAdminAccountsForOrganizationResponse'
  { -- | A list of Firewall Manager administrator accounts within the
    -- organization that were onboarded as administrators by
    -- AssociateAdminAccount or PutAdminAccount.
    adminAccounts :: Prelude.Maybe [AdminAccountSummary],
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
-- Create a value of 'ListAdminAccountsForOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccounts', 'listAdminAccountsForOrganizationResponse_adminAccounts' - A list of Firewall Manager administrator accounts within the
-- organization that were onboarded as administrators by
-- AssociateAdminAccount or PutAdminAccount.
--
-- 'nextToken', 'listAdminAccountsForOrganizationResponse_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
--
-- 'httpStatus', 'listAdminAccountsForOrganizationResponse_httpStatus' - The response's http status code.
newListAdminAccountsForOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAdminAccountsForOrganizationResponse
newListAdminAccountsForOrganizationResponse
  pHttpStatus_ =
    ListAdminAccountsForOrganizationResponse'
      { adminAccounts =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of Firewall Manager administrator accounts within the
-- organization that were onboarded as administrators by
-- AssociateAdminAccount or PutAdminAccount.
listAdminAccountsForOrganizationResponse_adminAccounts :: Lens.Lens' ListAdminAccountsForOrganizationResponse (Prelude.Maybe [AdminAccountSummary])
listAdminAccountsForOrganizationResponse_adminAccounts = Lens.lens (\ListAdminAccountsForOrganizationResponse' {adminAccounts} -> adminAccounts) (\s@ListAdminAccountsForOrganizationResponse' {} a -> s {adminAccounts = a} :: ListAdminAccountsForOrganizationResponse) Prelude.. Lens.mapping Lens.coerced

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listAdminAccountsForOrganizationResponse_nextToken :: Lens.Lens' ListAdminAccountsForOrganizationResponse (Prelude.Maybe Prelude.Text)
listAdminAccountsForOrganizationResponse_nextToken = Lens.lens (\ListAdminAccountsForOrganizationResponse' {nextToken} -> nextToken) (\s@ListAdminAccountsForOrganizationResponse' {} a -> s {nextToken = a} :: ListAdminAccountsForOrganizationResponse)

-- | The response's http status code.
listAdminAccountsForOrganizationResponse_httpStatus :: Lens.Lens' ListAdminAccountsForOrganizationResponse Prelude.Int
listAdminAccountsForOrganizationResponse_httpStatus = Lens.lens (\ListAdminAccountsForOrganizationResponse' {httpStatus} -> httpStatus) (\s@ListAdminAccountsForOrganizationResponse' {} a -> s {httpStatus = a} :: ListAdminAccountsForOrganizationResponse)

instance
  Prelude.NFData
    ListAdminAccountsForOrganizationResponse
  where
  rnf ListAdminAccountsForOrganizationResponse' {..} =
    Prelude.rnf adminAccounts
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
