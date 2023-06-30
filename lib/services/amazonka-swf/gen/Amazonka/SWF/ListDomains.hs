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
-- Module      : Amazonka.SWF.ListDomains
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of domains registered in the account. The results may
-- be split into multiple pages. To retrieve subsequent pages, make the
-- call again using the nextPageToken returned by the initial call.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains. The element must be set to
--     @arn:aws:swf::AccountID:domain\/*@, where /AccountID/ is the account
--     ID, with no dashes.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
--
-- This operation returns paginated results.
module Amazonka.SWF.ListDomains
  ( -- * Creating a Request
    ListDomains (..),
    newListDomains,

    -- * Request Lenses
    listDomains_maximumPageSize,
    listDomains_nextPageToken,
    listDomains_reverseOrder,
    listDomains_registrationStatus,

    -- * Destructuring the Response
    ListDomainsResponse (..),
    newListDomainsResponse,

    -- * Response Lenses
    listDomainsResponse_nextPageToken,
    listDomainsResponse_httpStatus,
    listDomainsResponse_domainInfos,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newListDomains' smart constructor.
data ListDomains = ListDomains'
  { -- | The maximum number of results that are returned per call. Use
    -- @nextPageToken@ to obtain further pages of results.
    maximumPageSize :: Prelude.Maybe Prelude.Natural,
    -- | If @NextPageToken@ is returned there are more results available. The
    -- value of @NextPageToken@ is a unique pagination token for each page.
    -- Make the call again using the returned token to retrieve the next page.
    -- Keep all other arguments unchanged. Each pagination token expires after
    -- 60 seconds. Using an expired pagination token will return a @400@ error:
    -- \"@Specified token has exceeded its maximum lifetime@\".
    --
    -- The configured @maximumPageSize@ determines how many results can be
    -- returned in a single call.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | When set to @true@, returns the results in reverse order. By default,
    -- the results are returned in ascending alphabetical order by @name@ of
    -- the domains.
    reverseOrder :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the registration status of the domains to list.
    registrationStatus :: RegistrationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumPageSize', 'listDomains_maximumPageSize' - The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
--
-- 'nextPageToken', 'listDomains_nextPageToken' - If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 60 seconds. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
--
-- 'reverseOrder', 'listDomains_reverseOrder' - When set to @true@, returns the results in reverse order. By default,
-- the results are returned in ascending alphabetical order by @name@ of
-- the domains.
--
-- 'registrationStatus', 'listDomains_registrationStatus' - Specifies the registration status of the domains to list.
newListDomains ::
  -- | 'registrationStatus'
  RegistrationStatus ->
  ListDomains
newListDomains pRegistrationStatus_ =
  ListDomains'
    { maximumPageSize = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      reverseOrder = Prelude.Nothing,
      registrationStatus = pRegistrationStatus_
    }

-- | The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
listDomains_maximumPageSize :: Lens.Lens' ListDomains (Prelude.Maybe Prelude.Natural)
listDomains_maximumPageSize = Lens.lens (\ListDomains' {maximumPageSize} -> maximumPageSize) (\s@ListDomains' {} a -> s {maximumPageSize = a} :: ListDomains)

-- | If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 60 seconds. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
listDomains_nextPageToken :: Lens.Lens' ListDomains (Prelude.Maybe Prelude.Text)
listDomains_nextPageToken = Lens.lens (\ListDomains' {nextPageToken} -> nextPageToken) (\s@ListDomains' {} a -> s {nextPageToken = a} :: ListDomains)

-- | When set to @true@, returns the results in reverse order. By default,
-- the results are returned in ascending alphabetical order by @name@ of
-- the domains.
listDomains_reverseOrder :: Lens.Lens' ListDomains (Prelude.Maybe Prelude.Bool)
listDomains_reverseOrder = Lens.lens (\ListDomains' {reverseOrder} -> reverseOrder) (\s@ListDomains' {} a -> s {reverseOrder = a} :: ListDomains)

-- | Specifies the registration status of the domains to list.
listDomains_registrationStatus :: Lens.Lens' ListDomains RegistrationStatus
listDomains_registrationStatus = Lens.lens (\ListDomains' {registrationStatus} -> registrationStatus) (\s@ListDomains' {} a -> s {registrationStatus = a} :: ListDomains)

instance Core.AWSPager ListDomains where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDomainsResponse_nextPageToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listDomainsResponse_domainInfos) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDomains_nextPageToken
          Lens..~ rs
          Lens.^? listDomainsResponse_nextPageToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDomains where
  type AWSResponse ListDomains = ListDomainsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainsResponse'
            Prelude.<$> (x Data..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "domainInfos" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListDomains where
  hashWithSalt _salt ListDomains' {..} =
    _salt
      `Prelude.hashWithSalt` maximumPageSize
      `Prelude.hashWithSalt` nextPageToken
      `Prelude.hashWithSalt` reverseOrder
      `Prelude.hashWithSalt` registrationStatus

instance Prelude.NFData ListDomains where
  rnf ListDomains' {..} =
    Prelude.rnf maximumPageSize
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf reverseOrder
      `Prelude.seq` Prelude.rnf registrationStatus

instance Data.ToHeaders ListDomains where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SimpleWorkflowService.ListDomains" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDomains where
  toJSON ListDomains' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maximumPageSize" Data..=)
              Prelude.<$> maximumPageSize,
            ("nextPageToken" Data..=) Prelude.<$> nextPageToken,
            ("reverseOrder" Data..=) Prelude.<$> reverseOrder,
            Prelude.Just
              ("registrationStatus" Data..= registrationStatus)
          ]
      )

instance Data.ToPath ListDomains where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDomains where
  toQuery = Prelude.const Prelude.mempty

-- | Contains a paginated collection of DomainInfo structures.
--
-- /See:/ 'newListDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { -- | If a @NextPageToken@ was returned by a previous call, there are more
    -- results available. To retrieve the next page of results, make the call
    -- again using the returned token in @nextPageToken@. Keep all other
    -- arguments unchanged.
    --
    -- The configured @maximumPageSize@ determines how many results can be
    -- returned in a single call.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of DomainInfo structures.
    domainInfos :: [DomainInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'listDomainsResponse_nextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
--
-- 'httpStatus', 'listDomainsResponse_httpStatus' - The response's http status code.
--
-- 'domainInfos', 'listDomainsResponse_domainInfos' - A list of DomainInfo structures.
newListDomainsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDomainsResponse
newListDomainsResponse pHttpStatus_ =
  ListDomainsResponse'
    { nextPageToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      domainInfos = Prelude.mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
listDomainsResponse_nextPageToken :: Lens.Lens' ListDomainsResponse (Prelude.Maybe Prelude.Text)
listDomainsResponse_nextPageToken = Lens.lens (\ListDomainsResponse' {nextPageToken} -> nextPageToken) (\s@ListDomainsResponse' {} a -> s {nextPageToken = a} :: ListDomainsResponse)

-- | The response's http status code.
listDomainsResponse_httpStatus :: Lens.Lens' ListDomainsResponse Prelude.Int
listDomainsResponse_httpStatus = Lens.lens (\ListDomainsResponse' {httpStatus} -> httpStatus) (\s@ListDomainsResponse' {} a -> s {httpStatus = a} :: ListDomainsResponse)

-- | A list of DomainInfo structures.
listDomainsResponse_domainInfos :: Lens.Lens' ListDomainsResponse [DomainInfo]
listDomainsResponse_domainInfos = Lens.lens (\ListDomainsResponse' {domainInfos} -> domainInfos) (\s@ListDomainsResponse' {} a -> s {domainInfos = a} :: ListDomainsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListDomainsResponse where
  rnf ListDomainsResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainInfos
