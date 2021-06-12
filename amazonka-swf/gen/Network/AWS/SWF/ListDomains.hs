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
-- Module      : Network.AWS.SWF.ListDomains
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SWF.ListDomains
  ( -- * Creating a Request
    ListDomains (..),
    newListDomains,

    -- * Request Lenses
    listDomains_nextPageToken,
    listDomains_maximumPageSize,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newListDomains' smart constructor.
data ListDomains = ListDomains'
  { -- | If @NextPageToken@ is returned there are more results available. The
    -- value of @NextPageToken@ is a unique pagination token for each page.
    -- Make the call again using the returned token to retrieve the next page.
    -- Keep all other arguments unchanged. Each pagination token expires after
    -- 60 seconds. Using an expired pagination token will return a @400@ error:
    -- \"@Specified token has exceeded its maximum lifetime@\".
    --
    -- The configured @maximumPageSize@ determines how many results can be
    -- returned in a single call.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The maximum number of results that are returned per call. Use
    -- @nextPageToken@ to obtain further pages of results.
    maximumPageSize :: Core.Maybe Core.Natural,
    -- | When set to @true@, returns the results in reverse order. By default,
    -- the results are returned in ascending alphabetical order by @name@ of
    -- the domains.
    reverseOrder :: Core.Maybe Core.Bool,
    -- | Specifies the registration status of the domains to list.
    registrationStatus :: RegistrationStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'maximumPageSize', 'listDomains_maximumPageSize' - The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
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
    { nextPageToken = Core.Nothing,
      maximumPageSize = Core.Nothing,
      reverseOrder = Core.Nothing,
      registrationStatus = pRegistrationStatus_
    }

-- | If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 60 seconds. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
listDomains_nextPageToken :: Lens.Lens' ListDomains (Core.Maybe Core.Text)
listDomains_nextPageToken = Lens.lens (\ListDomains' {nextPageToken} -> nextPageToken) (\s@ListDomains' {} a -> s {nextPageToken = a} :: ListDomains)

-- | The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
listDomains_maximumPageSize :: Lens.Lens' ListDomains (Core.Maybe Core.Natural)
listDomains_maximumPageSize = Lens.lens (\ListDomains' {maximumPageSize} -> maximumPageSize) (\s@ListDomains' {} a -> s {maximumPageSize = a} :: ListDomains)

-- | When set to @true@, returns the results in reverse order. By default,
-- the results are returned in ascending alphabetical order by @name@ of
-- the domains.
listDomains_reverseOrder :: Lens.Lens' ListDomains (Core.Maybe Core.Bool)
listDomains_reverseOrder = Lens.lens (\ListDomains' {reverseOrder} -> reverseOrder) (\s@ListDomains' {} a -> s {reverseOrder = a} :: ListDomains)

-- | Specifies the registration status of the domains to list.
listDomains_registrationStatus :: Lens.Lens' ListDomains RegistrationStatus
listDomains_registrationStatus = Lens.lens (\ListDomains' {registrationStatus} -> registrationStatus) (\s@ListDomains' {} a -> s {registrationStatus = a} :: ListDomains)

instance Core.AWSPager ListDomains where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDomainsResponse_nextPageToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listDomainsResponse_domainInfos) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDomains_nextPageToken
          Lens..~ rs
          Lens.^? listDomainsResponse_nextPageToken Core.. Lens._Just

instance Core.AWSRequest ListDomains where
  type AWSResponse ListDomains = ListDomainsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainsResponse'
            Core.<$> (x Core..?> "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "domainInfos" Core..!@ Core.mempty)
      )

instance Core.Hashable ListDomains

instance Core.NFData ListDomains

instance Core.ToHeaders ListDomains where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SimpleWorkflowService.ListDomains" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDomains where
  toJSON ListDomains' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextPageToken" Core..=) Core.<$> nextPageToken,
            ("maximumPageSize" Core..=) Core.<$> maximumPageSize,
            ("reverseOrder" Core..=) Core.<$> reverseOrder,
            Core.Just
              ("registrationStatus" Core..= registrationStatus)
          ]
      )

instance Core.ToPath ListDomains where
  toPath = Core.const "/"

instance Core.ToQuery ListDomains where
  toQuery = Core.const Core.mempty

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
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of DomainInfo structures.
    domainInfos :: [DomainInfo]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListDomainsResponse
newListDomainsResponse pHttpStatus_ =
  ListDomainsResponse'
    { nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      domainInfos = Core.mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
listDomainsResponse_nextPageToken :: Lens.Lens' ListDomainsResponse (Core.Maybe Core.Text)
listDomainsResponse_nextPageToken = Lens.lens (\ListDomainsResponse' {nextPageToken} -> nextPageToken) (\s@ListDomainsResponse' {} a -> s {nextPageToken = a} :: ListDomainsResponse)

-- | The response's http status code.
listDomainsResponse_httpStatus :: Lens.Lens' ListDomainsResponse Core.Int
listDomainsResponse_httpStatus = Lens.lens (\ListDomainsResponse' {httpStatus} -> httpStatus) (\s@ListDomainsResponse' {} a -> s {httpStatus = a} :: ListDomainsResponse)

-- | A list of DomainInfo structures.
listDomainsResponse_domainInfos :: Lens.Lens' ListDomainsResponse [DomainInfo]
listDomainsResponse_domainInfos = Lens.lens (\ListDomainsResponse' {domainInfos} -> domainInfos) (\s@ListDomainsResponse' {} a -> s {domainInfos = a} :: ListDomainsResponse) Core.. Lens._Coerce

instance Core.NFData ListDomainsResponse
