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
-- Module      : Amazonka.Account.ListRegions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the Regions for a given account and their respective opt-in
-- statuses. Optionally, this list can be filtered by the
-- @region-opt-status-contains@ parameter.
--
-- This operation returns paginated results.
module Amazonka.Account.ListRegions
  ( -- * Creating a Request
    ListRegions (..),
    newListRegions,

    -- * Request Lenses
    listRegions_accountId,
    listRegions_maxResults,
    listRegions_nextToken,
    listRegions_regionOptStatusContains,

    -- * Destructuring the Response
    ListRegionsResponse (..),
    newListRegionsResponse,

    -- * Response Lenses
    listRegionsResponse_nextToken,
    listRegionsResponse_regions,
    listRegionsResponse_httpStatus,
  )
where

import Amazonka.Account.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRegions' smart constructor.
data ListRegions = ListRegions'
  { -- | Specifies the 12-digit account ID number of the Amazon Web Services
    -- account that you want to access or modify with this operation. If you
    -- don\'t specify this parameter, it defaults to the Amazon Web Services
    -- account of the identity used to call the operation. To use this
    -- parameter, the caller must be an identity in the
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#account organization\'s management account>
    -- or a delegated administrator account. The specified account ID must also
    -- be a member account in the same organization. The organization must have
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features enabled>,
    -- and the organization must have
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-trusted-access.html trusted access>
    -- enabled for the Account Management service, and optionally a
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-delegated-admin.html delegated admin>
    -- account assigned.
    --
    -- The management account can\'t specify its own @AccountId@. It must call
    -- the operation in standalone context by not including the @AccountId@
    -- parameter.
    --
    -- To call this operation on an account that is not a member of an
    -- organization, don\'t specify this parameter. Instead, call the operation
    -- using an identity belonging to the account whose contacts you wish to
    -- retrieve or modify.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The total number of items to return in the command’s output. If the
    -- total number of items available is more than the value specified, a
    -- @NextToken@ is provided in the command’s output. To resume pagination,
    -- provide the @NextToken@ value in the @starting-token@ argument of a
    -- subsequent command. Do not use the @NextToken@ response element directly
    -- outside of the Amazon Web Services CLI. For usage examples, see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/pagination.html Pagination>
    -- in the /Amazon Web Services Command Line Interface User Guide/.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token used to specify where to start paginating. This is the
    -- @NextToken@ from a previously truncated response. For usage examples,
    -- see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/pagination.html Pagination>
    -- in the /Amazon Web Services Command Line Interface User Guide/.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of Region statuses (Enabling, Enabled, Disabling, Disabled,
    -- Enabled_by_default) to use to filter the list of Regions for a given
    -- account. For example, passing in a value of ENABLING will only return a
    -- list of Regions with a Region status of ENABLING.
    regionOptStatusContains :: Prelude.Maybe [RegionOptStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRegions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'listRegions_accountId' - Specifies the 12-digit account ID number of the Amazon Web Services
-- account that you want to access or modify with this operation. If you
-- don\'t specify this parameter, it defaults to the Amazon Web Services
-- account of the identity used to call the operation. To use this
-- parameter, the caller must be an identity in the
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#account organization\'s management account>
-- or a delegated administrator account. The specified account ID must also
-- be a member account in the same organization. The organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features enabled>,
-- and the organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-trusted-access.html trusted access>
-- enabled for the Account Management service, and optionally a
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-delegated-admin.html delegated admin>
-- account assigned.
--
-- The management account can\'t specify its own @AccountId@. It must call
-- the operation in standalone context by not including the @AccountId@
-- parameter.
--
-- To call this operation on an account that is not a member of an
-- organization, don\'t specify this parameter. Instead, call the operation
-- using an identity belonging to the account whose contacts you wish to
-- retrieve or modify.
--
-- 'maxResults', 'listRegions_maxResults' - The total number of items to return in the command’s output. If the
-- total number of items available is more than the value specified, a
-- @NextToken@ is provided in the command’s output. To resume pagination,
-- provide the @NextToken@ value in the @starting-token@ argument of a
-- subsequent command. Do not use the @NextToken@ response element directly
-- outside of the Amazon Web Services CLI. For usage examples, see
-- <http://docs.aws.amazon.com/cli/latest/userguide/pagination.html Pagination>
-- in the /Amazon Web Services Command Line Interface User Guide/.
--
-- 'nextToken', 'listRegions_nextToken' - A token used to specify where to start paginating. This is the
-- @NextToken@ from a previously truncated response. For usage examples,
-- see
-- <http://docs.aws.amazon.com/cli/latest/userguide/pagination.html Pagination>
-- in the /Amazon Web Services Command Line Interface User Guide/.
--
-- 'regionOptStatusContains', 'listRegions_regionOptStatusContains' - A list of Region statuses (Enabling, Enabled, Disabling, Disabled,
-- Enabled_by_default) to use to filter the list of Regions for a given
-- account. For example, passing in a value of ENABLING will only return a
-- list of Regions with a Region status of ENABLING.
newListRegions ::
  ListRegions
newListRegions =
  ListRegions'
    { accountId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      regionOptStatusContains = Prelude.Nothing
    }

-- | Specifies the 12-digit account ID number of the Amazon Web Services
-- account that you want to access or modify with this operation. If you
-- don\'t specify this parameter, it defaults to the Amazon Web Services
-- account of the identity used to call the operation. To use this
-- parameter, the caller must be an identity in the
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#account organization\'s management account>
-- or a delegated administrator account. The specified account ID must also
-- be a member account in the same organization. The organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features enabled>,
-- and the organization must have
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-trusted-access.html trusted access>
-- enabled for the Account Management service, and optionally a
-- <https://docs.aws.amazon.com/organizations/latest/userguide/using-orgs-delegated-admin.html delegated admin>
-- account assigned.
--
-- The management account can\'t specify its own @AccountId@. It must call
-- the operation in standalone context by not including the @AccountId@
-- parameter.
--
-- To call this operation on an account that is not a member of an
-- organization, don\'t specify this parameter. Instead, call the operation
-- using an identity belonging to the account whose contacts you wish to
-- retrieve or modify.
listRegions_accountId :: Lens.Lens' ListRegions (Prelude.Maybe Prelude.Text)
listRegions_accountId = Lens.lens (\ListRegions' {accountId} -> accountId) (\s@ListRegions' {} a -> s {accountId = a} :: ListRegions)

-- | The total number of items to return in the command’s output. If the
-- total number of items available is more than the value specified, a
-- @NextToken@ is provided in the command’s output. To resume pagination,
-- provide the @NextToken@ value in the @starting-token@ argument of a
-- subsequent command. Do not use the @NextToken@ response element directly
-- outside of the Amazon Web Services CLI. For usage examples, see
-- <http://docs.aws.amazon.com/cli/latest/userguide/pagination.html Pagination>
-- in the /Amazon Web Services Command Line Interface User Guide/.
listRegions_maxResults :: Lens.Lens' ListRegions (Prelude.Maybe Prelude.Natural)
listRegions_maxResults = Lens.lens (\ListRegions' {maxResults} -> maxResults) (\s@ListRegions' {} a -> s {maxResults = a} :: ListRegions)

-- | A token used to specify where to start paginating. This is the
-- @NextToken@ from a previously truncated response. For usage examples,
-- see
-- <http://docs.aws.amazon.com/cli/latest/userguide/pagination.html Pagination>
-- in the /Amazon Web Services Command Line Interface User Guide/.
listRegions_nextToken :: Lens.Lens' ListRegions (Prelude.Maybe Prelude.Text)
listRegions_nextToken = Lens.lens (\ListRegions' {nextToken} -> nextToken) (\s@ListRegions' {} a -> s {nextToken = a} :: ListRegions)

-- | A list of Region statuses (Enabling, Enabled, Disabling, Disabled,
-- Enabled_by_default) to use to filter the list of Regions for a given
-- account. For example, passing in a value of ENABLING will only return a
-- list of Regions with a Region status of ENABLING.
listRegions_regionOptStatusContains :: Lens.Lens' ListRegions (Prelude.Maybe [RegionOptStatus])
listRegions_regionOptStatusContains = Lens.lens (\ListRegions' {regionOptStatusContains} -> regionOptStatusContains) (\s@ListRegions' {} a -> s {regionOptStatusContains = a} :: ListRegions) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager ListRegions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRegionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRegionsResponse_regions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listRegions_nextToken
          Lens..~ rs
          Lens.^? listRegionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListRegions where
  type AWSResponse ListRegions = ListRegionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRegionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Regions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRegions where
  hashWithSalt _salt ListRegions' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` regionOptStatusContains

instance Prelude.NFData ListRegions where
  rnf ListRegions' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf regionOptStatusContains

instance Data.ToHeaders ListRegions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRegions where
  toJSON ListRegions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("RegionOptStatusContains" Data..=)
              Prelude.<$> regionOptStatusContains
          ]
      )

instance Data.ToPath ListRegions where
  toPath = Prelude.const "/listRegions"

instance Data.ToQuery ListRegions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRegionsResponse' smart constructor.
data ListRegionsResponse = ListRegionsResponse'
  { -- | If there is more data to be returned, this will be populated. It should
    -- be passed into the @next-token@ request parameter of @list-regions@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This is a list of Regions for a given account, or if the filtered
    -- parameter was used, a list of Regions that match the filter criteria set
    -- in the @filter@ parameter.
    regions :: Prelude.Maybe [Region],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRegionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRegionsResponse_nextToken' - If there is more data to be returned, this will be populated. It should
-- be passed into the @next-token@ request parameter of @list-regions@.
--
-- 'regions', 'listRegionsResponse_regions' - This is a list of Regions for a given account, or if the filtered
-- parameter was used, a list of Regions that match the filter criteria set
-- in the @filter@ parameter.
--
-- 'httpStatus', 'listRegionsResponse_httpStatus' - The response's http status code.
newListRegionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRegionsResponse
newListRegionsResponse pHttpStatus_ =
  ListRegionsResponse'
    { nextToken = Prelude.Nothing,
      regions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there is more data to be returned, this will be populated. It should
-- be passed into the @next-token@ request parameter of @list-regions@.
listRegionsResponse_nextToken :: Lens.Lens' ListRegionsResponse (Prelude.Maybe Prelude.Text)
listRegionsResponse_nextToken = Lens.lens (\ListRegionsResponse' {nextToken} -> nextToken) (\s@ListRegionsResponse' {} a -> s {nextToken = a} :: ListRegionsResponse)

-- | This is a list of Regions for a given account, or if the filtered
-- parameter was used, a list of Regions that match the filter criteria set
-- in the @filter@ parameter.
listRegionsResponse_regions :: Lens.Lens' ListRegionsResponse (Prelude.Maybe [Region])
listRegionsResponse_regions = Lens.lens (\ListRegionsResponse' {regions} -> regions) (\s@ListRegionsResponse' {} a -> s {regions = a} :: ListRegionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRegionsResponse_httpStatus :: Lens.Lens' ListRegionsResponse Prelude.Int
listRegionsResponse_httpStatus = Lens.lens (\ListRegionsResponse' {httpStatus} -> httpStatus) (\s@ListRegionsResponse' {} a -> s {httpStatus = a} :: ListRegionsResponse)

instance Prelude.NFData ListRegionsResponse where
  rnf ListRegionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf httpStatus
