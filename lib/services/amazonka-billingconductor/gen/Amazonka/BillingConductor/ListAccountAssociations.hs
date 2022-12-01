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
-- Module      : Amazonka.BillingConductor.ListAccountAssociations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is a paginated call to list linked accounts that are linked to the
-- payer account for the specified time period. If no information is
-- provided, the current billing period is used. The response will
-- optionally include the billing group that\'s associated with the linked
-- account.
--
-- This operation returns paginated results.
module Amazonka.BillingConductor.ListAccountAssociations
  ( -- * Creating a Request
    ListAccountAssociations (..),
    newListAccountAssociations,

    -- * Request Lenses
    listAccountAssociations_nextToken,
    listAccountAssociations_billingPeriod,
    listAccountAssociations_filters,

    -- * Destructuring the Response
    ListAccountAssociationsResponse (..),
    newListAccountAssociationsResponse,

    -- * Response Lenses
    listAccountAssociationsResponse_nextToken,
    listAccountAssociationsResponse_linkedAccounts,
    listAccountAssociationsResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAccountAssociations' smart constructor.
data ListAccountAssociations = ListAccountAssociations'
  { -- | The pagination token that\'s used on subsequent calls to retrieve
    -- accounts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The preferred billing period to get account associations.
    billingPeriod :: Prelude.Maybe Prelude.Text,
    -- | The filter on the account ID of the linked account, or any of the
    -- following:
    --
    -- @MONITORED@: linked accounts that are associated to billing groups.
    --
    -- @UNMONITORED@: linked accounts that aren\'t associated to billing
    -- groups.
    --
    -- @Billing Group Arn@: linked accounts that are associated to the provided
    -- billing group Arn.
    filters :: Prelude.Maybe ListAccountAssociationsFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccountAssociations_nextToken' - The pagination token that\'s used on subsequent calls to retrieve
-- accounts.
--
-- 'billingPeriod', 'listAccountAssociations_billingPeriod' - The preferred billing period to get account associations.
--
-- 'filters', 'listAccountAssociations_filters' - The filter on the account ID of the linked account, or any of the
-- following:
--
-- @MONITORED@: linked accounts that are associated to billing groups.
--
-- @UNMONITORED@: linked accounts that aren\'t associated to billing
-- groups.
--
-- @Billing Group Arn@: linked accounts that are associated to the provided
-- billing group Arn.
newListAccountAssociations ::
  ListAccountAssociations
newListAccountAssociations =
  ListAccountAssociations'
    { nextToken =
        Prelude.Nothing,
      billingPeriod = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The pagination token that\'s used on subsequent calls to retrieve
-- accounts.
listAccountAssociations_nextToken :: Lens.Lens' ListAccountAssociations (Prelude.Maybe Prelude.Text)
listAccountAssociations_nextToken = Lens.lens (\ListAccountAssociations' {nextToken} -> nextToken) (\s@ListAccountAssociations' {} a -> s {nextToken = a} :: ListAccountAssociations)

-- | The preferred billing period to get account associations.
listAccountAssociations_billingPeriod :: Lens.Lens' ListAccountAssociations (Prelude.Maybe Prelude.Text)
listAccountAssociations_billingPeriod = Lens.lens (\ListAccountAssociations' {billingPeriod} -> billingPeriod) (\s@ListAccountAssociations' {} a -> s {billingPeriod = a} :: ListAccountAssociations)

-- | The filter on the account ID of the linked account, or any of the
-- following:
--
-- @MONITORED@: linked accounts that are associated to billing groups.
--
-- @UNMONITORED@: linked accounts that aren\'t associated to billing
-- groups.
--
-- @Billing Group Arn@: linked accounts that are associated to the provided
-- billing group Arn.
listAccountAssociations_filters :: Lens.Lens' ListAccountAssociations (Prelude.Maybe ListAccountAssociationsFilter)
listAccountAssociations_filters = Lens.lens (\ListAccountAssociations' {filters} -> filters) (\s@ListAccountAssociations' {} a -> s {filters = a} :: ListAccountAssociations)

instance Core.AWSPager ListAccountAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccountAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAccountAssociationsResponse_linkedAccounts
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAccountAssociations_nextToken
          Lens..~ rs
          Lens.^? listAccountAssociationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAccountAssociations where
  type
    AWSResponse ListAccountAssociations =
      ListAccountAssociationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccountAssociationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "LinkedAccounts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAccountAssociations where
  hashWithSalt _salt ListAccountAssociations' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` billingPeriod
      `Prelude.hashWithSalt` filters

instance Prelude.NFData ListAccountAssociations where
  rnf ListAccountAssociations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf billingPeriod
      `Prelude.seq` Prelude.rnf filters

instance Core.ToHeaders ListAccountAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListAccountAssociations where
  toJSON ListAccountAssociations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("BillingPeriod" Core..=) Prelude.<$> billingPeriod,
            ("Filters" Core..=) Prelude.<$> filters
          ]
      )

instance Core.ToPath ListAccountAssociations where
  toPath = Prelude.const "/list-account-associations"

instance Core.ToQuery ListAccountAssociations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAccountAssociationsResponse' smart constructor.
data ListAccountAssociationsResponse = ListAccountAssociationsResponse'
  { -- | The pagination token that\'s used on subsequent calls to get accounts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of linked accounts in the payer account.
    linkedAccounts :: Prelude.Maybe [AccountAssociationsListElement],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccountAssociationsResponse_nextToken' - The pagination token that\'s used on subsequent calls to get accounts.
--
-- 'linkedAccounts', 'listAccountAssociationsResponse_linkedAccounts' - The list of linked accounts in the payer account.
--
-- 'httpStatus', 'listAccountAssociationsResponse_httpStatus' - The response's http status code.
newListAccountAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccountAssociationsResponse
newListAccountAssociationsResponse pHttpStatus_ =
  ListAccountAssociationsResponse'
    { nextToken =
        Prelude.Nothing,
      linkedAccounts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that\'s used on subsequent calls to get accounts.
listAccountAssociationsResponse_nextToken :: Lens.Lens' ListAccountAssociationsResponse (Prelude.Maybe Prelude.Text)
listAccountAssociationsResponse_nextToken = Lens.lens (\ListAccountAssociationsResponse' {nextToken} -> nextToken) (\s@ListAccountAssociationsResponse' {} a -> s {nextToken = a} :: ListAccountAssociationsResponse)

-- | The list of linked accounts in the payer account.
listAccountAssociationsResponse_linkedAccounts :: Lens.Lens' ListAccountAssociationsResponse (Prelude.Maybe [AccountAssociationsListElement])
listAccountAssociationsResponse_linkedAccounts = Lens.lens (\ListAccountAssociationsResponse' {linkedAccounts} -> linkedAccounts) (\s@ListAccountAssociationsResponse' {} a -> s {linkedAccounts = a} :: ListAccountAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAccountAssociationsResponse_httpStatus :: Lens.Lens' ListAccountAssociationsResponse Prelude.Int
listAccountAssociationsResponse_httpStatus = Lens.lens (\ListAccountAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListAccountAssociationsResponse' {} a -> s {httpStatus = a} :: ListAccountAssociationsResponse)

instance
  Prelude.NFData
    ListAccountAssociationsResponse
  where
  rnf ListAccountAssociationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf linkedAccounts
      `Prelude.seq` Prelude.rnf httpStatus
