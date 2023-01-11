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
-- Module      : Amazonka.BillingConductor.ListPricingPlans
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A paginated call to get pricing plans for the given billing period. If
-- you don\'t provide a billing period, the current billing period is used.
--
-- This operation returns paginated results.
module Amazonka.BillingConductor.ListPricingPlans
  ( -- * Creating a Request
    ListPricingPlans (..),
    newListPricingPlans,

    -- * Request Lenses
    listPricingPlans_billingPeriod,
    listPricingPlans_filters,
    listPricingPlans_maxResults,
    listPricingPlans_nextToken,

    -- * Destructuring the Response
    ListPricingPlansResponse (..),
    newListPricingPlansResponse,

    -- * Response Lenses
    listPricingPlansResponse_billingPeriod,
    listPricingPlansResponse_nextToken,
    listPricingPlansResponse_pricingPlans,
    listPricingPlansResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPricingPlans' smart constructor.
data ListPricingPlans = ListPricingPlans'
  { -- | The preferred billing period to get pricing plan.
    billingPeriod :: Prelude.Maybe Prelude.Text,
    -- | A @ListPricingPlansFilter@ that specifies the Amazon Resource Name
    -- (ARNs) of pricing plans to retrieve pricing plans information.
    filters :: Prelude.Maybe ListPricingPlansFilter,
    -- | The maximum number of pricing plans to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that\'s used on subsequent call to get pricing
    -- plans.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPricingPlans' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingPeriod', 'listPricingPlans_billingPeriod' - The preferred billing period to get pricing plan.
--
-- 'filters', 'listPricingPlans_filters' - A @ListPricingPlansFilter@ that specifies the Amazon Resource Name
-- (ARNs) of pricing plans to retrieve pricing plans information.
--
-- 'maxResults', 'listPricingPlans_maxResults' - The maximum number of pricing plans to retrieve.
--
-- 'nextToken', 'listPricingPlans_nextToken' - The pagination token that\'s used on subsequent call to get pricing
-- plans.
newListPricingPlans ::
  ListPricingPlans
newListPricingPlans =
  ListPricingPlans'
    { billingPeriod = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The preferred billing period to get pricing plan.
listPricingPlans_billingPeriod :: Lens.Lens' ListPricingPlans (Prelude.Maybe Prelude.Text)
listPricingPlans_billingPeriod = Lens.lens (\ListPricingPlans' {billingPeriod} -> billingPeriod) (\s@ListPricingPlans' {} a -> s {billingPeriod = a} :: ListPricingPlans)

-- | A @ListPricingPlansFilter@ that specifies the Amazon Resource Name
-- (ARNs) of pricing plans to retrieve pricing plans information.
listPricingPlans_filters :: Lens.Lens' ListPricingPlans (Prelude.Maybe ListPricingPlansFilter)
listPricingPlans_filters = Lens.lens (\ListPricingPlans' {filters} -> filters) (\s@ListPricingPlans' {} a -> s {filters = a} :: ListPricingPlans)

-- | The maximum number of pricing plans to retrieve.
listPricingPlans_maxResults :: Lens.Lens' ListPricingPlans (Prelude.Maybe Prelude.Natural)
listPricingPlans_maxResults = Lens.lens (\ListPricingPlans' {maxResults} -> maxResults) (\s@ListPricingPlans' {} a -> s {maxResults = a} :: ListPricingPlans)

-- | The pagination token that\'s used on subsequent call to get pricing
-- plans.
listPricingPlans_nextToken :: Lens.Lens' ListPricingPlans (Prelude.Maybe Prelude.Text)
listPricingPlans_nextToken = Lens.lens (\ListPricingPlans' {nextToken} -> nextToken) (\s@ListPricingPlans' {} a -> s {nextToken = a} :: ListPricingPlans)

instance Core.AWSPager ListPricingPlans where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPricingPlansResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPricingPlansResponse_pricingPlans
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPricingPlans_nextToken
          Lens..~ rs
          Lens.^? listPricingPlansResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPricingPlans where
  type
    AWSResponse ListPricingPlans =
      ListPricingPlansResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPricingPlansResponse'
            Prelude.<$> (x Data..?> "BillingPeriod")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "PricingPlans" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPricingPlans where
  hashWithSalt _salt ListPricingPlans' {..} =
    _salt `Prelude.hashWithSalt` billingPeriod
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListPricingPlans where
  rnf ListPricingPlans' {..} =
    Prelude.rnf billingPeriod
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListPricingPlans where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPricingPlans where
  toJSON ListPricingPlans' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BillingPeriod" Data..=) Prelude.<$> billingPeriod,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListPricingPlans where
  toPath = Prelude.const "/list-pricing-plans"

instance Data.ToQuery ListPricingPlans where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPricingPlansResponse' smart constructor.
data ListPricingPlansResponse = ListPricingPlansResponse'
  { -- | The billing period for which the described pricing plans are applicable.
    billingPeriod :: Prelude.Maybe Prelude.Text,
    -- | The pagination token that\'s used on subsequent calls to get pricing
    -- plans.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @PricingPlanListElement@ retrieved.
    pricingPlans :: Prelude.Maybe [PricingPlanListElement],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPricingPlansResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingPeriod', 'listPricingPlansResponse_billingPeriod' - The billing period for which the described pricing plans are applicable.
--
-- 'nextToken', 'listPricingPlansResponse_nextToken' - The pagination token that\'s used on subsequent calls to get pricing
-- plans.
--
-- 'pricingPlans', 'listPricingPlansResponse_pricingPlans' - A list of @PricingPlanListElement@ retrieved.
--
-- 'httpStatus', 'listPricingPlansResponse_httpStatus' - The response's http status code.
newListPricingPlansResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPricingPlansResponse
newListPricingPlansResponse pHttpStatus_ =
  ListPricingPlansResponse'
    { billingPeriod =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      pricingPlans = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The billing period for which the described pricing plans are applicable.
listPricingPlansResponse_billingPeriod :: Lens.Lens' ListPricingPlansResponse (Prelude.Maybe Prelude.Text)
listPricingPlansResponse_billingPeriod = Lens.lens (\ListPricingPlansResponse' {billingPeriod} -> billingPeriod) (\s@ListPricingPlansResponse' {} a -> s {billingPeriod = a} :: ListPricingPlansResponse)

-- | The pagination token that\'s used on subsequent calls to get pricing
-- plans.
listPricingPlansResponse_nextToken :: Lens.Lens' ListPricingPlansResponse (Prelude.Maybe Prelude.Text)
listPricingPlansResponse_nextToken = Lens.lens (\ListPricingPlansResponse' {nextToken} -> nextToken) (\s@ListPricingPlansResponse' {} a -> s {nextToken = a} :: ListPricingPlansResponse)

-- | A list of @PricingPlanListElement@ retrieved.
listPricingPlansResponse_pricingPlans :: Lens.Lens' ListPricingPlansResponse (Prelude.Maybe [PricingPlanListElement])
listPricingPlansResponse_pricingPlans = Lens.lens (\ListPricingPlansResponse' {pricingPlans} -> pricingPlans) (\s@ListPricingPlansResponse' {} a -> s {pricingPlans = a} :: ListPricingPlansResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPricingPlansResponse_httpStatus :: Lens.Lens' ListPricingPlansResponse Prelude.Int
listPricingPlansResponse_httpStatus = Lens.lens (\ListPricingPlansResponse' {httpStatus} -> httpStatus) (\s@ListPricingPlansResponse' {} a -> s {httpStatus = a} :: ListPricingPlansResponse)

instance Prelude.NFData ListPricingPlansResponse where
  rnf ListPricingPlansResponse' {..} =
    Prelude.rnf billingPeriod
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pricingPlans
      `Prelude.seq` Prelude.rnf httpStatus
