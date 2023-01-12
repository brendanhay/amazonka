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
-- Module      : Amazonka.BillingConductor.ListPricingRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a pricing rule that can be associated to a pricing plan, or
-- set of pricing plans.
--
-- This operation returns paginated results.
module Amazonka.BillingConductor.ListPricingRules
  ( -- * Creating a Request
    ListPricingRules (..),
    newListPricingRules,

    -- * Request Lenses
    listPricingRules_billingPeriod,
    listPricingRules_filters,
    listPricingRules_maxResults,
    listPricingRules_nextToken,

    -- * Destructuring the Response
    ListPricingRulesResponse (..),
    newListPricingRulesResponse,

    -- * Response Lenses
    listPricingRulesResponse_billingPeriod,
    listPricingRulesResponse_nextToken,
    listPricingRulesResponse_pricingRules,
    listPricingRulesResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPricingRules' smart constructor.
data ListPricingRules = ListPricingRules'
  { -- | The preferred billing period to get the pricing plan.
    billingPeriod :: Prelude.Maybe Prelude.Text,
    -- | A @DescribePricingRuleFilter@ that specifies the Amazon Resource Name
    -- (ARNs) of pricing rules to retrieve pricing rules information.
    filters :: Prelude.Maybe ListPricingRulesFilter,
    -- | The maximum number of pricing rules to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that\'s used on subsequent call to get pricing
    -- rules.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPricingRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingPeriod', 'listPricingRules_billingPeriod' - The preferred billing period to get the pricing plan.
--
-- 'filters', 'listPricingRules_filters' - A @DescribePricingRuleFilter@ that specifies the Amazon Resource Name
-- (ARNs) of pricing rules to retrieve pricing rules information.
--
-- 'maxResults', 'listPricingRules_maxResults' - The maximum number of pricing rules to retrieve.
--
-- 'nextToken', 'listPricingRules_nextToken' - The pagination token that\'s used on subsequent call to get pricing
-- rules.
newListPricingRules ::
  ListPricingRules
newListPricingRules =
  ListPricingRules'
    { billingPeriod = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The preferred billing period to get the pricing plan.
listPricingRules_billingPeriod :: Lens.Lens' ListPricingRules (Prelude.Maybe Prelude.Text)
listPricingRules_billingPeriod = Lens.lens (\ListPricingRules' {billingPeriod} -> billingPeriod) (\s@ListPricingRules' {} a -> s {billingPeriod = a} :: ListPricingRules)

-- | A @DescribePricingRuleFilter@ that specifies the Amazon Resource Name
-- (ARNs) of pricing rules to retrieve pricing rules information.
listPricingRules_filters :: Lens.Lens' ListPricingRules (Prelude.Maybe ListPricingRulesFilter)
listPricingRules_filters = Lens.lens (\ListPricingRules' {filters} -> filters) (\s@ListPricingRules' {} a -> s {filters = a} :: ListPricingRules)

-- | The maximum number of pricing rules to retrieve.
listPricingRules_maxResults :: Lens.Lens' ListPricingRules (Prelude.Maybe Prelude.Natural)
listPricingRules_maxResults = Lens.lens (\ListPricingRules' {maxResults} -> maxResults) (\s@ListPricingRules' {} a -> s {maxResults = a} :: ListPricingRules)

-- | The pagination token that\'s used on subsequent call to get pricing
-- rules.
listPricingRules_nextToken :: Lens.Lens' ListPricingRules (Prelude.Maybe Prelude.Text)
listPricingRules_nextToken = Lens.lens (\ListPricingRules' {nextToken} -> nextToken) (\s@ListPricingRules' {} a -> s {nextToken = a} :: ListPricingRules)

instance Core.AWSPager ListPricingRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPricingRulesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPricingRulesResponse_pricingRules
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPricingRules_nextToken
          Lens..~ rs
          Lens.^? listPricingRulesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPricingRules where
  type
    AWSResponse ListPricingRules =
      ListPricingRulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPricingRulesResponse'
            Prelude.<$> (x Data..?> "BillingPeriod")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "PricingRules" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPricingRules where
  hashWithSalt _salt ListPricingRules' {..} =
    _salt `Prelude.hashWithSalt` billingPeriod
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListPricingRules where
  rnf ListPricingRules' {..} =
    Prelude.rnf billingPeriod
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListPricingRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPricingRules where
  toJSON ListPricingRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BillingPeriod" Data..=) Prelude.<$> billingPeriod,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListPricingRules where
  toPath = Prelude.const "/list-pricing-rules"

instance Data.ToQuery ListPricingRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPricingRulesResponse' smart constructor.
data ListPricingRulesResponse = ListPricingRulesResponse'
  { -- | The billing period for which the described pricing rules are applicable.
    billingPeriod :: Prelude.Maybe Prelude.Text,
    -- | The pagination token that\'s used on subsequent calls to get pricing
    -- rules.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing the described pricing rules.
    pricingRules :: Prelude.Maybe [PricingRuleListElement],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPricingRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingPeriod', 'listPricingRulesResponse_billingPeriod' - The billing period for which the described pricing rules are applicable.
--
-- 'nextToken', 'listPricingRulesResponse_nextToken' - The pagination token that\'s used on subsequent calls to get pricing
-- rules.
--
-- 'pricingRules', 'listPricingRulesResponse_pricingRules' - A list containing the described pricing rules.
--
-- 'httpStatus', 'listPricingRulesResponse_httpStatus' - The response's http status code.
newListPricingRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPricingRulesResponse
newListPricingRulesResponse pHttpStatus_ =
  ListPricingRulesResponse'
    { billingPeriod =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      pricingRules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The billing period for which the described pricing rules are applicable.
listPricingRulesResponse_billingPeriod :: Lens.Lens' ListPricingRulesResponse (Prelude.Maybe Prelude.Text)
listPricingRulesResponse_billingPeriod = Lens.lens (\ListPricingRulesResponse' {billingPeriod} -> billingPeriod) (\s@ListPricingRulesResponse' {} a -> s {billingPeriod = a} :: ListPricingRulesResponse)

-- | The pagination token that\'s used on subsequent calls to get pricing
-- rules.
listPricingRulesResponse_nextToken :: Lens.Lens' ListPricingRulesResponse (Prelude.Maybe Prelude.Text)
listPricingRulesResponse_nextToken = Lens.lens (\ListPricingRulesResponse' {nextToken} -> nextToken) (\s@ListPricingRulesResponse' {} a -> s {nextToken = a} :: ListPricingRulesResponse)

-- | A list containing the described pricing rules.
listPricingRulesResponse_pricingRules :: Lens.Lens' ListPricingRulesResponse (Prelude.Maybe [PricingRuleListElement])
listPricingRulesResponse_pricingRules = Lens.lens (\ListPricingRulesResponse' {pricingRules} -> pricingRules) (\s@ListPricingRulesResponse' {} a -> s {pricingRules = a} :: ListPricingRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPricingRulesResponse_httpStatus :: Lens.Lens' ListPricingRulesResponse Prelude.Int
listPricingRulesResponse_httpStatus = Lens.lens (\ListPricingRulesResponse' {httpStatus} -> httpStatus) (\s@ListPricingRulesResponse' {} a -> s {httpStatus = a} :: ListPricingRulesResponse)

instance Prelude.NFData ListPricingRulesResponse where
  rnf ListPricingRulesResponse' {..} =
    Prelude.rnf billingPeriod
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pricingRules
      `Prelude.seq` Prelude.rnf httpStatus
