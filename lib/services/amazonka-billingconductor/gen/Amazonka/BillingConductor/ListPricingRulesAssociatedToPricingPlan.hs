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
-- Module      : Amazonka.BillingConductor.ListPricingRulesAssociatedToPricingPlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the pricing rules that are associated with a pricing plan.
--
-- This operation returns paginated results.
module Amazonka.BillingConductor.ListPricingRulesAssociatedToPricingPlan
  ( -- * Creating a Request
    ListPricingRulesAssociatedToPricingPlan (..),
    newListPricingRulesAssociatedToPricingPlan,

    -- * Request Lenses
    listPricingRulesAssociatedToPricingPlan_billingPeriod,
    listPricingRulesAssociatedToPricingPlan_maxResults,
    listPricingRulesAssociatedToPricingPlan_nextToken,
    listPricingRulesAssociatedToPricingPlan_pricingPlanArn,

    -- * Destructuring the Response
    ListPricingRulesAssociatedToPricingPlanResponse (..),
    newListPricingRulesAssociatedToPricingPlanResponse,

    -- * Response Lenses
    listPricingRulesAssociatedToPricingPlanResponse_billingPeriod,
    listPricingRulesAssociatedToPricingPlanResponse_nextToken,
    listPricingRulesAssociatedToPricingPlanResponse_pricingPlanArn,
    listPricingRulesAssociatedToPricingPlanResponse_pricingRuleArns,
    listPricingRulesAssociatedToPricingPlanResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPricingRulesAssociatedToPricingPlan' smart constructor.
data ListPricingRulesAssociatedToPricingPlan = ListPricingRulesAssociatedToPricingPlan'
  { -- | The billing period for which the pricing rule associations are to be
    -- listed.
    billingPeriod :: Prelude.Maybe Prelude.Text,
    -- | The optional maximum number of pricing rule associations to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The optional pagination token returned by a previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the pricing plan for which
    -- associations are to be listed.
    pricingPlanArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPricingRulesAssociatedToPricingPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingPeriod', 'listPricingRulesAssociatedToPricingPlan_billingPeriod' - The billing period for which the pricing rule associations are to be
-- listed.
--
-- 'maxResults', 'listPricingRulesAssociatedToPricingPlan_maxResults' - The optional maximum number of pricing rule associations to retrieve.
--
-- 'nextToken', 'listPricingRulesAssociatedToPricingPlan_nextToken' - The optional pagination token returned by a previous call.
--
-- 'pricingPlanArn', 'listPricingRulesAssociatedToPricingPlan_pricingPlanArn' - The Amazon Resource Name (ARN) of the pricing plan for which
-- associations are to be listed.
newListPricingRulesAssociatedToPricingPlan ::
  -- | 'pricingPlanArn'
  Prelude.Text ->
  ListPricingRulesAssociatedToPricingPlan
newListPricingRulesAssociatedToPricingPlan
  pPricingPlanArn_ =
    ListPricingRulesAssociatedToPricingPlan'
      { billingPeriod =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        pricingPlanArn = pPricingPlanArn_
      }

-- | The billing period for which the pricing rule associations are to be
-- listed.
listPricingRulesAssociatedToPricingPlan_billingPeriod :: Lens.Lens' ListPricingRulesAssociatedToPricingPlan (Prelude.Maybe Prelude.Text)
listPricingRulesAssociatedToPricingPlan_billingPeriod = Lens.lens (\ListPricingRulesAssociatedToPricingPlan' {billingPeriod} -> billingPeriod) (\s@ListPricingRulesAssociatedToPricingPlan' {} a -> s {billingPeriod = a} :: ListPricingRulesAssociatedToPricingPlan)

-- | The optional maximum number of pricing rule associations to retrieve.
listPricingRulesAssociatedToPricingPlan_maxResults :: Lens.Lens' ListPricingRulesAssociatedToPricingPlan (Prelude.Maybe Prelude.Natural)
listPricingRulesAssociatedToPricingPlan_maxResults = Lens.lens (\ListPricingRulesAssociatedToPricingPlan' {maxResults} -> maxResults) (\s@ListPricingRulesAssociatedToPricingPlan' {} a -> s {maxResults = a} :: ListPricingRulesAssociatedToPricingPlan)

-- | The optional pagination token returned by a previous call.
listPricingRulesAssociatedToPricingPlan_nextToken :: Lens.Lens' ListPricingRulesAssociatedToPricingPlan (Prelude.Maybe Prelude.Text)
listPricingRulesAssociatedToPricingPlan_nextToken = Lens.lens (\ListPricingRulesAssociatedToPricingPlan' {nextToken} -> nextToken) (\s@ListPricingRulesAssociatedToPricingPlan' {} a -> s {nextToken = a} :: ListPricingRulesAssociatedToPricingPlan)

-- | The Amazon Resource Name (ARN) of the pricing plan for which
-- associations are to be listed.
listPricingRulesAssociatedToPricingPlan_pricingPlanArn :: Lens.Lens' ListPricingRulesAssociatedToPricingPlan Prelude.Text
listPricingRulesAssociatedToPricingPlan_pricingPlanArn = Lens.lens (\ListPricingRulesAssociatedToPricingPlan' {pricingPlanArn} -> pricingPlanArn) (\s@ListPricingRulesAssociatedToPricingPlan' {} a -> s {pricingPlanArn = a} :: ListPricingRulesAssociatedToPricingPlan)

instance
  Core.AWSPager
    ListPricingRulesAssociatedToPricingPlan
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPricingRulesAssociatedToPricingPlanResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPricingRulesAssociatedToPricingPlanResponse_pricingRuleArns
              Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPricingRulesAssociatedToPricingPlan_nextToken
          Lens..~ rs
            Lens.^? listPricingRulesAssociatedToPricingPlanResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListPricingRulesAssociatedToPricingPlan
  where
  type
    AWSResponse
      ListPricingRulesAssociatedToPricingPlan =
      ListPricingRulesAssociatedToPricingPlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPricingRulesAssociatedToPricingPlanResponse'
            Prelude.<$> (x Data..?> "BillingPeriod")
              Prelude.<*> (x Data..?> "NextToken")
              Prelude.<*> (x Data..?> "PricingPlanArn")
              Prelude.<*> (x Data..?> "PricingRuleArns")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListPricingRulesAssociatedToPricingPlan
  where
  hashWithSalt
    _salt
    ListPricingRulesAssociatedToPricingPlan' {..} =
      _salt `Prelude.hashWithSalt` billingPeriod
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` pricingPlanArn

instance
  Prelude.NFData
    ListPricingRulesAssociatedToPricingPlan
  where
  rnf ListPricingRulesAssociatedToPricingPlan' {..} =
    Prelude.rnf billingPeriod
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pricingPlanArn

instance
  Data.ToHeaders
    ListPricingRulesAssociatedToPricingPlan
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListPricingRulesAssociatedToPricingPlan
  where
  toJSON ListPricingRulesAssociatedToPricingPlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BillingPeriod" Data..=) Prelude.<$> billingPeriod,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("PricingPlanArn" Data..= pricingPlanArn)
          ]
      )

instance
  Data.ToPath
    ListPricingRulesAssociatedToPricingPlan
  where
  toPath =
    Prelude.const
      "/list-pricing-rules-associated-to-pricing-plan"

instance
  Data.ToQuery
    ListPricingRulesAssociatedToPricingPlan
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPricingRulesAssociatedToPricingPlanResponse' smart constructor.
data ListPricingRulesAssociatedToPricingPlanResponse = ListPricingRulesAssociatedToPricingPlanResponse'
  { -- | The billing period for which the pricing rule associations are listed.
    billingPeriod :: Prelude.Maybe Prelude.Text,
    -- | The pagination token to be used on subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the pricing plan for which
    -- associations are listed.
    pricingPlanArn :: Prelude.Maybe Prelude.Text,
    -- | A list containing pricing rules that are associated with the requested
    -- pricing plan.
    pricingRuleArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPricingRulesAssociatedToPricingPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingPeriod', 'listPricingRulesAssociatedToPricingPlanResponse_billingPeriod' - The billing period for which the pricing rule associations are listed.
--
-- 'nextToken', 'listPricingRulesAssociatedToPricingPlanResponse_nextToken' - The pagination token to be used on subsequent calls.
--
-- 'pricingPlanArn', 'listPricingRulesAssociatedToPricingPlanResponse_pricingPlanArn' - The Amazon Resource Name (ARN) of the pricing plan for which
-- associations are listed.
--
-- 'pricingRuleArns', 'listPricingRulesAssociatedToPricingPlanResponse_pricingRuleArns' - A list containing pricing rules that are associated with the requested
-- pricing plan.
--
-- 'httpStatus', 'listPricingRulesAssociatedToPricingPlanResponse_httpStatus' - The response's http status code.
newListPricingRulesAssociatedToPricingPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPricingRulesAssociatedToPricingPlanResponse
newListPricingRulesAssociatedToPricingPlanResponse
  pHttpStatus_ =
    ListPricingRulesAssociatedToPricingPlanResponse'
      { billingPeriod =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        pricingPlanArn =
          Prelude.Nothing,
        pricingRuleArns =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The billing period for which the pricing rule associations are listed.
listPricingRulesAssociatedToPricingPlanResponse_billingPeriod :: Lens.Lens' ListPricingRulesAssociatedToPricingPlanResponse (Prelude.Maybe Prelude.Text)
listPricingRulesAssociatedToPricingPlanResponse_billingPeriod = Lens.lens (\ListPricingRulesAssociatedToPricingPlanResponse' {billingPeriod} -> billingPeriod) (\s@ListPricingRulesAssociatedToPricingPlanResponse' {} a -> s {billingPeriod = a} :: ListPricingRulesAssociatedToPricingPlanResponse)

-- | The pagination token to be used on subsequent calls.
listPricingRulesAssociatedToPricingPlanResponse_nextToken :: Lens.Lens' ListPricingRulesAssociatedToPricingPlanResponse (Prelude.Maybe Prelude.Text)
listPricingRulesAssociatedToPricingPlanResponse_nextToken = Lens.lens (\ListPricingRulesAssociatedToPricingPlanResponse' {nextToken} -> nextToken) (\s@ListPricingRulesAssociatedToPricingPlanResponse' {} a -> s {nextToken = a} :: ListPricingRulesAssociatedToPricingPlanResponse)

-- | The Amazon Resource Name (ARN) of the pricing plan for which
-- associations are listed.
listPricingRulesAssociatedToPricingPlanResponse_pricingPlanArn :: Lens.Lens' ListPricingRulesAssociatedToPricingPlanResponse (Prelude.Maybe Prelude.Text)
listPricingRulesAssociatedToPricingPlanResponse_pricingPlanArn = Lens.lens (\ListPricingRulesAssociatedToPricingPlanResponse' {pricingPlanArn} -> pricingPlanArn) (\s@ListPricingRulesAssociatedToPricingPlanResponse' {} a -> s {pricingPlanArn = a} :: ListPricingRulesAssociatedToPricingPlanResponse)

-- | A list containing pricing rules that are associated with the requested
-- pricing plan.
listPricingRulesAssociatedToPricingPlanResponse_pricingRuleArns :: Lens.Lens' ListPricingRulesAssociatedToPricingPlanResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listPricingRulesAssociatedToPricingPlanResponse_pricingRuleArns = Lens.lens (\ListPricingRulesAssociatedToPricingPlanResponse' {pricingRuleArns} -> pricingRuleArns) (\s@ListPricingRulesAssociatedToPricingPlanResponse' {} a -> s {pricingRuleArns = a} :: ListPricingRulesAssociatedToPricingPlanResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPricingRulesAssociatedToPricingPlanResponse_httpStatus :: Lens.Lens' ListPricingRulesAssociatedToPricingPlanResponse Prelude.Int
listPricingRulesAssociatedToPricingPlanResponse_httpStatus = Lens.lens (\ListPricingRulesAssociatedToPricingPlanResponse' {httpStatus} -> httpStatus) (\s@ListPricingRulesAssociatedToPricingPlanResponse' {} a -> s {httpStatus = a} :: ListPricingRulesAssociatedToPricingPlanResponse)

instance
  Prelude.NFData
    ListPricingRulesAssociatedToPricingPlanResponse
  where
  rnf
    ListPricingRulesAssociatedToPricingPlanResponse' {..} =
      Prelude.rnf billingPeriod
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf pricingPlanArn
        `Prelude.seq` Prelude.rnf pricingRuleArns
        `Prelude.seq` Prelude.rnf httpStatus
