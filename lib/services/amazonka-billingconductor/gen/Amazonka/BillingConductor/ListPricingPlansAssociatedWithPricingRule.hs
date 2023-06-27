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
-- Module      : Amazonka.BillingConductor.ListPricingPlansAssociatedWithPricingRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of the pricing plans that are associated with a pricing rule.
--
-- This operation returns paginated results.
module Amazonka.BillingConductor.ListPricingPlansAssociatedWithPricingRule
  ( -- * Creating a Request
    ListPricingPlansAssociatedWithPricingRule (..),
    newListPricingPlansAssociatedWithPricingRule,

    -- * Request Lenses
    listPricingPlansAssociatedWithPricingRule_billingPeriod,
    listPricingPlansAssociatedWithPricingRule_maxResults,
    listPricingPlansAssociatedWithPricingRule_nextToken,
    listPricingPlansAssociatedWithPricingRule_pricingRuleArn,

    -- * Destructuring the Response
    ListPricingPlansAssociatedWithPricingRuleResponse (..),
    newListPricingPlansAssociatedWithPricingRuleResponse,

    -- * Response Lenses
    listPricingPlansAssociatedWithPricingRuleResponse_billingPeriod,
    listPricingPlansAssociatedWithPricingRuleResponse_nextToken,
    listPricingPlansAssociatedWithPricingRuleResponse_pricingPlanArns,
    listPricingPlansAssociatedWithPricingRuleResponse_pricingRuleArn,
    listPricingPlansAssociatedWithPricingRuleResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPricingPlansAssociatedWithPricingRule' smart constructor.
data ListPricingPlansAssociatedWithPricingRule = ListPricingPlansAssociatedWithPricingRule'
  { -- | The pricing plan billing period for which associations will be listed.
    billingPeriod :: Prelude.Maybe Prelude.Text,
    -- | The optional maximum number of pricing rule associations to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The optional pagination token returned by a previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The pricing rule Amazon Resource Name (ARN) for which associations will
    -- be listed.
    pricingRuleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPricingPlansAssociatedWithPricingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingPeriod', 'listPricingPlansAssociatedWithPricingRule_billingPeriod' - The pricing plan billing period for which associations will be listed.
--
-- 'maxResults', 'listPricingPlansAssociatedWithPricingRule_maxResults' - The optional maximum number of pricing rule associations to retrieve.
--
-- 'nextToken', 'listPricingPlansAssociatedWithPricingRule_nextToken' - The optional pagination token returned by a previous call.
--
-- 'pricingRuleArn', 'listPricingPlansAssociatedWithPricingRule_pricingRuleArn' - The pricing rule Amazon Resource Name (ARN) for which associations will
-- be listed.
newListPricingPlansAssociatedWithPricingRule ::
  -- | 'pricingRuleArn'
  Prelude.Text ->
  ListPricingPlansAssociatedWithPricingRule
newListPricingPlansAssociatedWithPricingRule
  pPricingRuleArn_ =
    ListPricingPlansAssociatedWithPricingRule'
      { billingPeriod =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        pricingRuleArn =
          pPricingRuleArn_
      }

-- | The pricing plan billing period for which associations will be listed.
listPricingPlansAssociatedWithPricingRule_billingPeriod :: Lens.Lens' ListPricingPlansAssociatedWithPricingRule (Prelude.Maybe Prelude.Text)
listPricingPlansAssociatedWithPricingRule_billingPeriod = Lens.lens (\ListPricingPlansAssociatedWithPricingRule' {billingPeriod} -> billingPeriod) (\s@ListPricingPlansAssociatedWithPricingRule' {} a -> s {billingPeriod = a} :: ListPricingPlansAssociatedWithPricingRule)

-- | The optional maximum number of pricing rule associations to retrieve.
listPricingPlansAssociatedWithPricingRule_maxResults :: Lens.Lens' ListPricingPlansAssociatedWithPricingRule (Prelude.Maybe Prelude.Natural)
listPricingPlansAssociatedWithPricingRule_maxResults = Lens.lens (\ListPricingPlansAssociatedWithPricingRule' {maxResults} -> maxResults) (\s@ListPricingPlansAssociatedWithPricingRule' {} a -> s {maxResults = a} :: ListPricingPlansAssociatedWithPricingRule)

-- | The optional pagination token returned by a previous call.
listPricingPlansAssociatedWithPricingRule_nextToken :: Lens.Lens' ListPricingPlansAssociatedWithPricingRule (Prelude.Maybe Prelude.Text)
listPricingPlansAssociatedWithPricingRule_nextToken = Lens.lens (\ListPricingPlansAssociatedWithPricingRule' {nextToken} -> nextToken) (\s@ListPricingPlansAssociatedWithPricingRule' {} a -> s {nextToken = a} :: ListPricingPlansAssociatedWithPricingRule)

-- | The pricing rule Amazon Resource Name (ARN) for which associations will
-- be listed.
listPricingPlansAssociatedWithPricingRule_pricingRuleArn :: Lens.Lens' ListPricingPlansAssociatedWithPricingRule Prelude.Text
listPricingPlansAssociatedWithPricingRule_pricingRuleArn = Lens.lens (\ListPricingPlansAssociatedWithPricingRule' {pricingRuleArn} -> pricingRuleArn) (\s@ListPricingPlansAssociatedWithPricingRule' {} a -> s {pricingRuleArn = a} :: ListPricingPlansAssociatedWithPricingRule)

instance
  Core.AWSPager
    ListPricingPlansAssociatedWithPricingRule
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPricingPlansAssociatedWithPricingRuleResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPricingPlansAssociatedWithPricingRuleResponse_pricingPlanArns
            Prelude.. Lens._Just
            Prelude.. Lens.to Prelude.toList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPricingPlansAssociatedWithPricingRule_nextToken
          Lens..~ rs
          Lens.^? listPricingPlansAssociatedWithPricingRuleResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListPricingPlansAssociatedWithPricingRule
  where
  type
    AWSResponse
      ListPricingPlansAssociatedWithPricingRule =
      ListPricingPlansAssociatedWithPricingRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPricingPlansAssociatedWithPricingRuleResponse'
            Prelude.<$> (x Data..?> "BillingPeriod")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "PricingPlanArns")
            Prelude.<*> (x Data..?> "PricingRuleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListPricingPlansAssociatedWithPricingRule
  where
  hashWithSalt
    _salt
    ListPricingPlansAssociatedWithPricingRule' {..} =
      _salt
        `Prelude.hashWithSalt` billingPeriod
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` pricingRuleArn

instance
  Prelude.NFData
    ListPricingPlansAssociatedWithPricingRule
  where
  rnf ListPricingPlansAssociatedWithPricingRule' {..} =
    Prelude.rnf billingPeriod
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pricingRuleArn

instance
  Data.ToHeaders
    ListPricingPlansAssociatedWithPricingRule
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
    ListPricingPlansAssociatedWithPricingRule
  where
  toJSON ListPricingPlansAssociatedWithPricingRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BillingPeriod" Data..=) Prelude.<$> billingPeriod,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("PricingRuleArn" Data..= pricingRuleArn)
          ]
      )

instance
  Data.ToPath
    ListPricingPlansAssociatedWithPricingRule
  where
  toPath =
    Prelude.const
      "/list-pricing-plans-associated-with-pricing-rule"

instance
  Data.ToQuery
    ListPricingPlansAssociatedWithPricingRule
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPricingPlansAssociatedWithPricingRuleResponse' smart constructor.
data ListPricingPlansAssociatedWithPricingRuleResponse = ListPricingPlansAssociatedWithPricingRuleResponse'
  { -- | The pricing plan billing period for which associations will be listed.
    billingPeriod :: Prelude.Maybe Prelude.Text,
    -- | The pagination token to be used on subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list containing pricing plans that are associated with the requested
    -- pricing rule.
    pricingPlanArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The pricing rule Amazon Resource Name (ARN) for which associations will
    -- be listed.
    pricingRuleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPricingPlansAssociatedWithPricingRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingPeriod', 'listPricingPlansAssociatedWithPricingRuleResponse_billingPeriod' - The pricing plan billing period for which associations will be listed.
--
-- 'nextToken', 'listPricingPlansAssociatedWithPricingRuleResponse_nextToken' - The pagination token to be used on subsequent calls.
--
-- 'pricingPlanArns', 'listPricingPlansAssociatedWithPricingRuleResponse_pricingPlanArns' - The list containing pricing plans that are associated with the requested
-- pricing rule.
--
-- 'pricingRuleArn', 'listPricingPlansAssociatedWithPricingRuleResponse_pricingRuleArn' - The pricing rule Amazon Resource Name (ARN) for which associations will
-- be listed.
--
-- 'httpStatus', 'listPricingPlansAssociatedWithPricingRuleResponse_httpStatus' - The response's http status code.
newListPricingPlansAssociatedWithPricingRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPricingPlansAssociatedWithPricingRuleResponse
newListPricingPlansAssociatedWithPricingRuleResponse
  pHttpStatus_ =
    ListPricingPlansAssociatedWithPricingRuleResponse'
      { billingPeriod =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        pricingPlanArns =
          Prelude.Nothing,
        pricingRuleArn =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The pricing plan billing period for which associations will be listed.
listPricingPlansAssociatedWithPricingRuleResponse_billingPeriod :: Lens.Lens' ListPricingPlansAssociatedWithPricingRuleResponse (Prelude.Maybe Prelude.Text)
listPricingPlansAssociatedWithPricingRuleResponse_billingPeriod = Lens.lens (\ListPricingPlansAssociatedWithPricingRuleResponse' {billingPeriod} -> billingPeriod) (\s@ListPricingPlansAssociatedWithPricingRuleResponse' {} a -> s {billingPeriod = a} :: ListPricingPlansAssociatedWithPricingRuleResponse)

-- | The pagination token to be used on subsequent calls.
listPricingPlansAssociatedWithPricingRuleResponse_nextToken :: Lens.Lens' ListPricingPlansAssociatedWithPricingRuleResponse (Prelude.Maybe Prelude.Text)
listPricingPlansAssociatedWithPricingRuleResponse_nextToken = Lens.lens (\ListPricingPlansAssociatedWithPricingRuleResponse' {nextToken} -> nextToken) (\s@ListPricingPlansAssociatedWithPricingRuleResponse' {} a -> s {nextToken = a} :: ListPricingPlansAssociatedWithPricingRuleResponse)

-- | The list containing pricing plans that are associated with the requested
-- pricing rule.
listPricingPlansAssociatedWithPricingRuleResponse_pricingPlanArns :: Lens.Lens' ListPricingPlansAssociatedWithPricingRuleResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listPricingPlansAssociatedWithPricingRuleResponse_pricingPlanArns = Lens.lens (\ListPricingPlansAssociatedWithPricingRuleResponse' {pricingPlanArns} -> pricingPlanArns) (\s@ListPricingPlansAssociatedWithPricingRuleResponse' {} a -> s {pricingPlanArns = a} :: ListPricingPlansAssociatedWithPricingRuleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pricing rule Amazon Resource Name (ARN) for which associations will
-- be listed.
listPricingPlansAssociatedWithPricingRuleResponse_pricingRuleArn :: Lens.Lens' ListPricingPlansAssociatedWithPricingRuleResponse (Prelude.Maybe Prelude.Text)
listPricingPlansAssociatedWithPricingRuleResponse_pricingRuleArn = Lens.lens (\ListPricingPlansAssociatedWithPricingRuleResponse' {pricingRuleArn} -> pricingRuleArn) (\s@ListPricingPlansAssociatedWithPricingRuleResponse' {} a -> s {pricingRuleArn = a} :: ListPricingPlansAssociatedWithPricingRuleResponse)

-- | The response's http status code.
listPricingPlansAssociatedWithPricingRuleResponse_httpStatus :: Lens.Lens' ListPricingPlansAssociatedWithPricingRuleResponse Prelude.Int
listPricingPlansAssociatedWithPricingRuleResponse_httpStatus = Lens.lens (\ListPricingPlansAssociatedWithPricingRuleResponse' {httpStatus} -> httpStatus) (\s@ListPricingPlansAssociatedWithPricingRuleResponse' {} a -> s {httpStatus = a} :: ListPricingPlansAssociatedWithPricingRuleResponse)

instance
  Prelude.NFData
    ListPricingPlansAssociatedWithPricingRuleResponse
  where
  rnf
    ListPricingPlansAssociatedWithPricingRuleResponse' {..} =
      Prelude.rnf billingPeriod
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf pricingPlanArns
        `Prelude.seq` Prelude.rnf pricingRuleArn
        `Prelude.seq` Prelude.rnf httpStatus
