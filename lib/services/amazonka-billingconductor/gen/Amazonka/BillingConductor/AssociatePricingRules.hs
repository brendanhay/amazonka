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
-- Module      : Amazonka.BillingConductor.AssociatePricingRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Connects an array of @PricingRuleArns@ to a defined @PricingPlan@. The
-- maximum number @PricingRuleArn@ that can be associated in one call is
-- 30.
module Amazonka.BillingConductor.AssociatePricingRules
  ( -- * Creating a Request
    AssociatePricingRules (..),
    newAssociatePricingRules,

    -- * Request Lenses
    associatePricingRules_arn,
    associatePricingRules_pricingRuleArns,

    -- * Destructuring the Response
    AssociatePricingRulesResponse (..),
    newAssociatePricingRulesResponse,

    -- * Response Lenses
    associatePricingRulesResponse_arn,
    associatePricingRulesResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociatePricingRules' smart constructor.
data AssociatePricingRules = AssociatePricingRules'
  { -- | The @PricingPlanArn@ that the @PricingRuleArns@ are associated with.
    arn :: Prelude.Text,
    -- | The @PricingRuleArns@ that are associated with the Pricing Plan.
    pricingRuleArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePricingRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'associatePricingRules_arn' - The @PricingPlanArn@ that the @PricingRuleArns@ are associated with.
--
-- 'pricingRuleArns', 'associatePricingRules_pricingRuleArns' - The @PricingRuleArns@ that are associated with the Pricing Plan.
newAssociatePricingRules ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'pricingRuleArns'
  Prelude.NonEmpty Prelude.Text ->
  AssociatePricingRules
newAssociatePricingRules pArn_ pPricingRuleArns_ =
  AssociatePricingRules'
    { arn = pArn_,
      pricingRuleArns =
        Lens.coerced Lens.# pPricingRuleArns_
    }

-- | The @PricingPlanArn@ that the @PricingRuleArns@ are associated with.
associatePricingRules_arn :: Lens.Lens' AssociatePricingRules Prelude.Text
associatePricingRules_arn = Lens.lens (\AssociatePricingRules' {arn} -> arn) (\s@AssociatePricingRules' {} a -> s {arn = a} :: AssociatePricingRules)

-- | The @PricingRuleArns@ that are associated with the Pricing Plan.
associatePricingRules_pricingRuleArns :: Lens.Lens' AssociatePricingRules (Prelude.NonEmpty Prelude.Text)
associatePricingRules_pricingRuleArns = Lens.lens (\AssociatePricingRules' {pricingRuleArns} -> pricingRuleArns) (\s@AssociatePricingRules' {} a -> s {pricingRuleArns = a} :: AssociatePricingRules) Prelude.. Lens.coerced

instance Core.AWSRequest AssociatePricingRules where
  type
    AWSResponse AssociatePricingRules =
      AssociatePricingRulesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociatePricingRulesResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociatePricingRules where
  hashWithSalt _salt AssociatePricingRules' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` pricingRuleArns

instance Prelude.NFData AssociatePricingRules where
  rnf AssociatePricingRules' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf pricingRuleArns

instance Data.ToHeaders AssociatePricingRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociatePricingRules where
  toJSON AssociatePricingRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Arn" Data..= arn),
            Prelude.Just
              ("PricingRuleArns" Data..= pricingRuleArns)
          ]
      )

instance Data.ToPath AssociatePricingRules where
  toPath = Prelude.const "/associate-pricing-rules"

instance Data.ToQuery AssociatePricingRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociatePricingRulesResponse' smart constructor.
data AssociatePricingRulesResponse = AssociatePricingRulesResponse'
  { -- | The @PricingPlanArn@ that the @PricingRuleArns@ are associated with.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePricingRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'associatePricingRulesResponse_arn' - The @PricingPlanArn@ that the @PricingRuleArns@ are associated with.
--
-- 'httpStatus', 'associatePricingRulesResponse_httpStatus' - The response's http status code.
newAssociatePricingRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociatePricingRulesResponse
newAssociatePricingRulesResponse pHttpStatus_ =
  AssociatePricingRulesResponse'
    { arn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @PricingPlanArn@ that the @PricingRuleArns@ are associated with.
associatePricingRulesResponse_arn :: Lens.Lens' AssociatePricingRulesResponse (Prelude.Maybe Prelude.Text)
associatePricingRulesResponse_arn = Lens.lens (\AssociatePricingRulesResponse' {arn} -> arn) (\s@AssociatePricingRulesResponse' {} a -> s {arn = a} :: AssociatePricingRulesResponse)

-- | The response's http status code.
associatePricingRulesResponse_httpStatus :: Lens.Lens' AssociatePricingRulesResponse Prelude.Int
associatePricingRulesResponse_httpStatus = Lens.lens (\AssociatePricingRulesResponse' {httpStatus} -> httpStatus) (\s@AssociatePricingRulesResponse' {} a -> s {httpStatus = a} :: AssociatePricingRulesResponse)

instance Prelude.NFData AssociatePricingRulesResponse where
  rnf AssociatePricingRulesResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
