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
-- Module      : Amazonka.BillingConductor.DisassociatePricingRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a list of pricing rules from a pricing plan.
module Amazonka.BillingConductor.DisassociatePricingRules
  ( -- * Creating a Request
    DisassociatePricingRules (..),
    newDisassociatePricingRules,

    -- * Request Lenses
    disassociatePricingRules_arn,
    disassociatePricingRules_pricingRuleArns,

    -- * Destructuring the Response
    DisassociatePricingRulesResponse (..),
    newDisassociatePricingRulesResponse,

    -- * Response Lenses
    disassociatePricingRulesResponse_arn,
    disassociatePricingRulesResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociatePricingRules' smart constructor.
data DisassociatePricingRules = DisassociatePricingRules'
  { -- | The pricing plan Amazon Resource Name (ARN) to disassociate pricing
    -- rules from.
    arn :: Prelude.Text,
    -- | A list containing the Amazon Resource Name (ARN) of the pricing rules
    -- that will be disassociated.
    pricingRuleArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociatePricingRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'disassociatePricingRules_arn' - The pricing plan Amazon Resource Name (ARN) to disassociate pricing
-- rules from.
--
-- 'pricingRuleArns', 'disassociatePricingRules_pricingRuleArns' - A list containing the Amazon Resource Name (ARN) of the pricing rules
-- that will be disassociated.
newDisassociatePricingRules ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'pricingRuleArns'
  Prelude.NonEmpty Prelude.Text ->
  DisassociatePricingRules
newDisassociatePricingRules pArn_ pPricingRuleArns_ =
  DisassociatePricingRules'
    { arn = pArn_,
      pricingRuleArns =
        Lens.coerced Lens.# pPricingRuleArns_
    }

-- | The pricing plan Amazon Resource Name (ARN) to disassociate pricing
-- rules from.
disassociatePricingRules_arn :: Lens.Lens' DisassociatePricingRules Prelude.Text
disassociatePricingRules_arn = Lens.lens (\DisassociatePricingRules' {arn} -> arn) (\s@DisassociatePricingRules' {} a -> s {arn = a} :: DisassociatePricingRules)

-- | A list containing the Amazon Resource Name (ARN) of the pricing rules
-- that will be disassociated.
disassociatePricingRules_pricingRuleArns :: Lens.Lens' DisassociatePricingRules (Prelude.NonEmpty Prelude.Text)
disassociatePricingRules_pricingRuleArns = Lens.lens (\DisassociatePricingRules' {pricingRuleArns} -> pricingRuleArns) (\s@DisassociatePricingRules' {} a -> s {pricingRuleArns = a} :: DisassociatePricingRules) Prelude.. Lens.coerced

instance Core.AWSRequest DisassociatePricingRules where
  type
    AWSResponse DisassociatePricingRules =
      DisassociatePricingRulesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociatePricingRulesResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociatePricingRules where
  hashWithSalt _salt DisassociatePricingRules' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` pricingRuleArns

instance Prelude.NFData DisassociatePricingRules where
  rnf DisassociatePricingRules' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf pricingRuleArns

instance Data.ToHeaders DisassociatePricingRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociatePricingRules where
  toJSON DisassociatePricingRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Arn" Data..= arn),
            Prelude.Just
              ("PricingRuleArns" Data..= pricingRuleArns)
          ]
      )

instance Data.ToPath DisassociatePricingRules where
  toPath = Prelude.const "/disassociate-pricing-rules"

instance Data.ToQuery DisassociatePricingRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociatePricingRulesResponse' smart constructor.
data DisassociatePricingRulesResponse = DisassociatePricingRulesResponse'
  { -- | The Amazon Resource Name (ARN) of the pricing plan that the pricing
    -- rules successfully disassociated from.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociatePricingRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'disassociatePricingRulesResponse_arn' - The Amazon Resource Name (ARN) of the pricing plan that the pricing
-- rules successfully disassociated from.
--
-- 'httpStatus', 'disassociatePricingRulesResponse_httpStatus' - The response's http status code.
newDisassociatePricingRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociatePricingRulesResponse
newDisassociatePricingRulesResponse pHttpStatus_ =
  DisassociatePricingRulesResponse'
    { arn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the pricing plan that the pricing
-- rules successfully disassociated from.
disassociatePricingRulesResponse_arn :: Lens.Lens' DisassociatePricingRulesResponse (Prelude.Maybe Prelude.Text)
disassociatePricingRulesResponse_arn = Lens.lens (\DisassociatePricingRulesResponse' {arn} -> arn) (\s@DisassociatePricingRulesResponse' {} a -> s {arn = a} :: DisassociatePricingRulesResponse)

-- | The response's http status code.
disassociatePricingRulesResponse_httpStatus :: Lens.Lens' DisassociatePricingRulesResponse Prelude.Int
disassociatePricingRulesResponse_httpStatus = Lens.lens (\DisassociatePricingRulesResponse' {httpStatus} -> httpStatus) (\s@DisassociatePricingRulesResponse' {} a -> s {httpStatus = a} :: DisassociatePricingRulesResponse)

instance
  Prelude.NFData
    DisassociatePricingRulesResponse
  where
  rnf DisassociatePricingRulesResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
