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
-- Module      : Amazonka.BillingConductor.CreatePricingPlan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pricing plan that is used for computing Amazon Web Services
-- charges for billing groups.
module Amazonka.BillingConductor.CreatePricingPlan
  ( -- * Creating a Request
    CreatePricingPlan (..),
    newCreatePricingPlan,

    -- * Request Lenses
    createPricingPlan_tags,
    createPricingPlan_clientToken,
    createPricingPlan_description,
    createPricingPlan_pricingRuleArns,
    createPricingPlan_name,

    -- * Destructuring the Response
    CreatePricingPlanResponse (..),
    newCreatePricingPlanResponse,

    -- * Response Lenses
    createPricingPlanResponse_arn,
    createPricingPlanResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePricingPlan' smart constructor.
data CreatePricingPlan = CreatePricingPlan'
  { -- | A map that contains tag keys and tag values that are attached to a
    -- pricing plan.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The token that is needed to support idempotency. Idempotency isn\'t
    -- currently supported, but will be implemented in a future update.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the pricing plan.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A list of Amazon Resource Names (ARNs) that define the pricing plan
    -- parameters.
    pricingRuleArns :: Prelude.Maybe [Prelude.Text],
    -- | The name of the pricing plan. The names must be unique to each pricing
    -- plan.
    name :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePricingPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createPricingPlan_tags' - A map that contains tag keys and tag values that are attached to a
-- pricing plan.
--
-- 'clientToken', 'createPricingPlan_clientToken' - The token that is needed to support idempotency. Idempotency isn\'t
-- currently supported, but will be implemented in a future update.
--
-- 'description', 'createPricingPlan_description' - The description of the pricing plan.
--
-- 'pricingRuleArns', 'createPricingPlan_pricingRuleArns' - A list of Amazon Resource Names (ARNs) that define the pricing plan
-- parameters.
--
-- 'name', 'createPricingPlan_name' - The name of the pricing plan. The names must be unique to each pricing
-- plan.
newCreatePricingPlan ::
  -- | 'name'
  Prelude.Text ->
  CreatePricingPlan
newCreatePricingPlan pName_ =
  CreatePricingPlan'
    { tags = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      pricingRuleArns = Prelude.Nothing,
      name = Core._Sensitive Lens.# pName_
    }

-- | A map that contains tag keys and tag values that are attached to a
-- pricing plan.
createPricingPlan_tags :: Lens.Lens' CreatePricingPlan (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPricingPlan_tags = Lens.lens (\CreatePricingPlan' {tags} -> tags) (\s@CreatePricingPlan' {} a -> s {tags = a} :: CreatePricingPlan) Prelude.. Lens.mapping Lens.coerced

-- | The token that is needed to support idempotency. Idempotency isn\'t
-- currently supported, but will be implemented in a future update.
createPricingPlan_clientToken :: Lens.Lens' CreatePricingPlan (Prelude.Maybe Prelude.Text)
createPricingPlan_clientToken = Lens.lens (\CreatePricingPlan' {clientToken} -> clientToken) (\s@CreatePricingPlan' {} a -> s {clientToken = a} :: CreatePricingPlan)

-- | The description of the pricing plan.
createPricingPlan_description :: Lens.Lens' CreatePricingPlan (Prelude.Maybe Prelude.Text)
createPricingPlan_description = Lens.lens (\CreatePricingPlan' {description} -> description) (\s@CreatePricingPlan' {} a -> s {description = a} :: CreatePricingPlan) Prelude.. Lens.mapping Core._Sensitive

-- | A list of Amazon Resource Names (ARNs) that define the pricing plan
-- parameters.
createPricingPlan_pricingRuleArns :: Lens.Lens' CreatePricingPlan (Prelude.Maybe [Prelude.Text])
createPricingPlan_pricingRuleArns = Lens.lens (\CreatePricingPlan' {pricingRuleArns} -> pricingRuleArns) (\s@CreatePricingPlan' {} a -> s {pricingRuleArns = a} :: CreatePricingPlan) Prelude.. Lens.mapping Lens.coerced

-- | The name of the pricing plan. The names must be unique to each pricing
-- plan.
createPricingPlan_name :: Lens.Lens' CreatePricingPlan Prelude.Text
createPricingPlan_name = Lens.lens (\CreatePricingPlan' {name} -> name) (\s@CreatePricingPlan' {} a -> s {name = a} :: CreatePricingPlan) Prelude.. Core._Sensitive

instance Core.AWSRequest CreatePricingPlan where
  type
    AWSResponse CreatePricingPlan =
      CreatePricingPlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePricingPlanResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePricingPlan where
  hashWithSalt _salt CreatePricingPlan' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` pricingRuleArns
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreatePricingPlan where
  rnf CreatePricingPlan' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf pricingRuleArns
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreatePricingPlan where
  toHeaders CreatePricingPlan' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreatePricingPlan where
  toJSON CreatePricingPlan' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Description" Core..=) Prelude.<$> description,
            ("PricingRuleArns" Core..=)
              Prelude.<$> pricingRuleArns,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreatePricingPlan where
  toPath = Prelude.const "/create-pricing-plan"

instance Core.ToQuery CreatePricingPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePricingPlanResponse' smart constructor.
data CreatePricingPlanResponse = CreatePricingPlanResponse'
  { -- | The Amazon Resource Name (ARN) of the created pricing plan.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePricingPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createPricingPlanResponse_arn' - The Amazon Resource Name (ARN) of the created pricing plan.
--
-- 'httpStatus', 'createPricingPlanResponse_httpStatus' - The response's http status code.
newCreatePricingPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePricingPlanResponse
newCreatePricingPlanResponse pHttpStatus_ =
  CreatePricingPlanResponse'
    { arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the created pricing plan.
createPricingPlanResponse_arn :: Lens.Lens' CreatePricingPlanResponse (Prelude.Maybe Prelude.Text)
createPricingPlanResponse_arn = Lens.lens (\CreatePricingPlanResponse' {arn} -> arn) (\s@CreatePricingPlanResponse' {} a -> s {arn = a} :: CreatePricingPlanResponse)

-- | The response's http status code.
createPricingPlanResponse_httpStatus :: Lens.Lens' CreatePricingPlanResponse Prelude.Int
createPricingPlanResponse_httpStatus = Lens.lens (\CreatePricingPlanResponse' {httpStatus} -> httpStatus) (\s@CreatePricingPlanResponse' {} a -> s {httpStatus = a} :: CreatePricingPlanResponse)

instance Prelude.NFData CreatePricingPlanResponse where
  rnf CreatePricingPlanResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
