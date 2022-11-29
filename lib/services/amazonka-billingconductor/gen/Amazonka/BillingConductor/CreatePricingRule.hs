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
-- Module      : Amazonka.BillingConductor.CreatePricingRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pricing rule can be associated to a pricing plan, or a set of
-- pricing plans.
module Amazonka.BillingConductor.CreatePricingRule
  ( -- * Creating a Request
    CreatePricingRule (..),
    newCreatePricingRule,

    -- * Request Lenses
    createPricingRule_tags,
    createPricingRule_clientToken,
    createPricingRule_billingEntity,
    createPricingRule_description,
    createPricingRule_service,
    createPricingRule_name,
    createPricingRule_scope,
    createPricingRule_type,
    createPricingRule_modifierPercentage,

    -- * Destructuring the Response
    CreatePricingRuleResponse (..),
    newCreatePricingRuleResponse,

    -- * Response Lenses
    createPricingRuleResponse_arn,
    createPricingRuleResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePricingRule' smart constructor.
data CreatePricingRule = CreatePricingRule'
  { -- | A map that contains tag keys and tag values that are attached to a
    -- pricing rule.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The token that\'s needed to support idempotency. Idempotency isn\'t
    -- currently supported, but will be implemented in a future update.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The seller of services provided by Amazon Web Services, their
    -- affiliates, or third-party providers selling services via Amazon Web
    -- Services Marketplace.
    billingEntity :: Prelude.Maybe Prelude.Text,
    -- | The pricing rule description.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | If the @Scope@ attribute is set to @SERVICE@, the attribute indicates
    -- which service the @PricingRule@ is applicable for.
    service :: Prelude.Maybe Prelude.Text,
    -- | The pricing rule name. The names must be unique to each pricing rule.
    name :: Core.Sensitive Prelude.Text,
    -- | The scope of pricing rule that indicates if it\'s globally applicable,
    -- or it\'s service-specific.
    scope :: PricingRuleScope,
    -- | The type of pricing rule.
    type' :: PricingRuleType,
    -- | A percentage modifier that\'s applied on the public pricing rates.
    modifierPercentage :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePricingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createPricingRule_tags' - A map that contains tag keys and tag values that are attached to a
-- pricing rule.
--
-- 'clientToken', 'createPricingRule_clientToken' - The token that\'s needed to support idempotency. Idempotency isn\'t
-- currently supported, but will be implemented in a future update.
--
-- 'billingEntity', 'createPricingRule_billingEntity' - The seller of services provided by Amazon Web Services, their
-- affiliates, or third-party providers selling services via Amazon Web
-- Services Marketplace.
--
-- 'description', 'createPricingRule_description' - The pricing rule description.
--
-- 'service', 'createPricingRule_service' - If the @Scope@ attribute is set to @SERVICE@, the attribute indicates
-- which service the @PricingRule@ is applicable for.
--
-- 'name', 'createPricingRule_name' - The pricing rule name. The names must be unique to each pricing rule.
--
-- 'scope', 'createPricingRule_scope' - The scope of pricing rule that indicates if it\'s globally applicable,
-- or it\'s service-specific.
--
-- 'type'', 'createPricingRule_type' - The type of pricing rule.
--
-- 'modifierPercentage', 'createPricingRule_modifierPercentage' - A percentage modifier that\'s applied on the public pricing rates.
newCreatePricingRule ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  PricingRuleScope ->
  -- | 'type''
  PricingRuleType ->
  -- | 'modifierPercentage'
  Prelude.Double ->
  CreatePricingRule
newCreatePricingRule
  pName_
  pScope_
  pType_
  pModifierPercentage_ =
    CreatePricingRule'
      { tags = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        billingEntity = Prelude.Nothing,
        description = Prelude.Nothing,
        service = Prelude.Nothing,
        name = Core._Sensitive Lens.# pName_,
        scope = pScope_,
        type' = pType_,
        modifierPercentage = pModifierPercentage_
      }

-- | A map that contains tag keys and tag values that are attached to a
-- pricing rule.
createPricingRule_tags :: Lens.Lens' CreatePricingRule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPricingRule_tags = Lens.lens (\CreatePricingRule' {tags} -> tags) (\s@CreatePricingRule' {} a -> s {tags = a} :: CreatePricingRule) Prelude.. Lens.mapping Lens.coerced

-- | The token that\'s needed to support idempotency. Idempotency isn\'t
-- currently supported, but will be implemented in a future update.
createPricingRule_clientToken :: Lens.Lens' CreatePricingRule (Prelude.Maybe Prelude.Text)
createPricingRule_clientToken = Lens.lens (\CreatePricingRule' {clientToken} -> clientToken) (\s@CreatePricingRule' {} a -> s {clientToken = a} :: CreatePricingRule)

-- | The seller of services provided by Amazon Web Services, their
-- affiliates, or third-party providers selling services via Amazon Web
-- Services Marketplace.
createPricingRule_billingEntity :: Lens.Lens' CreatePricingRule (Prelude.Maybe Prelude.Text)
createPricingRule_billingEntity = Lens.lens (\CreatePricingRule' {billingEntity} -> billingEntity) (\s@CreatePricingRule' {} a -> s {billingEntity = a} :: CreatePricingRule)

-- | The pricing rule description.
createPricingRule_description :: Lens.Lens' CreatePricingRule (Prelude.Maybe Prelude.Text)
createPricingRule_description = Lens.lens (\CreatePricingRule' {description} -> description) (\s@CreatePricingRule' {} a -> s {description = a} :: CreatePricingRule) Prelude.. Lens.mapping Core._Sensitive

-- | If the @Scope@ attribute is set to @SERVICE@, the attribute indicates
-- which service the @PricingRule@ is applicable for.
createPricingRule_service :: Lens.Lens' CreatePricingRule (Prelude.Maybe Prelude.Text)
createPricingRule_service = Lens.lens (\CreatePricingRule' {service} -> service) (\s@CreatePricingRule' {} a -> s {service = a} :: CreatePricingRule)

-- | The pricing rule name. The names must be unique to each pricing rule.
createPricingRule_name :: Lens.Lens' CreatePricingRule Prelude.Text
createPricingRule_name = Lens.lens (\CreatePricingRule' {name} -> name) (\s@CreatePricingRule' {} a -> s {name = a} :: CreatePricingRule) Prelude.. Core._Sensitive

-- | The scope of pricing rule that indicates if it\'s globally applicable,
-- or it\'s service-specific.
createPricingRule_scope :: Lens.Lens' CreatePricingRule PricingRuleScope
createPricingRule_scope = Lens.lens (\CreatePricingRule' {scope} -> scope) (\s@CreatePricingRule' {} a -> s {scope = a} :: CreatePricingRule)

-- | The type of pricing rule.
createPricingRule_type :: Lens.Lens' CreatePricingRule PricingRuleType
createPricingRule_type = Lens.lens (\CreatePricingRule' {type'} -> type') (\s@CreatePricingRule' {} a -> s {type' = a} :: CreatePricingRule)

-- | A percentage modifier that\'s applied on the public pricing rates.
createPricingRule_modifierPercentage :: Lens.Lens' CreatePricingRule Prelude.Double
createPricingRule_modifierPercentage = Lens.lens (\CreatePricingRule' {modifierPercentage} -> modifierPercentage) (\s@CreatePricingRule' {} a -> s {modifierPercentage = a} :: CreatePricingRule)

instance Core.AWSRequest CreatePricingRule where
  type
    AWSResponse CreatePricingRule =
      CreatePricingRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePricingRuleResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePricingRule where
  hashWithSalt _salt CreatePricingRule' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` billingEntity
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` service
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` modifierPercentage

instance Prelude.NFData CreatePricingRule where
  rnf CreatePricingRule' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf billingEntity
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf service
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf modifierPercentage

instance Core.ToHeaders CreatePricingRule where
  toHeaders CreatePricingRule' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreatePricingRule where
  toJSON CreatePricingRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("BillingEntity" Core..=) Prelude.<$> billingEntity,
            ("Description" Core..=) Prelude.<$> description,
            ("Service" Core..=) Prelude.<$> service,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Scope" Core..= scope),
            Prelude.Just ("Type" Core..= type'),
            Prelude.Just
              ("ModifierPercentage" Core..= modifierPercentage)
          ]
      )

instance Core.ToPath CreatePricingRule where
  toPath = Prelude.const "/create-pricing-rule"

instance Core.ToQuery CreatePricingRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePricingRuleResponse' smart constructor.
data CreatePricingRuleResponse = CreatePricingRuleResponse'
  { -- | The Amazon Resource Name (ARN) of the created pricing rule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePricingRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createPricingRuleResponse_arn' - The Amazon Resource Name (ARN) of the created pricing rule.
--
-- 'httpStatus', 'createPricingRuleResponse_httpStatus' - The response's http status code.
newCreatePricingRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePricingRuleResponse
newCreatePricingRuleResponse pHttpStatus_ =
  CreatePricingRuleResponse'
    { arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the created pricing rule.
createPricingRuleResponse_arn :: Lens.Lens' CreatePricingRuleResponse (Prelude.Maybe Prelude.Text)
createPricingRuleResponse_arn = Lens.lens (\CreatePricingRuleResponse' {arn} -> arn) (\s@CreatePricingRuleResponse' {} a -> s {arn = a} :: CreatePricingRuleResponse)

-- | The response's http status code.
createPricingRuleResponse_httpStatus :: Lens.Lens' CreatePricingRuleResponse Prelude.Int
createPricingRuleResponse_httpStatus = Lens.lens (\CreatePricingRuleResponse' {httpStatus} -> httpStatus) (\s@CreatePricingRuleResponse' {} a -> s {httpStatus = a} :: CreatePricingRuleResponse)

instance Prelude.NFData CreatePricingRuleResponse where
  rnf CreatePricingRuleResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
