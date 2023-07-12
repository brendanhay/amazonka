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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    createPricingRule_billingEntity,
    createPricingRule_clientToken,
    createPricingRule_description,
    createPricingRule_modifierPercentage,
    createPricingRule_service,
    createPricingRule_tags,
    createPricingRule_tiering,
    createPricingRule_name,
    createPricingRule_scope,
    createPricingRule_type,

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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePricingRule' smart constructor.
data CreatePricingRule = CreatePricingRule'
  { -- | The seller of services provided by Amazon Web Services, their
    -- affiliates, or third-party providers selling services via Amazon Web
    -- Services Marketplace.
    billingEntity :: Prelude.Maybe Prelude.Text,
    -- | The token that\'s needed to support idempotency. Idempotency isn\'t
    -- currently supported, but will be implemented in a future update.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The pricing rule description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A percentage modifier that\'s applied on the public pricing rates.
    modifierPercentage :: Prelude.Maybe Prelude.Double,
    -- | If the @Scope@ attribute is set to @SERVICE@, the attribute indicates
    -- which service the @PricingRule@ is applicable for.
    service :: Prelude.Maybe Prelude.Text,
    -- | A map that contains tag keys and tag values that are attached to a
    -- pricing rule.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The set of tiering configurations for the pricing rule.
    tiering :: Prelude.Maybe CreateTieringInput,
    -- | The pricing rule name. The names must be unique to each pricing rule.
    name :: Data.Sensitive Prelude.Text,
    -- | The scope of pricing rule that indicates if it\'s globally applicable,
    -- or it\'s service-specific.
    scope :: PricingRuleScope,
    -- | The type of pricing rule.
    type' :: PricingRuleType
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
-- 'billingEntity', 'createPricingRule_billingEntity' - The seller of services provided by Amazon Web Services, their
-- affiliates, or third-party providers selling services via Amazon Web
-- Services Marketplace.
--
-- 'clientToken', 'createPricingRule_clientToken' - The token that\'s needed to support idempotency. Idempotency isn\'t
-- currently supported, but will be implemented in a future update.
--
-- 'description', 'createPricingRule_description' - The pricing rule description.
--
-- 'modifierPercentage', 'createPricingRule_modifierPercentage' - A percentage modifier that\'s applied on the public pricing rates.
--
-- 'service', 'createPricingRule_service' - If the @Scope@ attribute is set to @SERVICE@, the attribute indicates
-- which service the @PricingRule@ is applicable for.
--
-- 'tags', 'createPricingRule_tags' - A map that contains tag keys and tag values that are attached to a
-- pricing rule.
--
-- 'tiering', 'createPricingRule_tiering' - The set of tiering configurations for the pricing rule.
--
-- 'name', 'createPricingRule_name' - The pricing rule name. The names must be unique to each pricing rule.
--
-- 'scope', 'createPricingRule_scope' - The scope of pricing rule that indicates if it\'s globally applicable,
-- or it\'s service-specific.
--
-- 'type'', 'createPricingRule_type' - The type of pricing rule.
newCreatePricingRule ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  PricingRuleScope ->
  -- | 'type''
  PricingRuleType ->
  CreatePricingRule
newCreatePricingRule pName_ pScope_ pType_ =
  CreatePricingRule'
    { billingEntity = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      modifierPercentage = Prelude.Nothing,
      service = Prelude.Nothing,
      tags = Prelude.Nothing,
      tiering = Prelude.Nothing,
      name = Data._Sensitive Lens.# pName_,
      scope = pScope_,
      type' = pType_
    }

-- | The seller of services provided by Amazon Web Services, their
-- affiliates, or third-party providers selling services via Amazon Web
-- Services Marketplace.
createPricingRule_billingEntity :: Lens.Lens' CreatePricingRule (Prelude.Maybe Prelude.Text)
createPricingRule_billingEntity = Lens.lens (\CreatePricingRule' {billingEntity} -> billingEntity) (\s@CreatePricingRule' {} a -> s {billingEntity = a} :: CreatePricingRule)

-- | The token that\'s needed to support idempotency. Idempotency isn\'t
-- currently supported, but will be implemented in a future update.
createPricingRule_clientToken :: Lens.Lens' CreatePricingRule (Prelude.Maybe Prelude.Text)
createPricingRule_clientToken = Lens.lens (\CreatePricingRule' {clientToken} -> clientToken) (\s@CreatePricingRule' {} a -> s {clientToken = a} :: CreatePricingRule)

-- | The pricing rule description.
createPricingRule_description :: Lens.Lens' CreatePricingRule (Prelude.Maybe Prelude.Text)
createPricingRule_description = Lens.lens (\CreatePricingRule' {description} -> description) (\s@CreatePricingRule' {} a -> s {description = a} :: CreatePricingRule) Prelude.. Lens.mapping Data._Sensitive

-- | A percentage modifier that\'s applied on the public pricing rates.
createPricingRule_modifierPercentage :: Lens.Lens' CreatePricingRule (Prelude.Maybe Prelude.Double)
createPricingRule_modifierPercentage = Lens.lens (\CreatePricingRule' {modifierPercentage} -> modifierPercentage) (\s@CreatePricingRule' {} a -> s {modifierPercentage = a} :: CreatePricingRule)

-- | If the @Scope@ attribute is set to @SERVICE@, the attribute indicates
-- which service the @PricingRule@ is applicable for.
createPricingRule_service :: Lens.Lens' CreatePricingRule (Prelude.Maybe Prelude.Text)
createPricingRule_service = Lens.lens (\CreatePricingRule' {service} -> service) (\s@CreatePricingRule' {} a -> s {service = a} :: CreatePricingRule)

-- | A map that contains tag keys and tag values that are attached to a
-- pricing rule.
createPricingRule_tags :: Lens.Lens' CreatePricingRule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPricingRule_tags = Lens.lens (\CreatePricingRule' {tags} -> tags) (\s@CreatePricingRule' {} a -> s {tags = a} :: CreatePricingRule) Prelude.. Lens.mapping Lens.coerced

-- | The set of tiering configurations for the pricing rule.
createPricingRule_tiering :: Lens.Lens' CreatePricingRule (Prelude.Maybe CreateTieringInput)
createPricingRule_tiering = Lens.lens (\CreatePricingRule' {tiering} -> tiering) (\s@CreatePricingRule' {} a -> s {tiering = a} :: CreatePricingRule)

-- | The pricing rule name. The names must be unique to each pricing rule.
createPricingRule_name :: Lens.Lens' CreatePricingRule Prelude.Text
createPricingRule_name = Lens.lens (\CreatePricingRule' {name} -> name) (\s@CreatePricingRule' {} a -> s {name = a} :: CreatePricingRule) Prelude.. Data._Sensitive

-- | The scope of pricing rule that indicates if it\'s globally applicable,
-- or it\'s service-specific.
createPricingRule_scope :: Lens.Lens' CreatePricingRule PricingRuleScope
createPricingRule_scope = Lens.lens (\CreatePricingRule' {scope} -> scope) (\s@CreatePricingRule' {} a -> s {scope = a} :: CreatePricingRule)

-- | The type of pricing rule.
createPricingRule_type :: Lens.Lens' CreatePricingRule PricingRuleType
createPricingRule_type = Lens.lens (\CreatePricingRule' {type'} -> type') (\s@CreatePricingRule' {} a -> s {type' = a} :: CreatePricingRule)

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
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePricingRule where
  hashWithSalt _salt CreatePricingRule' {..} =
    _salt
      `Prelude.hashWithSalt` billingEntity
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` modifierPercentage
      `Prelude.hashWithSalt` service
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tiering
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreatePricingRule where
  rnf CreatePricingRule' {..} =
    Prelude.rnf billingEntity
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf modifierPercentage
      `Prelude.seq` Prelude.rnf service
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tiering
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreatePricingRule where
  toHeaders CreatePricingRule' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreatePricingRule where
  toJSON CreatePricingRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BillingEntity" Data..=) Prelude.<$> billingEntity,
            ("Description" Data..=) Prelude.<$> description,
            ("ModifierPercentage" Data..=)
              Prelude.<$> modifierPercentage,
            ("Service" Data..=) Prelude.<$> service,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Tiering" Data..=) Prelude.<$> tiering,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath CreatePricingRule where
  toPath = Prelude.const "/create-pricing-rule"

instance Data.ToQuery CreatePricingRule where
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
