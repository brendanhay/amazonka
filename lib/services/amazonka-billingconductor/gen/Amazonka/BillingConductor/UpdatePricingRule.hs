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
-- Module      : Amazonka.BillingConductor.UpdatePricingRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing pricing rule.
module Amazonka.BillingConductor.UpdatePricingRule
  ( -- * Creating a Request
    UpdatePricingRule (..),
    newUpdatePricingRule,

    -- * Request Lenses
    updatePricingRule_description,
    updatePricingRule_modifierPercentage,
    updatePricingRule_name,
    updatePricingRule_tiering,
    updatePricingRule_type,
    updatePricingRule_arn,

    -- * Destructuring the Response
    UpdatePricingRuleResponse (..),
    newUpdatePricingRuleResponse,

    -- * Response Lenses
    updatePricingRuleResponse_arn,
    updatePricingRuleResponse_associatedPricingPlanCount,
    updatePricingRuleResponse_billingEntity,
    updatePricingRuleResponse_description,
    updatePricingRuleResponse_lastModifiedTime,
    updatePricingRuleResponse_modifierPercentage,
    updatePricingRuleResponse_name,
    updatePricingRuleResponse_scope,
    updatePricingRuleResponse_service,
    updatePricingRuleResponse_tiering,
    updatePricingRuleResponse_type,
    updatePricingRuleResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePricingRule' smart constructor.
data UpdatePricingRule = UpdatePricingRule'
  { -- | The new description for the pricing rule.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The new modifier to show pricing plan rates as a percentage.
    modifierPercentage :: Prelude.Maybe Prelude.Double,
    -- | The new name of the pricing rule. The name must be unique to each
    -- pricing rule.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The set of tiering configurations for the pricing rule.
    tiering :: Prelude.Maybe UpdateTieringInput,
    -- | The new pricing rule type.
    type' :: Prelude.Maybe PricingRuleType,
    -- | The Amazon Resource Name (ARN) of the pricing rule to update.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePricingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updatePricingRule_description' - The new description for the pricing rule.
--
-- 'modifierPercentage', 'updatePricingRule_modifierPercentage' - The new modifier to show pricing plan rates as a percentage.
--
-- 'name', 'updatePricingRule_name' - The new name of the pricing rule. The name must be unique to each
-- pricing rule.
--
-- 'tiering', 'updatePricingRule_tiering' - The set of tiering configurations for the pricing rule.
--
-- 'type'', 'updatePricingRule_type' - The new pricing rule type.
--
-- 'arn', 'updatePricingRule_arn' - The Amazon Resource Name (ARN) of the pricing rule to update.
newUpdatePricingRule ::
  -- | 'arn'
  Prelude.Text ->
  UpdatePricingRule
newUpdatePricingRule pArn_ =
  UpdatePricingRule'
    { description = Prelude.Nothing,
      modifierPercentage = Prelude.Nothing,
      name = Prelude.Nothing,
      tiering = Prelude.Nothing,
      type' = Prelude.Nothing,
      arn = pArn_
    }

-- | The new description for the pricing rule.
updatePricingRule_description :: Lens.Lens' UpdatePricingRule (Prelude.Maybe Prelude.Text)
updatePricingRule_description = Lens.lens (\UpdatePricingRule' {description} -> description) (\s@UpdatePricingRule' {} a -> s {description = a} :: UpdatePricingRule) Prelude.. Lens.mapping Data._Sensitive

-- | The new modifier to show pricing plan rates as a percentage.
updatePricingRule_modifierPercentage :: Lens.Lens' UpdatePricingRule (Prelude.Maybe Prelude.Double)
updatePricingRule_modifierPercentage = Lens.lens (\UpdatePricingRule' {modifierPercentage} -> modifierPercentage) (\s@UpdatePricingRule' {} a -> s {modifierPercentage = a} :: UpdatePricingRule)

-- | The new name of the pricing rule. The name must be unique to each
-- pricing rule.
updatePricingRule_name :: Lens.Lens' UpdatePricingRule (Prelude.Maybe Prelude.Text)
updatePricingRule_name = Lens.lens (\UpdatePricingRule' {name} -> name) (\s@UpdatePricingRule' {} a -> s {name = a} :: UpdatePricingRule) Prelude.. Lens.mapping Data._Sensitive

-- | The set of tiering configurations for the pricing rule.
updatePricingRule_tiering :: Lens.Lens' UpdatePricingRule (Prelude.Maybe UpdateTieringInput)
updatePricingRule_tiering = Lens.lens (\UpdatePricingRule' {tiering} -> tiering) (\s@UpdatePricingRule' {} a -> s {tiering = a} :: UpdatePricingRule)

-- | The new pricing rule type.
updatePricingRule_type :: Lens.Lens' UpdatePricingRule (Prelude.Maybe PricingRuleType)
updatePricingRule_type = Lens.lens (\UpdatePricingRule' {type'} -> type') (\s@UpdatePricingRule' {} a -> s {type' = a} :: UpdatePricingRule)

-- | The Amazon Resource Name (ARN) of the pricing rule to update.
updatePricingRule_arn :: Lens.Lens' UpdatePricingRule Prelude.Text
updatePricingRule_arn = Lens.lens (\UpdatePricingRule' {arn} -> arn) (\s@UpdatePricingRule' {} a -> s {arn = a} :: UpdatePricingRule)

instance Core.AWSRequest UpdatePricingRule where
  type
    AWSResponse UpdatePricingRule =
      UpdatePricingRuleResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePricingRuleResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "AssociatedPricingPlanCount")
            Prelude.<*> (x Data..?> "BillingEntity")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "ModifierPercentage")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Scope")
            Prelude.<*> (x Data..?> "Service")
            Prelude.<*> (x Data..?> "Tiering")
            Prelude.<*> (x Data..?> "Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePricingRule where
  hashWithSalt _salt UpdatePricingRule' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` modifierPercentage
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tiering
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` arn

instance Prelude.NFData UpdatePricingRule where
  rnf UpdatePricingRule' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf modifierPercentage
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tiering
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders UpdatePricingRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePricingRule where
  toJSON UpdatePricingRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("ModifierPercentage" Data..=)
              Prelude.<$> modifierPercentage,
            ("Name" Data..=) Prelude.<$> name,
            ("Tiering" Data..=) Prelude.<$> tiering,
            ("Type" Data..=) Prelude.<$> type',
            Prelude.Just ("Arn" Data..= arn)
          ]
      )

instance Data.ToPath UpdatePricingRule where
  toPath = Prelude.const "/update-pricing-rule"

instance Data.ToQuery UpdatePricingRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePricingRuleResponse' smart constructor.
data UpdatePricingRuleResponse = UpdatePricingRuleResponse'
  { -- | The Amazon Resource Name (ARN) of the successfully updated pricing rule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The pricing plans count that this pricing rule is associated with.
    associatedPricingPlanCount :: Prelude.Maybe Prelude.Natural,
    -- | The seller of services provided by Amazon Web Services, their
    -- affiliates, or third-party providers selling services via Amazon Web
    -- Services Marketplace.
    billingEntity :: Prelude.Maybe Prelude.Text,
    -- | The new description for the pricing rule.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The most recent time the pricing rule was modified.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | The new modifier to show pricing plan rates as a percentage.
    modifierPercentage :: Prelude.Maybe Prelude.Double,
    -- | The new name of the pricing rule. The name must be unique to each
    -- pricing rule.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The scope of pricing rule that indicates if it\'s globally applicable,
    -- or it\'s service-specific.
    scope :: Prelude.Maybe PricingRuleScope,
    -- | If the @Scope@ attribute is set to @SERVICE@, the attribute indicates
    -- which service the @PricingRule@ is applicable for.
    service :: Prelude.Maybe Prelude.Text,
    -- | The set of tiering configurations for the pricing rule.
    tiering :: Prelude.Maybe UpdateTieringInput,
    -- | The new pricing rule type.
    type' :: Prelude.Maybe PricingRuleType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePricingRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updatePricingRuleResponse_arn' - The Amazon Resource Name (ARN) of the successfully updated pricing rule.
--
-- 'associatedPricingPlanCount', 'updatePricingRuleResponse_associatedPricingPlanCount' - The pricing plans count that this pricing rule is associated with.
--
-- 'billingEntity', 'updatePricingRuleResponse_billingEntity' - The seller of services provided by Amazon Web Services, their
-- affiliates, or third-party providers selling services via Amazon Web
-- Services Marketplace.
--
-- 'description', 'updatePricingRuleResponse_description' - The new description for the pricing rule.
--
-- 'lastModifiedTime', 'updatePricingRuleResponse_lastModifiedTime' - The most recent time the pricing rule was modified.
--
-- 'modifierPercentage', 'updatePricingRuleResponse_modifierPercentage' - The new modifier to show pricing plan rates as a percentage.
--
-- 'name', 'updatePricingRuleResponse_name' - The new name of the pricing rule. The name must be unique to each
-- pricing rule.
--
-- 'scope', 'updatePricingRuleResponse_scope' - The scope of pricing rule that indicates if it\'s globally applicable,
-- or it\'s service-specific.
--
-- 'service', 'updatePricingRuleResponse_service' - If the @Scope@ attribute is set to @SERVICE@, the attribute indicates
-- which service the @PricingRule@ is applicable for.
--
-- 'tiering', 'updatePricingRuleResponse_tiering' - The set of tiering configurations for the pricing rule.
--
-- 'type'', 'updatePricingRuleResponse_type' - The new pricing rule type.
--
-- 'httpStatus', 'updatePricingRuleResponse_httpStatus' - The response's http status code.
newUpdatePricingRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePricingRuleResponse
newUpdatePricingRuleResponse pHttpStatus_ =
  UpdatePricingRuleResponse'
    { arn = Prelude.Nothing,
      associatedPricingPlanCount = Prelude.Nothing,
      billingEntity = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      modifierPercentage = Prelude.Nothing,
      name = Prelude.Nothing,
      scope = Prelude.Nothing,
      service = Prelude.Nothing,
      tiering = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the successfully updated pricing rule.
updatePricingRuleResponse_arn :: Lens.Lens' UpdatePricingRuleResponse (Prelude.Maybe Prelude.Text)
updatePricingRuleResponse_arn = Lens.lens (\UpdatePricingRuleResponse' {arn} -> arn) (\s@UpdatePricingRuleResponse' {} a -> s {arn = a} :: UpdatePricingRuleResponse)

-- | The pricing plans count that this pricing rule is associated with.
updatePricingRuleResponse_associatedPricingPlanCount :: Lens.Lens' UpdatePricingRuleResponse (Prelude.Maybe Prelude.Natural)
updatePricingRuleResponse_associatedPricingPlanCount = Lens.lens (\UpdatePricingRuleResponse' {associatedPricingPlanCount} -> associatedPricingPlanCount) (\s@UpdatePricingRuleResponse' {} a -> s {associatedPricingPlanCount = a} :: UpdatePricingRuleResponse)

-- | The seller of services provided by Amazon Web Services, their
-- affiliates, or third-party providers selling services via Amazon Web
-- Services Marketplace.
updatePricingRuleResponse_billingEntity :: Lens.Lens' UpdatePricingRuleResponse (Prelude.Maybe Prelude.Text)
updatePricingRuleResponse_billingEntity = Lens.lens (\UpdatePricingRuleResponse' {billingEntity} -> billingEntity) (\s@UpdatePricingRuleResponse' {} a -> s {billingEntity = a} :: UpdatePricingRuleResponse)

-- | The new description for the pricing rule.
updatePricingRuleResponse_description :: Lens.Lens' UpdatePricingRuleResponse (Prelude.Maybe Prelude.Text)
updatePricingRuleResponse_description = Lens.lens (\UpdatePricingRuleResponse' {description} -> description) (\s@UpdatePricingRuleResponse' {} a -> s {description = a} :: UpdatePricingRuleResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The most recent time the pricing rule was modified.
updatePricingRuleResponse_lastModifiedTime :: Lens.Lens' UpdatePricingRuleResponse (Prelude.Maybe Prelude.Integer)
updatePricingRuleResponse_lastModifiedTime = Lens.lens (\UpdatePricingRuleResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdatePricingRuleResponse' {} a -> s {lastModifiedTime = a} :: UpdatePricingRuleResponse)

-- | The new modifier to show pricing plan rates as a percentage.
updatePricingRuleResponse_modifierPercentage :: Lens.Lens' UpdatePricingRuleResponse (Prelude.Maybe Prelude.Double)
updatePricingRuleResponse_modifierPercentage = Lens.lens (\UpdatePricingRuleResponse' {modifierPercentage} -> modifierPercentage) (\s@UpdatePricingRuleResponse' {} a -> s {modifierPercentage = a} :: UpdatePricingRuleResponse)

-- | The new name of the pricing rule. The name must be unique to each
-- pricing rule.
updatePricingRuleResponse_name :: Lens.Lens' UpdatePricingRuleResponse (Prelude.Maybe Prelude.Text)
updatePricingRuleResponse_name = Lens.lens (\UpdatePricingRuleResponse' {name} -> name) (\s@UpdatePricingRuleResponse' {} a -> s {name = a} :: UpdatePricingRuleResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The scope of pricing rule that indicates if it\'s globally applicable,
-- or it\'s service-specific.
updatePricingRuleResponse_scope :: Lens.Lens' UpdatePricingRuleResponse (Prelude.Maybe PricingRuleScope)
updatePricingRuleResponse_scope = Lens.lens (\UpdatePricingRuleResponse' {scope} -> scope) (\s@UpdatePricingRuleResponse' {} a -> s {scope = a} :: UpdatePricingRuleResponse)

-- | If the @Scope@ attribute is set to @SERVICE@, the attribute indicates
-- which service the @PricingRule@ is applicable for.
updatePricingRuleResponse_service :: Lens.Lens' UpdatePricingRuleResponse (Prelude.Maybe Prelude.Text)
updatePricingRuleResponse_service = Lens.lens (\UpdatePricingRuleResponse' {service} -> service) (\s@UpdatePricingRuleResponse' {} a -> s {service = a} :: UpdatePricingRuleResponse)

-- | The set of tiering configurations for the pricing rule.
updatePricingRuleResponse_tiering :: Lens.Lens' UpdatePricingRuleResponse (Prelude.Maybe UpdateTieringInput)
updatePricingRuleResponse_tiering = Lens.lens (\UpdatePricingRuleResponse' {tiering} -> tiering) (\s@UpdatePricingRuleResponse' {} a -> s {tiering = a} :: UpdatePricingRuleResponse)

-- | The new pricing rule type.
updatePricingRuleResponse_type :: Lens.Lens' UpdatePricingRuleResponse (Prelude.Maybe PricingRuleType)
updatePricingRuleResponse_type = Lens.lens (\UpdatePricingRuleResponse' {type'} -> type') (\s@UpdatePricingRuleResponse' {} a -> s {type' = a} :: UpdatePricingRuleResponse)

-- | The response's http status code.
updatePricingRuleResponse_httpStatus :: Lens.Lens' UpdatePricingRuleResponse Prelude.Int
updatePricingRuleResponse_httpStatus = Lens.lens (\UpdatePricingRuleResponse' {httpStatus} -> httpStatus) (\s@UpdatePricingRuleResponse' {} a -> s {httpStatus = a} :: UpdatePricingRuleResponse)

instance Prelude.NFData UpdatePricingRuleResponse where
  rnf UpdatePricingRuleResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf associatedPricingPlanCount
      `Prelude.seq` Prelude.rnf billingEntity
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf modifierPercentage
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf service
      `Prelude.seq` Prelude.rnf tiering
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf httpStatus
