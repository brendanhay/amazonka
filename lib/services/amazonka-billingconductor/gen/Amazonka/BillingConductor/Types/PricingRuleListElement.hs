{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.BillingConductor.Types.PricingRuleListElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.PricingRuleListElement where

import Amazonka.BillingConductor.Types.PricingRuleScope
import Amazonka.BillingConductor.Types.PricingRuleType
import Amazonka.BillingConductor.Types.Tiering
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of a pricing rule.
--
-- /See:/ 'newPricingRuleListElement' smart constructor.
data PricingRuleListElement = PricingRuleListElement'
  { -- | The Amazon Resource Name (ARN) used to uniquely identify a pricing rule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The pricing plans count that this pricing rule is associated with.
    associatedPricingPlanCount :: Prelude.Maybe Prelude.Natural,
    -- | The seller of services provided by Amazon Web Services, their
    -- affiliates, or third-party providers selling services via Amazon Web
    -- Services Marketplace.
    billingEntity :: Prelude.Maybe Prelude.Text,
    -- | The time when the pricing rule was created.
    creationTime :: Prelude.Maybe Prelude.Integer,
    -- | The pricing rule description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The most recent time when the pricing rule was modified.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | A percentage modifier applied on the public pricing rates.
    modifierPercentage :: Prelude.Maybe Prelude.Double,
    -- | The name of a pricing rule.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The scope of pricing rule that indicates if it is globally applicable,
    -- or if it is service-specific.
    scope :: Prelude.Maybe PricingRuleScope,
    -- | If the @Scope@ attribute is @SERVICE@, this attribute indicates which
    -- service the @PricingRule@ is applicable for.
    service :: Prelude.Maybe Prelude.Text,
    -- | The set of tiering configurations for the pricing rule.
    tiering :: Prelude.Maybe Tiering,
    -- | The type of pricing rule.
    type' :: Prelude.Maybe PricingRuleType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PricingRuleListElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'pricingRuleListElement_arn' - The Amazon Resource Name (ARN) used to uniquely identify a pricing rule.
--
-- 'associatedPricingPlanCount', 'pricingRuleListElement_associatedPricingPlanCount' - The pricing plans count that this pricing rule is associated with.
--
-- 'billingEntity', 'pricingRuleListElement_billingEntity' - The seller of services provided by Amazon Web Services, their
-- affiliates, or third-party providers selling services via Amazon Web
-- Services Marketplace.
--
-- 'creationTime', 'pricingRuleListElement_creationTime' - The time when the pricing rule was created.
--
-- 'description', 'pricingRuleListElement_description' - The pricing rule description.
--
-- 'lastModifiedTime', 'pricingRuleListElement_lastModifiedTime' - The most recent time when the pricing rule was modified.
--
-- 'modifierPercentage', 'pricingRuleListElement_modifierPercentage' - A percentage modifier applied on the public pricing rates.
--
-- 'name', 'pricingRuleListElement_name' - The name of a pricing rule.
--
-- 'scope', 'pricingRuleListElement_scope' - The scope of pricing rule that indicates if it is globally applicable,
-- or if it is service-specific.
--
-- 'service', 'pricingRuleListElement_service' - If the @Scope@ attribute is @SERVICE@, this attribute indicates which
-- service the @PricingRule@ is applicable for.
--
-- 'tiering', 'pricingRuleListElement_tiering' - The set of tiering configurations for the pricing rule.
--
-- 'type'', 'pricingRuleListElement_type' - The type of pricing rule.
newPricingRuleListElement ::
  PricingRuleListElement
newPricingRuleListElement =
  PricingRuleListElement'
    { arn = Prelude.Nothing,
      associatedPricingPlanCount = Prelude.Nothing,
      billingEntity = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      modifierPercentage = Prelude.Nothing,
      name = Prelude.Nothing,
      scope = Prelude.Nothing,
      service = Prelude.Nothing,
      tiering = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) used to uniquely identify a pricing rule.
pricingRuleListElement_arn :: Lens.Lens' PricingRuleListElement (Prelude.Maybe Prelude.Text)
pricingRuleListElement_arn = Lens.lens (\PricingRuleListElement' {arn} -> arn) (\s@PricingRuleListElement' {} a -> s {arn = a} :: PricingRuleListElement)

-- | The pricing plans count that this pricing rule is associated with.
pricingRuleListElement_associatedPricingPlanCount :: Lens.Lens' PricingRuleListElement (Prelude.Maybe Prelude.Natural)
pricingRuleListElement_associatedPricingPlanCount = Lens.lens (\PricingRuleListElement' {associatedPricingPlanCount} -> associatedPricingPlanCount) (\s@PricingRuleListElement' {} a -> s {associatedPricingPlanCount = a} :: PricingRuleListElement)

-- | The seller of services provided by Amazon Web Services, their
-- affiliates, or third-party providers selling services via Amazon Web
-- Services Marketplace.
pricingRuleListElement_billingEntity :: Lens.Lens' PricingRuleListElement (Prelude.Maybe Prelude.Text)
pricingRuleListElement_billingEntity = Lens.lens (\PricingRuleListElement' {billingEntity} -> billingEntity) (\s@PricingRuleListElement' {} a -> s {billingEntity = a} :: PricingRuleListElement)

-- | The time when the pricing rule was created.
pricingRuleListElement_creationTime :: Lens.Lens' PricingRuleListElement (Prelude.Maybe Prelude.Integer)
pricingRuleListElement_creationTime = Lens.lens (\PricingRuleListElement' {creationTime} -> creationTime) (\s@PricingRuleListElement' {} a -> s {creationTime = a} :: PricingRuleListElement)

-- | The pricing rule description.
pricingRuleListElement_description :: Lens.Lens' PricingRuleListElement (Prelude.Maybe Prelude.Text)
pricingRuleListElement_description = Lens.lens (\PricingRuleListElement' {description} -> description) (\s@PricingRuleListElement' {} a -> s {description = a} :: PricingRuleListElement) Prelude.. Lens.mapping Data._Sensitive

-- | The most recent time when the pricing rule was modified.
pricingRuleListElement_lastModifiedTime :: Lens.Lens' PricingRuleListElement (Prelude.Maybe Prelude.Integer)
pricingRuleListElement_lastModifiedTime = Lens.lens (\PricingRuleListElement' {lastModifiedTime} -> lastModifiedTime) (\s@PricingRuleListElement' {} a -> s {lastModifiedTime = a} :: PricingRuleListElement)

-- | A percentage modifier applied on the public pricing rates.
pricingRuleListElement_modifierPercentage :: Lens.Lens' PricingRuleListElement (Prelude.Maybe Prelude.Double)
pricingRuleListElement_modifierPercentage = Lens.lens (\PricingRuleListElement' {modifierPercentage} -> modifierPercentage) (\s@PricingRuleListElement' {} a -> s {modifierPercentage = a} :: PricingRuleListElement)

-- | The name of a pricing rule.
pricingRuleListElement_name :: Lens.Lens' PricingRuleListElement (Prelude.Maybe Prelude.Text)
pricingRuleListElement_name = Lens.lens (\PricingRuleListElement' {name} -> name) (\s@PricingRuleListElement' {} a -> s {name = a} :: PricingRuleListElement) Prelude.. Lens.mapping Data._Sensitive

-- | The scope of pricing rule that indicates if it is globally applicable,
-- or if it is service-specific.
pricingRuleListElement_scope :: Lens.Lens' PricingRuleListElement (Prelude.Maybe PricingRuleScope)
pricingRuleListElement_scope = Lens.lens (\PricingRuleListElement' {scope} -> scope) (\s@PricingRuleListElement' {} a -> s {scope = a} :: PricingRuleListElement)

-- | If the @Scope@ attribute is @SERVICE@, this attribute indicates which
-- service the @PricingRule@ is applicable for.
pricingRuleListElement_service :: Lens.Lens' PricingRuleListElement (Prelude.Maybe Prelude.Text)
pricingRuleListElement_service = Lens.lens (\PricingRuleListElement' {service} -> service) (\s@PricingRuleListElement' {} a -> s {service = a} :: PricingRuleListElement)

-- | The set of tiering configurations for the pricing rule.
pricingRuleListElement_tiering :: Lens.Lens' PricingRuleListElement (Prelude.Maybe Tiering)
pricingRuleListElement_tiering = Lens.lens (\PricingRuleListElement' {tiering} -> tiering) (\s@PricingRuleListElement' {} a -> s {tiering = a} :: PricingRuleListElement)

-- | The type of pricing rule.
pricingRuleListElement_type :: Lens.Lens' PricingRuleListElement (Prelude.Maybe PricingRuleType)
pricingRuleListElement_type = Lens.lens (\PricingRuleListElement' {type'} -> type') (\s@PricingRuleListElement' {} a -> s {type' = a} :: PricingRuleListElement)

instance Data.FromJSON PricingRuleListElement where
  parseJSON =
    Data.withObject
      "PricingRuleListElement"
      ( \x ->
          PricingRuleListElement'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "AssociatedPricingPlanCount")
            Prelude.<*> (x Data..:? "BillingEntity")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "ModifierPercentage")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Scope")
            Prelude.<*> (x Data..:? "Service")
            Prelude.<*> (x Data..:? "Tiering")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable PricingRuleListElement where
  hashWithSalt _salt PricingRuleListElement' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` associatedPricingPlanCount
      `Prelude.hashWithSalt` billingEntity
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` modifierPercentage
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` service
      `Prelude.hashWithSalt` tiering
      `Prelude.hashWithSalt` type'

instance Prelude.NFData PricingRuleListElement where
  rnf PricingRuleListElement' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf associatedPricingPlanCount
      `Prelude.seq` Prelude.rnf billingEntity
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf modifierPercentage
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf service
      `Prelude.seq` Prelude.rnf tiering
      `Prelude.seq` Prelude.rnf type'
