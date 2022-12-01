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
-- Module      : Amazonka.BillingConductor.Types.CustomLineItemListElement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.CustomLineItemListElement where

import Amazonka.BillingConductor.Types.CurrencyCode
import Amazonka.BillingConductor.Types.ListCustomLineItemChargeDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A representation of a custom line item.
--
-- /See:/ 'newCustomLineItemListElement' smart constructor.
data CustomLineItemListElement = CustomLineItemListElement'
  { -- | The custom line item\'s name.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A @ListCustomLineItemChargeDetails@ that describes the charge details of
    -- a custom line item.
    chargeDetails :: Prelude.Maybe ListCustomLineItemChargeDetails,
    -- | The Amazon Resource Name (ARN) that references the billing group where
    -- the custom line item applies to.
    billingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARNs) for custom line items.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The number of resources that are associated to the custom line item.
    associationSize :: Prelude.Maybe Prelude.Natural,
    -- | The product code that\'s associated with the custom line item.
    productCode :: Prelude.Maybe Prelude.Text,
    -- | The custom line item\'s description. This is shown on the Bills page in
    -- association with the charge value.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The custom line item\'s charge value currency. Only one of the valid
    -- values can be used.
    currencyCode :: Prelude.Maybe CurrencyCode,
    -- | The most recent time when the custom line item was modified.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | The time created.
    creationTime :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomLineItemListElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'customLineItemListElement_name' - The custom line item\'s name.
--
-- 'chargeDetails', 'customLineItemListElement_chargeDetails' - A @ListCustomLineItemChargeDetails@ that describes the charge details of
-- a custom line item.
--
-- 'billingGroupArn', 'customLineItemListElement_billingGroupArn' - The Amazon Resource Name (ARN) that references the billing group where
-- the custom line item applies to.
--
-- 'arn', 'customLineItemListElement_arn' - The Amazon Resource Names (ARNs) for custom line items.
--
-- 'associationSize', 'customLineItemListElement_associationSize' - The number of resources that are associated to the custom line item.
--
-- 'productCode', 'customLineItemListElement_productCode' - The product code that\'s associated with the custom line item.
--
-- 'description', 'customLineItemListElement_description' - The custom line item\'s description. This is shown on the Bills page in
-- association with the charge value.
--
-- 'currencyCode', 'customLineItemListElement_currencyCode' - The custom line item\'s charge value currency. Only one of the valid
-- values can be used.
--
-- 'lastModifiedTime', 'customLineItemListElement_lastModifiedTime' - The most recent time when the custom line item was modified.
--
-- 'creationTime', 'customLineItemListElement_creationTime' - The time created.
newCustomLineItemListElement ::
  CustomLineItemListElement
newCustomLineItemListElement =
  CustomLineItemListElement'
    { name = Prelude.Nothing,
      chargeDetails = Prelude.Nothing,
      billingGroupArn = Prelude.Nothing,
      arn = Prelude.Nothing,
      associationSize = Prelude.Nothing,
      productCode = Prelude.Nothing,
      description = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      creationTime = Prelude.Nothing
    }

-- | The custom line item\'s name.
customLineItemListElement_name :: Lens.Lens' CustomLineItemListElement (Prelude.Maybe Prelude.Text)
customLineItemListElement_name = Lens.lens (\CustomLineItemListElement' {name} -> name) (\s@CustomLineItemListElement' {} a -> s {name = a} :: CustomLineItemListElement) Prelude.. Lens.mapping Core._Sensitive

-- | A @ListCustomLineItemChargeDetails@ that describes the charge details of
-- a custom line item.
customLineItemListElement_chargeDetails :: Lens.Lens' CustomLineItemListElement (Prelude.Maybe ListCustomLineItemChargeDetails)
customLineItemListElement_chargeDetails = Lens.lens (\CustomLineItemListElement' {chargeDetails} -> chargeDetails) (\s@CustomLineItemListElement' {} a -> s {chargeDetails = a} :: CustomLineItemListElement)

-- | The Amazon Resource Name (ARN) that references the billing group where
-- the custom line item applies to.
customLineItemListElement_billingGroupArn :: Lens.Lens' CustomLineItemListElement (Prelude.Maybe Prelude.Text)
customLineItemListElement_billingGroupArn = Lens.lens (\CustomLineItemListElement' {billingGroupArn} -> billingGroupArn) (\s@CustomLineItemListElement' {} a -> s {billingGroupArn = a} :: CustomLineItemListElement)

-- | The Amazon Resource Names (ARNs) for custom line items.
customLineItemListElement_arn :: Lens.Lens' CustomLineItemListElement (Prelude.Maybe Prelude.Text)
customLineItemListElement_arn = Lens.lens (\CustomLineItemListElement' {arn} -> arn) (\s@CustomLineItemListElement' {} a -> s {arn = a} :: CustomLineItemListElement)

-- | The number of resources that are associated to the custom line item.
customLineItemListElement_associationSize :: Lens.Lens' CustomLineItemListElement (Prelude.Maybe Prelude.Natural)
customLineItemListElement_associationSize = Lens.lens (\CustomLineItemListElement' {associationSize} -> associationSize) (\s@CustomLineItemListElement' {} a -> s {associationSize = a} :: CustomLineItemListElement)

-- | The product code that\'s associated with the custom line item.
customLineItemListElement_productCode :: Lens.Lens' CustomLineItemListElement (Prelude.Maybe Prelude.Text)
customLineItemListElement_productCode = Lens.lens (\CustomLineItemListElement' {productCode} -> productCode) (\s@CustomLineItemListElement' {} a -> s {productCode = a} :: CustomLineItemListElement)

-- | The custom line item\'s description. This is shown on the Bills page in
-- association with the charge value.
customLineItemListElement_description :: Lens.Lens' CustomLineItemListElement (Prelude.Maybe Prelude.Text)
customLineItemListElement_description = Lens.lens (\CustomLineItemListElement' {description} -> description) (\s@CustomLineItemListElement' {} a -> s {description = a} :: CustomLineItemListElement) Prelude.. Lens.mapping Core._Sensitive

-- | The custom line item\'s charge value currency. Only one of the valid
-- values can be used.
customLineItemListElement_currencyCode :: Lens.Lens' CustomLineItemListElement (Prelude.Maybe CurrencyCode)
customLineItemListElement_currencyCode = Lens.lens (\CustomLineItemListElement' {currencyCode} -> currencyCode) (\s@CustomLineItemListElement' {} a -> s {currencyCode = a} :: CustomLineItemListElement)

-- | The most recent time when the custom line item was modified.
customLineItemListElement_lastModifiedTime :: Lens.Lens' CustomLineItemListElement (Prelude.Maybe Prelude.Integer)
customLineItemListElement_lastModifiedTime = Lens.lens (\CustomLineItemListElement' {lastModifiedTime} -> lastModifiedTime) (\s@CustomLineItemListElement' {} a -> s {lastModifiedTime = a} :: CustomLineItemListElement)

-- | The time created.
customLineItemListElement_creationTime :: Lens.Lens' CustomLineItemListElement (Prelude.Maybe Prelude.Integer)
customLineItemListElement_creationTime = Lens.lens (\CustomLineItemListElement' {creationTime} -> creationTime) (\s@CustomLineItemListElement' {} a -> s {creationTime = a} :: CustomLineItemListElement)

instance Core.FromJSON CustomLineItemListElement where
  parseJSON =
    Core.withObject
      "CustomLineItemListElement"
      ( \x ->
          CustomLineItemListElement'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ChargeDetails")
            Prelude.<*> (x Core..:? "BillingGroupArn")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "AssociationSize")
            Prelude.<*> (x Core..:? "ProductCode")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "CurrencyCode")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "CreationTime")
      )

instance Prelude.Hashable CustomLineItemListElement where
  hashWithSalt _salt CustomLineItemListElement' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` chargeDetails
      `Prelude.hashWithSalt` billingGroupArn
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` associationSize
      `Prelude.hashWithSalt` productCode
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData CustomLineItemListElement where
  rnf CustomLineItemListElement' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf chargeDetails
      `Prelude.seq` Prelude.rnf billingGroupArn
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf associationSize
      `Prelude.seq` Prelude.rnf productCode
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf creationTime
