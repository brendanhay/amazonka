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
-- Module      : Amazonka.BillingConductor.Types.CustomLineItemVersionListElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.CustomLineItemVersionListElement where

import Amazonka.BillingConductor.Types.CurrencyCode
import Amazonka.BillingConductor.Types.ListCustomLineItemChargeDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of a custom line item version.
--
-- /See:/ 'newCustomLineItemVersionListElement' smart constructor.
data CustomLineItemVersionListElement = CustomLineItemVersionListElement'
  { -- | The number of resources that are associated with the custom line item.
    associationSize :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the billing group that the custom line
    -- item applies to.
    billingGroupArn :: Prelude.Maybe Prelude.Text,
    chargeDetails :: Prelude.Maybe ListCustomLineItemChargeDetails,
    -- | The time when the custom line item version was created.
    creationTime :: Prelude.Maybe Prelude.Integer,
    -- | The charge value currency of the custom line item.
    currencyCode :: Prelude.Maybe CurrencyCode,
    -- | The description of the custom line item.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The end billing period of the custom line item version.
    endBillingPeriod :: Prelude.Maybe Prelude.Text,
    -- | The most recent time that the custom line item version was modified.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | The name of the custom line item.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The product code that’s associated with the custom line item.
    productCode :: Prelude.Maybe Prelude.Text,
    -- | The start billing period of the custom line item version.
    startBillingPeriod :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomLineItemVersionListElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationSize', 'customLineItemVersionListElement_associationSize' - The number of resources that are associated with the custom line item.
--
-- 'billingGroupArn', 'customLineItemVersionListElement_billingGroupArn' - The Amazon Resource Name (ARN) of the billing group that the custom line
-- item applies to.
--
-- 'chargeDetails', 'customLineItemVersionListElement_chargeDetails' - Undocumented member.
--
-- 'creationTime', 'customLineItemVersionListElement_creationTime' - The time when the custom line item version was created.
--
-- 'currencyCode', 'customLineItemVersionListElement_currencyCode' - The charge value currency of the custom line item.
--
-- 'description', 'customLineItemVersionListElement_description' - The description of the custom line item.
--
-- 'endBillingPeriod', 'customLineItemVersionListElement_endBillingPeriod' - The end billing period of the custom line item version.
--
-- 'lastModifiedTime', 'customLineItemVersionListElement_lastModifiedTime' - The most recent time that the custom line item version was modified.
--
-- 'name', 'customLineItemVersionListElement_name' - The name of the custom line item.
--
-- 'productCode', 'customLineItemVersionListElement_productCode' - The product code that’s associated with the custom line item.
--
-- 'startBillingPeriod', 'customLineItemVersionListElement_startBillingPeriod' - The start billing period of the custom line item version.
newCustomLineItemVersionListElement ::
  CustomLineItemVersionListElement
newCustomLineItemVersionListElement =
  CustomLineItemVersionListElement'
    { associationSize =
        Prelude.Nothing,
      billingGroupArn = Prelude.Nothing,
      chargeDetails = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      description = Prelude.Nothing,
      endBillingPeriod = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      productCode = Prelude.Nothing,
      startBillingPeriod = Prelude.Nothing
    }

-- | The number of resources that are associated with the custom line item.
customLineItemVersionListElement_associationSize :: Lens.Lens' CustomLineItemVersionListElement (Prelude.Maybe Prelude.Natural)
customLineItemVersionListElement_associationSize = Lens.lens (\CustomLineItemVersionListElement' {associationSize} -> associationSize) (\s@CustomLineItemVersionListElement' {} a -> s {associationSize = a} :: CustomLineItemVersionListElement)

-- | The Amazon Resource Name (ARN) of the billing group that the custom line
-- item applies to.
customLineItemVersionListElement_billingGroupArn :: Lens.Lens' CustomLineItemVersionListElement (Prelude.Maybe Prelude.Text)
customLineItemVersionListElement_billingGroupArn = Lens.lens (\CustomLineItemVersionListElement' {billingGroupArn} -> billingGroupArn) (\s@CustomLineItemVersionListElement' {} a -> s {billingGroupArn = a} :: CustomLineItemVersionListElement)

-- | Undocumented member.
customLineItemVersionListElement_chargeDetails :: Lens.Lens' CustomLineItemVersionListElement (Prelude.Maybe ListCustomLineItemChargeDetails)
customLineItemVersionListElement_chargeDetails = Lens.lens (\CustomLineItemVersionListElement' {chargeDetails} -> chargeDetails) (\s@CustomLineItemVersionListElement' {} a -> s {chargeDetails = a} :: CustomLineItemVersionListElement)

-- | The time when the custom line item version was created.
customLineItemVersionListElement_creationTime :: Lens.Lens' CustomLineItemVersionListElement (Prelude.Maybe Prelude.Integer)
customLineItemVersionListElement_creationTime = Lens.lens (\CustomLineItemVersionListElement' {creationTime} -> creationTime) (\s@CustomLineItemVersionListElement' {} a -> s {creationTime = a} :: CustomLineItemVersionListElement)

-- | The charge value currency of the custom line item.
customLineItemVersionListElement_currencyCode :: Lens.Lens' CustomLineItemVersionListElement (Prelude.Maybe CurrencyCode)
customLineItemVersionListElement_currencyCode = Lens.lens (\CustomLineItemVersionListElement' {currencyCode} -> currencyCode) (\s@CustomLineItemVersionListElement' {} a -> s {currencyCode = a} :: CustomLineItemVersionListElement)

-- | The description of the custom line item.
customLineItemVersionListElement_description :: Lens.Lens' CustomLineItemVersionListElement (Prelude.Maybe Prelude.Text)
customLineItemVersionListElement_description = Lens.lens (\CustomLineItemVersionListElement' {description} -> description) (\s@CustomLineItemVersionListElement' {} a -> s {description = a} :: CustomLineItemVersionListElement) Prelude.. Lens.mapping Data._Sensitive

-- | The end billing period of the custom line item version.
customLineItemVersionListElement_endBillingPeriod :: Lens.Lens' CustomLineItemVersionListElement (Prelude.Maybe Prelude.Text)
customLineItemVersionListElement_endBillingPeriod = Lens.lens (\CustomLineItemVersionListElement' {endBillingPeriod} -> endBillingPeriod) (\s@CustomLineItemVersionListElement' {} a -> s {endBillingPeriod = a} :: CustomLineItemVersionListElement)

-- | The most recent time that the custom line item version was modified.
customLineItemVersionListElement_lastModifiedTime :: Lens.Lens' CustomLineItemVersionListElement (Prelude.Maybe Prelude.Integer)
customLineItemVersionListElement_lastModifiedTime = Lens.lens (\CustomLineItemVersionListElement' {lastModifiedTime} -> lastModifiedTime) (\s@CustomLineItemVersionListElement' {} a -> s {lastModifiedTime = a} :: CustomLineItemVersionListElement)

-- | The name of the custom line item.
customLineItemVersionListElement_name :: Lens.Lens' CustomLineItemVersionListElement (Prelude.Maybe Prelude.Text)
customLineItemVersionListElement_name = Lens.lens (\CustomLineItemVersionListElement' {name} -> name) (\s@CustomLineItemVersionListElement' {} a -> s {name = a} :: CustomLineItemVersionListElement) Prelude.. Lens.mapping Data._Sensitive

-- | The product code that’s associated with the custom line item.
customLineItemVersionListElement_productCode :: Lens.Lens' CustomLineItemVersionListElement (Prelude.Maybe Prelude.Text)
customLineItemVersionListElement_productCode = Lens.lens (\CustomLineItemVersionListElement' {productCode} -> productCode) (\s@CustomLineItemVersionListElement' {} a -> s {productCode = a} :: CustomLineItemVersionListElement)

-- | The start billing period of the custom line item version.
customLineItemVersionListElement_startBillingPeriod :: Lens.Lens' CustomLineItemVersionListElement (Prelude.Maybe Prelude.Text)
customLineItemVersionListElement_startBillingPeriod = Lens.lens (\CustomLineItemVersionListElement' {startBillingPeriod} -> startBillingPeriod) (\s@CustomLineItemVersionListElement' {} a -> s {startBillingPeriod = a} :: CustomLineItemVersionListElement)

instance
  Data.FromJSON
    CustomLineItemVersionListElement
  where
  parseJSON =
    Data.withObject
      "CustomLineItemVersionListElement"
      ( \x ->
          CustomLineItemVersionListElement'
            Prelude.<$> (x Data..:? "AssociationSize")
            Prelude.<*> (x Data..:? "BillingGroupArn")
            Prelude.<*> (x Data..:? "ChargeDetails")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "CurrencyCode")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EndBillingPeriod")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ProductCode")
            Prelude.<*> (x Data..:? "StartBillingPeriod")
      )

instance
  Prelude.Hashable
    CustomLineItemVersionListElement
  where
  hashWithSalt
    _salt
    CustomLineItemVersionListElement' {..} =
      _salt
        `Prelude.hashWithSalt` associationSize
        `Prelude.hashWithSalt` billingGroupArn
        `Prelude.hashWithSalt` chargeDetails
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` currencyCode
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` endBillingPeriod
        `Prelude.hashWithSalt` lastModifiedTime
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` productCode
        `Prelude.hashWithSalt` startBillingPeriod

instance
  Prelude.NFData
    CustomLineItemVersionListElement
  where
  rnf CustomLineItemVersionListElement' {..} =
    Prelude.rnf associationSize
      `Prelude.seq` Prelude.rnf billingGroupArn
      `Prelude.seq` Prelude.rnf chargeDetails
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf endBillingPeriod
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf productCode
      `Prelude.seq` Prelude.rnf startBillingPeriod
