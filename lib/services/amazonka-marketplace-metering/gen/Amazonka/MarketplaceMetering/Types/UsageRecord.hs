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
-- Module      : Amazonka.MarketplaceMetering.Types.UsageRecord
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceMetering.Types.UsageRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MarketplaceMetering.Types.UsageAllocation
import qualified Amazonka.Prelude as Prelude

-- | A @UsageRecord@ indicates a quantity of usage for a given product,
-- customer, dimension and time.
--
-- Multiple requests with the same @UsageRecords@ as input will be
-- de-duplicated to prevent double charges.
--
-- /See:/ 'newUsageRecord' smart constructor.
data UsageRecord = UsageRecord'
  { -- | The quantity of usage consumed by the customer for the given dimension
    -- and time. Defaults to @0@ if not specified.
    quantity :: Prelude.Maybe Prelude.Natural,
    -- | The set of @UsageAllocations@ to submit. The sum of all
    -- @UsageAllocation@ quantities must equal the Quantity of the
    -- @UsageRecord@.
    usageAllocations :: Prelude.Maybe (Prelude.NonEmpty UsageAllocation),
    -- | Timestamp, in UTC, for which the usage is being reported.
    --
    -- Your application can meter usage for up to one hour in the past. Make
    -- sure the @timestamp@ value is not before the start of the software
    -- usage.
    timestamp :: Core.POSIX,
    -- | The @CustomerIdentifier@ is obtained through the @ResolveCustomer@
    -- operation and represents an individual buyer in your application.
    customerIdentifier :: Prelude.Text,
    -- | During the process of registering a product on AWS Marketplace,
    -- dimensions are specified. These represent different units of value in
    -- your application.
    dimension :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsageRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quantity', 'usageRecord_quantity' - The quantity of usage consumed by the customer for the given dimension
-- and time. Defaults to @0@ if not specified.
--
-- 'usageAllocations', 'usageRecord_usageAllocations' - The set of @UsageAllocations@ to submit. The sum of all
-- @UsageAllocation@ quantities must equal the Quantity of the
-- @UsageRecord@.
--
-- 'timestamp', 'usageRecord_timestamp' - Timestamp, in UTC, for which the usage is being reported.
--
-- Your application can meter usage for up to one hour in the past. Make
-- sure the @timestamp@ value is not before the start of the software
-- usage.
--
-- 'customerIdentifier', 'usageRecord_customerIdentifier' - The @CustomerIdentifier@ is obtained through the @ResolveCustomer@
-- operation and represents an individual buyer in your application.
--
-- 'dimension', 'usageRecord_dimension' - During the process of registering a product on AWS Marketplace,
-- dimensions are specified. These represent different units of value in
-- your application.
newUsageRecord ::
  -- | 'timestamp'
  Prelude.UTCTime ->
  -- | 'customerIdentifier'
  Prelude.Text ->
  -- | 'dimension'
  Prelude.Text ->
  UsageRecord
newUsageRecord
  pTimestamp_
  pCustomerIdentifier_
  pDimension_ =
    UsageRecord'
      { quantity = Prelude.Nothing,
        usageAllocations = Prelude.Nothing,
        timestamp = Core._Time Lens.# pTimestamp_,
        customerIdentifier = pCustomerIdentifier_,
        dimension = pDimension_
      }

-- | The quantity of usage consumed by the customer for the given dimension
-- and time. Defaults to @0@ if not specified.
usageRecord_quantity :: Lens.Lens' UsageRecord (Prelude.Maybe Prelude.Natural)
usageRecord_quantity = Lens.lens (\UsageRecord' {quantity} -> quantity) (\s@UsageRecord' {} a -> s {quantity = a} :: UsageRecord)

-- | The set of @UsageAllocations@ to submit. The sum of all
-- @UsageAllocation@ quantities must equal the Quantity of the
-- @UsageRecord@.
usageRecord_usageAllocations :: Lens.Lens' UsageRecord (Prelude.Maybe (Prelude.NonEmpty UsageAllocation))
usageRecord_usageAllocations = Lens.lens (\UsageRecord' {usageAllocations} -> usageAllocations) (\s@UsageRecord' {} a -> s {usageAllocations = a} :: UsageRecord) Prelude.. Lens.mapping Lens.coerced

-- | Timestamp, in UTC, for which the usage is being reported.
--
-- Your application can meter usage for up to one hour in the past. Make
-- sure the @timestamp@ value is not before the start of the software
-- usage.
usageRecord_timestamp :: Lens.Lens' UsageRecord Prelude.UTCTime
usageRecord_timestamp = Lens.lens (\UsageRecord' {timestamp} -> timestamp) (\s@UsageRecord' {} a -> s {timestamp = a} :: UsageRecord) Prelude.. Core._Time

-- | The @CustomerIdentifier@ is obtained through the @ResolveCustomer@
-- operation and represents an individual buyer in your application.
usageRecord_customerIdentifier :: Lens.Lens' UsageRecord Prelude.Text
usageRecord_customerIdentifier = Lens.lens (\UsageRecord' {customerIdentifier} -> customerIdentifier) (\s@UsageRecord' {} a -> s {customerIdentifier = a} :: UsageRecord)

-- | During the process of registering a product on AWS Marketplace,
-- dimensions are specified. These represent different units of value in
-- your application.
usageRecord_dimension :: Lens.Lens' UsageRecord Prelude.Text
usageRecord_dimension = Lens.lens (\UsageRecord' {dimension} -> dimension) (\s@UsageRecord' {} a -> s {dimension = a} :: UsageRecord)

instance Core.FromJSON UsageRecord where
  parseJSON =
    Core.withObject
      "UsageRecord"
      ( \x ->
          UsageRecord'
            Prelude.<$> (x Core..:? "Quantity")
            Prelude.<*> (x Core..:? "UsageAllocations")
            Prelude.<*> (x Core..: "Timestamp")
            Prelude.<*> (x Core..: "CustomerIdentifier")
            Prelude.<*> (x Core..: "Dimension")
      )

instance Prelude.Hashable UsageRecord where
  hashWithSalt _salt UsageRecord' {..} =
    _salt `Prelude.hashWithSalt` quantity
      `Prelude.hashWithSalt` usageAllocations
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` customerIdentifier
      `Prelude.hashWithSalt` dimension

instance Prelude.NFData UsageRecord where
  rnf UsageRecord' {..} =
    Prelude.rnf quantity
      `Prelude.seq` Prelude.rnf usageAllocations
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf customerIdentifier
      `Prelude.seq` Prelude.rnf dimension

instance Core.ToJSON UsageRecord where
  toJSON UsageRecord' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Quantity" Core..=) Prelude.<$> quantity,
            ("UsageAllocations" Core..=)
              Prelude.<$> usageAllocations,
            Prelude.Just ("Timestamp" Core..= timestamp),
            Prelude.Just
              ("CustomerIdentifier" Core..= customerIdentifier),
            Prelude.Just ("Dimension" Core..= dimension)
          ]
      )
