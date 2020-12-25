{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.Types.UsageRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceMetering.Types.UsageRecord
  ( UsageRecord (..),

    -- * Smart constructor
    mkUsageRecord,

    -- * Lenses
    urTimestamp,
    urCustomerIdentifier,
    urDimension,
    urQuantity,
    urUsageAllocations,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MarketplaceMetering.Types.CustomerIdentifier as Types
import qualified Network.AWS.MarketplaceMetering.Types.UsageAllocation as Types
import qualified Network.AWS.MarketplaceMetering.Types.UsageDimension as Types
import qualified Network.AWS.Prelude as Core

-- | A UsageRecord indicates a quantity of usage for a given product, customer, dimension and time.
--
-- Multiple requests with the same UsageRecords as input will be deduplicated to prevent double charges.
--
-- /See:/ 'mkUsageRecord' smart constructor.
data UsageRecord = UsageRecord'
  { -- | Timestamp, in UTC, for which the usage is being reported.
    --
    -- Your application can meter usage for up to one hour in the past. Make sure the timestamp value is not before the start of the software usage.
    timestamp :: Core.NominalDiffTime,
    -- | The CustomerIdentifier is obtained through the ResolveCustomer operation and represents an individual buyer in your application.
    customerIdentifier :: Types.CustomerIdentifier,
    -- | During the process of registering a product on AWS Marketplace, up to eight dimensions are specified. These represent different units of value in your application.
    dimension :: Types.UsageDimension,
    -- | The quantity of usage consumed by the customer for the given dimension and time. Defaults to @0@ if not specified.
    quantity :: Core.Maybe Core.Natural,
    -- | The set of UsageAllocations to submit. The sum of all UsageAllocation quantities must equal the Quantity of the UsageRecord.
    usageAllocations :: Core.Maybe (Core.NonEmpty Types.UsageAllocation)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UsageRecord' value with any optional fields omitted.
mkUsageRecord ::
  -- | 'timestamp'
  Core.NominalDiffTime ->
  -- | 'customerIdentifier'
  Types.CustomerIdentifier ->
  -- | 'dimension'
  Types.UsageDimension ->
  UsageRecord
mkUsageRecord timestamp customerIdentifier dimension =
  UsageRecord'
    { timestamp,
      customerIdentifier,
      dimension,
      quantity = Core.Nothing,
      usageAllocations = Core.Nothing
    }

-- | Timestamp, in UTC, for which the usage is being reported.
--
-- Your application can meter usage for up to one hour in the past. Make sure the timestamp value is not before the start of the software usage.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTimestamp :: Lens.Lens' UsageRecord Core.NominalDiffTime
urTimestamp = Lens.field @"timestamp"
{-# DEPRECATED urTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The CustomerIdentifier is obtained through the ResolveCustomer operation and represents an individual buyer in your application.
--
-- /Note:/ Consider using 'customerIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urCustomerIdentifier :: Lens.Lens' UsageRecord Types.CustomerIdentifier
urCustomerIdentifier = Lens.field @"customerIdentifier"
{-# DEPRECATED urCustomerIdentifier "Use generic-lens or generic-optics with 'customerIdentifier' instead." #-}

-- | During the process of registering a product on AWS Marketplace, up to eight dimensions are specified. These represent different units of value in your application.
--
-- /Note:/ Consider using 'dimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDimension :: Lens.Lens' UsageRecord Types.UsageDimension
urDimension = Lens.field @"dimension"
{-# DEPRECATED urDimension "Use generic-lens or generic-optics with 'dimension' instead." #-}

-- | The quantity of usage consumed by the customer for the given dimension and time. Defaults to @0@ if not specified.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urQuantity :: Lens.Lens' UsageRecord (Core.Maybe Core.Natural)
urQuantity = Lens.field @"quantity"
{-# DEPRECATED urQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | The set of UsageAllocations to submit. The sum of all UsageAllocation quantities must equal the Quantity of the UsageRecord.
--
-- /Note:/ Consider using 'usageAllocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urUsageAllocations :: Lens.Lens' UsageRecord (Core.Maybe (Core.NonEmpty Types.UsageAllocation))
urUsageAllocations = Lens.field @"usageAllocations"
{-# DEPRECATED urUsageAllocations "Use generic-lens or generic-optics with 'usageAllocations' instead." #-}

instance Core.FromJSON UsageRecord where
  toJSON UsageRecord {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Timestamp" Core..= timestamp),
            Core.Just ("CustomerIdentifier" Core..= customerIdentifier),
            Core.Just ("Dimension" Core..= dimension),
            ("Quantity" Core..=) Core.<$> quantity,
            ("UsageAllocations" Core..=) Core.<$> usageAllocations
          ]
      )

instance Core.FromJSON UsageRecord where
  parseJSON =
    Core.withObject "UsageRecord" Core.$
      \x ->
        UsageRecord'
          Core.<$> (x Core..: "Timestamp")
          Core.<*> (x Core..: "CustomerIdentifier")
          Core.<*> (x Core..: "Dimension")
          Core.<*> (x Core..:? "Quantity")
          Core.<*> (x Core..:? "UsageAllocations")
