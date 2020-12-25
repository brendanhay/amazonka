{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.Types.UsageRecordResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceMetering.Types.UsageRecordResult
  ( UsageRecordResult (..),

    -- * Smart constructor
    mkUsageRecordResult,

    -- * Lenses
    urrMeteringRecordId,
    urrStatus,
    urrUsageRecord,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MarketplaceMetering.Types.String as Types
import qualified Network.AWS.MarketplaceMetering.Types.UsageRecord as Types
import qualified Network.AWS.MarketplaceMetering.Types.UsageRecordResultStatus as Types
import qualified Network.AWS.Prelude as Core

-- | A UsageRecordResult indicates the status of a given UsageRecord processed by BatchMeterUsage.
--
-- /See:/ 'mkUsageRecordResult' smart constructor.
data UsageRecordResult = UsageRecordResult'
  { -- | The MeteringRecordId is a unique identifier for this metering event.
    meteringRecordId :: Core.Maybe Types.String,
    -- | The UsageRecordResult Status indicates the status of an individual UsageRecord processed by BatchMeterUsage.
    --
    --
    --     * /Success/ - The UsageRecord was accepted and honored by BatchMeterUsage.
    --
    --
    --     * /CustomerNotSubscribed/ - The CustomerIdentifier specified is not subscribed to your product. The UsageRecord was not honored. Future UsageRecords for this customer will fail until the customer subscribes to your product.
    --
    --
    --     * /DuplicateRecord/ - Indicates that the UsageRecord was invalid and not honored. A previously metered UsageRecord had the same customer, dimension, and time, but a different quantity.
    status :: Core.Maybe Types.UsageRecordResultStatus,
    -- | The UsageRecord that was part of the BatchMeterUsage request.
    usageRecord :: Core.Maybe Types.UsageRecord
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UsageRecordResult' value with any optional fields omitted.
mkUsageRecordResult ::
  UsageRecordResult
mkUsageRecordResult =
  UsageRecordResult'
    { meteringRecordId = Core.Nothing,
      status = Core.Nothing,
      usageRecord = Core.Nothing
    }

-- | The MeteringRecordId is a unique identifier for this metering event.
--
-- /Note:/ Consider using 'meteringRecordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrMeteringRecordId :: Lens.Lens' UsageRecordResult (Core.Maybe Types.String)
urrMeteringRecordId = Lens.field @"meteringRecordId"
{-# DEPRECATED urrMeteringRecordId "Use generic-lens or generic-optics with 'meteringRecordId' instead." #-}

-- | The UsageRecordResult Status indicates the status of an individual UsageRecord processed by BatchMeterUsage.
--
--
--     * /Success/ - The UsageRecord was accepted and honored by BatchMeterUsage.
--
--
--     * /CustomerNotSubscribed/ - The CustomerIdentifier specified is not subscribed to your product. The UsageRecord was not honored. Future UsageRecords for this customer will fail until the customer subscribes to your product.
--
--
--     * /DuplicateRecord/ - Indicates that the UsageRecord was invalid and not honored. A previously metered UsageRecord had the same customer, dimension, and time, but a different quantity.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrStatus :: Lens.Lens' UsageRecordResult (Core.Maybe Types.UsageRecordResultStatus)
urrStatus = Lens.field @"status"
{-# DEPRECATED urrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The UsageRecord that was part of the BatchMeterUsage request.
--
-- /Note:/ Consider using 'usageRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrUsageRecord :: Lens.Lens' UsageRecordResult (Core.Maybe Types.UsageRecord)
urrUsageRecord = Lens.field @"usageRecord"
{-# DEPRECATED urrUsageRecord "Use generic-lens or generic-optics with 'usageRecord' instead." #-}

instance Core.FromJSON UsageRecordResult where
  parseJSON =
    Core.withObject "UsageRecordResult" Core.$
      \x ->
        UsageRecordResult'
          Core.<$> (x Core..:? "MeteringRecordId")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "UsageRecord")
