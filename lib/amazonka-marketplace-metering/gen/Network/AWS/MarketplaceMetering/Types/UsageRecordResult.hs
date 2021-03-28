{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.Types.UsageRecordResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MarketplaceMetering.Types.UsageRecordResult
  ( UsageRecordResult (..)
  -- * Smart constructor
  , mkUsageRecordResult
  -- * Lenses
  , urrMeteringRecordId
  , urrStatus
  , urrUsageRecord
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MarketplaceMetering.Types.UsageRecord as Types
import qualified Network.AWS.MarketplaceMetering.Types.UsageRecordResultStatus as Types
import qualified Network.AWS.Prelude as Core

-- | A UsageRecordResult indicates the status of a given UsageRecord processed by BatchMeterUsage.
--
-- /See:/ 'mkUsageRecordResult' smart constructor.
data UsageRecordResult = UsageRecordResult'
  { meteringRecordId :: Core.Maybe Core.Text
    -- ^ The MeteringRecordId is a unique identifier for this metering event.
  , status :: Core.Maybe Types.UsageRecordResultStatus
    -- ^ The UsageRecordResult Status indicates the status of an individual UsageRecord processed by BatchMeterUsage.
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
  , usageRecord :: Core.Maybe Types.UsageRecord
    -- ^ The UsageRecord that was part of the BatchMeterUsage request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UsageRecordResult' value with any optional fields omitted.
mkUsageRecordResult
    :: UsageRecordResult
mkUsageRecordResult
  = UsageRecordResult'{meteringRecordId = Core.Nothing,
                       status = Core.Nothing, usageRecord = Core.Nothing}

-- | The MeteringRecordId is a unique identifier for this metering event.
--
-- /Note:/ Consider using 'meteringRecordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrMeteringRecordId :: Lens.Lens' UsageRecordResult (Core.Maybe Core.Text)
urrMeteringRecordId = Lens.field @"meteringRecordId"
{-# INLINEABLE urrMeteringRecordId #-}
{-# DEPRECATED meteringRecordId "Use generic-lens or generic-optics with 'meteringRecordId' instead"  #-}

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
{-# INLINEABLE urrStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The UsageRecord that was part of the BatchMeterUsage request.
--
-- /Note:/ Consider using 'usageRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrUsageRecord :: Lens.Lens' UsageRecordResult (Core.Maybe Types.UsageRecord)
urrUsageRecord = Lens.field @"usageRecord"
{-# INLINEABLE urrUsageRecord #-}
{-# DEPRECATED usageRecord "Use generic-lens or generic-optics with 'usageRecord' instead"  #-}

instance Core.FromJSON UsageRecordResult where
        parseJSON
          = Core.withObject "UsageRecordResult" Core.$
              \ x ->
                UsageRecordResult' Core.<$>
                  (x Core..:? "MeteringRecordId") Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "UsageRecord"
