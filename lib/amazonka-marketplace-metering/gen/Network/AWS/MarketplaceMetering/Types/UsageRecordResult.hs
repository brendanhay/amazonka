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
    urrStatus,
    urrUsageRecord,
    urrMeteringRecordId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceMetering.Types.UsageRecord
import Network.AWS.MarketplaceMetering.Types.UsageRecordResultStatus
import qualified Network.AWS.Prelude as Lude

-- | A UsageRecordResult indicates the status of a given UsageRecord processed by BatchMeterUsage.
--
-- /See:/ 'mkUsageRecordResult' smart constructor.
data UsageRecordResult = UsageRecordResult'
  { status ::
      Lude.Maybe UsageRecordResultStatus,
    usageRecord :: Lude.Maybe UsageRecord,
    meteringRecordId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UsageRecordResult' with the minimum fields required to make a request.
--
-- * 'meteringRecordId' - The MeteringRecordId is a unique identifier for this metering event.
-- * 'status' - The UsageRecordResult Status indicates the status of an individual UsageRecord processed by BatchMeterUsage.
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
-- * 'usageRecord' - The UsageRecord that was part of the BatchMeterUsage request.
mkUsageRecordResult ::
  UsageRecordResult
mkUsageRecordResult =
  UsageRecordResult'
    { status = Lude.Nothing,
      usageRecord = Lude.Nothing,
      meteringRecordId = Lude.Nothing
    }

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
urrStatus :: Lens.Lens' UsageRecordResult (Lude.Maybe UsageRecordResultStatus)
urrStatus = Lens.lens (status :: UsageRecordResult -> Lude.Maybe UsageRecordResultStatus) (\s a -> s {status = a} :: UsageRecordResult)
{-# DEPRECATED urrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The UsageRecord that was part of the BatchMeterUsage request.
--
-- /Note:/ Consider using 'usageRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrUsageRecord :: Lens.Lens' UsageRecordResult (Lude.Maybe UsageRecord)
urrUsageRecord = Lens.lens (usageRecord :: UsageRecordResult -> Lude.Maybe UsageRecord) (\s a -> s {usageRecord = a} :: UsageRecordResult)
{-# DEPRECATED urrUsageRecord "Use generic-lens or generic-optics with 'usageRecord' instead." #-}

-- | The MeteringRecordId is a unique identifier for this metering event.
--
-- /Note:/ Consider using 'meteringRecordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrMeteringRecordId :: Lens.Lens' UsageRecordResult (Lude.Maybe Lude.Text)
urrMeteringRecordId = Lens.lens (meteringRecordId :: UsageRecordResult -> Lude.Maybe Lude.Text) (\s a -> s {meteringRecordId = a} :: UsageRecordResult)
{-# DEPRECATED urrMeteringRecordId "Use generic-lens or generic-optics with 'meteringRecordId' instead." #-}

instance Lude.FromJSON UsageRecordResult where
  parseJSON =
    Lude.withObject
      "UsageRecordResult"
      ( \x ->
          UsageRecordResult'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "UsageRecord")
            Lude.<*> (x Lude..:? "MeteringRecordId")
      )
