{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ProvisionedThroughputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ProvisionedThroughputDescription
  ( ProvisionedThroughputDescription (..)
  -- * Smart constructor
  , mkProvisionedThroughputDescription
  -- * Lenses
  , ptdLastDecreaseDateTime
  , ptdLastIncreaseDateTime
  , ptdNumberOfDecreasesToday
  , ptdReadCapacityUnits
  , ptdWriteCapacityUnits
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the provisioned throughput settings for the table, consisting of read and write capacity units, along with data about increases and decreases.
--
-- /See:/ 'mkProvisionedThroughputDescription' smart constructor.
data ProvisionedThroughputDescription = ProvisionedThroughputDescription'
  { lastDecreaseDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time of the last provisioned throughput decrease for this table.
  , lastIncreaseDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time of the last provisioned throughput increase for this table.
  , numberOfDecreasesToday :: Core.Maybe Core.Natural
    -- ^ The number of provisioned throughput decreases for this table during this UTC calendar day. For current maximums on provisioned throughput decreases, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
  , readCapacityUnits :: Core.Maybe Core.Natural
    -- ^ The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . Eventually consistent reads require less effort than strongly consistent reads, so a setting of 50 @ReadCapacityUnits@ per second provides 100 eventually consistent @ReadCapacityUnits@ per second.
  , writeCapacityUnits :: Core.Maybe Core.Natural
    -- ^ The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ProvisionedThroughputDescription' value with any optional fields omitted.
mkProvisionedThroughputDescription
    :: ProvisionedThroughputDescription
mkProvisionedThroughputDescription
  = ProvisionedThroughputDescription'{lastDecreaseDateTime =
                                        Core.Nothing,
                                      lastIncreaseDateTime = Core.Nothing,
                                      numberOfDecreasesToday = Core.Nothing,
                                      readCapacityUnits = Core.Nothing,
                                      writeCapacityUnits = Core.Nothing}

-- | The date and time of the last provisioned throughput decrease for this table.
--
-- /Note:/ Consider using 'lastDecreaseDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdLastDecreaseDateTime :: Lens.Lens' ProvisionedThroughputDescription (Core.Maybe Core.NominalDiffTime)
ptdLastDecreaseDateTime = Lens.field @"lastDecreaseDateTime"
{-# INLINEABLE ptdLastDecreaseDateTime #-}
{-# DEPRECATED lastDecreaseDateTime "Use generic-lens or generic-optics with 'lastDecreaseDateTime' instead"  #-}

-- | The date and time of the last provisioned throughput increase for this table.
--
-- /Note:/ Consider using 'lastIncreaseDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdLastIncreaseDateTime :: Lens.Lens' ProvisionedThroughputDescription (Core.Maybe Core.NominalDiffTime)
ptdLastIncreaseDateTime = Lens.field @"lastIncreaseDateTime"
{-# INLINEABLE ptdLastIncreaseDateTime #-}
{-# DEPRECATED lastIncreaseDateTime "Use generic-lens or generic-optics with 'lastIncreaseDateTime' instead"  #-}

-- | The number of provisioned throughput decreases for this table during this UTC calendar day. For current maximums on provisioned throughput decreases, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'numberOfDecreasesToday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdNumberOfDecreasesToday :: Lens.Lens' ProvisionedThroughputDescription (Core.Maybe Core.Natural)
ptdNumberOfDecreasesToday = Lens.field @"numberOfDecreasesToday"
{-# INLINEABLE ptdNumberOfDecreasesToday #-}
{-# DEPRECATED numberOfDecreasesToday "Use generic-lens or generic-optics with 'numberOfDecreasesToday' instead"  #-}

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . Eventually consistent reads require less effort than strongly consistent reads, so a setting of 50 @ReadCapacityUnits@ per second provides 100 eventually consistent @ReadCapacityUnits@ per second.
--
-- /Note:/ Consider using 'readCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdReadCapacityUnits :: Lens.Lens' ProvisionedThroughputDescription (Core.Maybe Core.Natural)
ptdReadCapacityUnits = Lens.field @"readCapacityUnits"
{-# INLINEABLE ptdReadCapacityUnits #-}
{-# DEPRECATED readCapacityUnits "Use generic-lens or generic-optics with 'readCapacityUnits' instead"  #-}

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- /Note:/ Consider using 'writeCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdWriteCapacityUnits :: Lens.Lens' ProvisionedThroughputDescription (Core.Maybe Core.Natural)
ptdWriteCapacityUnits = Lens.field @"writeCapacityUnits"
{-# INLINEABLE ptdWriteCapacityUnits #-}
{-# DEPRECATED writeCapacityUnits "Use generic-lens or generic-optics with 'writeCapacityUnits' instead"  #-}

instance Core.FromJSON ProvisionedThroughputDescription where
        parseJSON
          = Core.withObject "ProvisionedThroughputDescription" Core.$
              \ x ->
                ProvisionedThroughputDescription' Core.<$>
                  (x Core..:? "LastDecreaseDateTime") Core.<*>
                    x Core..:? "LastIncreaseDateTime"
                    Core.<*> x Core..:? "NumberOfDecreasesToday"
                    Core.<*> x Core..:? "ReadCapacityUnits"
                    Core.<*> x Core..:? "WriteCapacityUnits"
