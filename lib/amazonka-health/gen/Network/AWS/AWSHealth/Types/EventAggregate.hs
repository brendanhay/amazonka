{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventAggregate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AWSHealth.Types.EventAggregate
  ( EventAggregate (..)
  -- * Smart constructor
  , mkEventAggregate
  -- * Lenses
  , eaAggregateValue
  , eaCount
  ) where

import qualified Network.AWS.AWSHealth.Types.AggregateValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The number of events of each issue type. Returned by the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventAggregates.html DescribeEventAggregates> operation.
--
-- /See:/ 'mkEventAggregate' smart constructor.
data EventAggregate = EventAggregate'
  { aggregateValue :: Core.Maybe Types.AggregateValue
    -- ^ The issue type for the associated count.
  , count :: Core.Maybe Core.Int
    -- ^ The number of events of the associated issue type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventAggregate' value with any optional fields omitted.
mkEventAggregate
    :: EventAggregate
mkEventAggregate
  = EventAggregate'{aggregateValue = Core.Nothing,
                    count = Core.Nothing}

-- | The issue type for the associated count.
--
-- /Note:/ Consider using 'aggregateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaAggregateValue :: Lens.Lens' EventAggregate (Core.Maybe Types.AggregateValue)
eaAggregateValue = Lens.field @"aggregateValue"
{-# INLINEABLE eaAggregateValue #-}
{-# DEPRECATED aggregateValue "Use generic-lens or generic-optics with 'aggregateValue' instead"  #-}

-- | The number of events of the associated issue type.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaCount :: Lens.Lens' EventAggregate (Core.Maybe Core.Int)
eaCount = Lens.field @"count"
{-# INLINEABLE eaCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

instance Core.FromJSON EventAggregate where
        parseJSON
          = Core.withObject "EventAggregate" Core.$
              \ x ->
                EventAggregate' Core.<$>
                  (x Core..:? "aggregateValue") Core.<*> x Core..:? "count"
