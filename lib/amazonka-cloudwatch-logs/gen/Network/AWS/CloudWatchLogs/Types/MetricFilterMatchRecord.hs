{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.MetricFilterMatchRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.MetricFilterMatchRecord
  ( MetricFilterMatchRecord (..)
  -- * Smart constructor
  , mkMetricFilterMatchRecord
  -- * Lenses
  , mfmrEventMessage
  , mfmrEventNumber
  , mfmrExtractedValues
  ) where

import qualified Network.AWS.CloudWatchLogs.Types.EventMessage as Types
import qualified Network.AWS.CloudWatchLogs.Types.Token as Types
import qualified Network.AWS.CloudWatchLogs.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a matched event.
--
-- /See:/ 'mkMetricFilterMatchRecord' smart constructor.
data MetricFilterMatchRecord = MetricFilterMatchRecord'
  { eventMessage :: Core.Maybe Types.EventMessage
    -- ^ The raw event data.
  , eventNumber :: Core.Maybe Core.Integer
    -- ^ The event number.
  , extractedValues :: Core.Maybe (Core.HashMap Types.Token Types.Value)
    -- ^ The values extracted from the event data by the filter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricFilterMatchRecord' value with any optional fields omitted.
mkMetricFilterMatchRecord
    :: MetricFilterMatchRecord
mkMetricFilterMatchRecord
  = MetricFilterMatchRecord'{eventMessage = Core.Nothing,
                             eventNumber = Core.Nothing, extractedValues = Core.Nothing}

-- | The raw event data.
--
-- /Note:/ Consider using 'eventMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfmrEventMessage :: Lens.Lens' MetricFilterMatchRecord (Core.Maybe Types.EventMessage)
mfmrEventMessage = Lens.field @"eventMessage"
{-# INLINEABLE mfmrEventMessage #-}
{-# DEPRECATED eventMessage "Use generic-lens or generic-optics with 'eventMessage' instead"  #-}

-- | The event number.
--
-- /Note:/ Consider using 'eventNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfmrEventNumber :: Lens.Lens' MetricFilterMatchRecord (Core.Maybe Core.Integer)
mfmrEventNumber = Lens.field @"eventNumber"
{-# INLINEABLE mfmrEventNumber #-}
{-# DEPRECATED eventNumber "Use generic-lens or generic-optics with 'eventNumber' instead"  #-}

-- | The values extracted from the event data by the filter.
--
-- /Note:/ Consider using 'extractedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfmrExtractedValues :: Lens.Lens' MetricFilterMatchRecord (Core.Maybe (Core.HashMap Types.Token Types.Value))
mfmrExtractedValues = Lens.field @"extractedValues"
{-# INLINEABLE mfmrExtractedValues #-}
{-# DEPRECATED extractedValues "Use generic-lens or generic-optics with 'extractedValues' instead"  #-}

instance Core.FromJSON MetricFilterMatchRecord where
        parseJSON
          = Core.withObject "MetricFilterMatchRecord" Core.$
              \ x ->
                MetricFilterMatchRecord' Core.<$>
                  (x Core..:? "eventMessage") Core.<*> x Core..:? "eventNumber"
                    Core.<*> x Core..:? "extractedValues"
