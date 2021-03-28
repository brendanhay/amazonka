{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TimestreamTimestamp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.TimestreamTimestamp
  ( TimestreamTimestamp (..)
  -- * Smart constructor
  , mkTimestreamTimestamp
  -- * Lenses
  , ttValue
  , ttUnit
  ) where

import qualified Network.AWS.IoT.Types.TimestreamTimestampValue as Types
import qualified Network.AWS.IoT.Types.Unit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes how to interpret an application-defined timestamp value from an MQTT message payload and the precision of that value.
--
-- /See:/ 'mkTimestreamTimestamp' smart constructor.
data TimestreamTimestamp = TimestreamTimestamp'
  { value :: Types.TimestreamTimestampValue
    -- ^ An expression that returns a long epoch time value.
  , unit :: Types.Unit
    -- ^ The precision of the timestamp value that results from the expression described in @value@ .
--
-- Valid values: @SECONDS@ | @MILLISECONDS@ | @MICROSECONDS@ | @NANOSECONDS@ . The default is @MILLISECONDS@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TimestreamTimestamp' value with any optional fields omitted.
mkTimestreamTimestamp
    :: Types.TimestreamTimestampValue -- ^ 'value'
    -> Types.Unit -- ^ 'unit'
    -> TimestreamTimestamp
mkTimestreamTimestamp value unit
  = TimestreamTimestamp'{value, unit}

-- | An expression that returns a long epoch time value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttValue :: Lens.Lens' TimestreamTimestamp Types.TimestreamTimestampValue
ttValue = Lens.field @"value"
{-# INLINEABLE ttValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

-- | The precision of the timestamp value that results from the expression described in @value@ .
--
-- Valid values: @SECONDS@ | @MILLISECONDS@ | @MICROSECONDS@ | @NANOSECONDS@ . The default is @MILLISECONDS@ .
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttUnit :: Lens.Lens' TimestreamTimestamp Types.Unit
ttUnit = Lens.field @"unit"
{-# INLINEABLE ttUnit #-}
{-# DEPRECATED unit "Use generic-lens or generic-optics with 'unit' instead"  #-}

instance Core.FromJSON TimestreamTimestamp where
        toJSON TimestreamTimestamp{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("value" Core..= value),
                  Core.Just ("unit" Core..= unit)])

instance Core.FromJSON TimestreamTimestamp where
        parseJSON
          = Core.withObject "TimestreamTimestamp" Core.$
              \ x ->
                TimestreamTimestamp' Core.<$>
                  (x Core..: "value") Core.<*> x Core..: "unit"
