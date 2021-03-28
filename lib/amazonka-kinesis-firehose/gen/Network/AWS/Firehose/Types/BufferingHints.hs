{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.BufferingHints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.BufferingHints
  ( BufferingHints (..)
  -- * Smart constructor
  , mkBufferingHints
  -- * Lenses
  , bhIntervalInSeconds
  , bhSizeInMBs
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes hints for the buffering to perform before delivering data to the destination. These options are treated as hints, and therefore Kinesis Data Firehose might choose to use different values when it is optimal. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other.
--
-- /See:/ 'mkBufferingHints' smart constructor.
data BufferingHints = BufferingHints'
  { intervalInSeconds :: Core.Maybe Core.Natural
    -- ^ Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300. This parameter is optional but if you specify a value for it, you must also specify a value for @SizeInMBs@ , and vice versa.
  , sizeInMBs :: Core.Maybe Core.Natural
    -- ^ Buffer incoming data to the specified size, in MiBs, before delivering it to the destination. The default value is 5. This parameter is optional but if you specify a value for it, you must also specify a value for @IntervalInSeconds@ , and vice versa.
--
-- We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MiB/sec, the value should be 10 MiB or higher.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BufferingHints' value with any optional fields omitted.
mkBufferingHints
    :: BufferingHints
mkBufferingHints
  = BufferingHints'{intervalInSeconds = Core.Nothing,
                    sizeInMBs = Core.Nothing}

-- | Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300. This parameter is optional but if you specify a value for it, you must also specify a value for @SizeInMBs@ , and vice versa.
--
-- /Note:/ Consider using 'intervalInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bhIntervalInSeconds :: Lens.Lens' BufferingHints (Core.Maybe Core.Natural)
bhIntervalInSeconds = Lens.field @"intervalInSeconds"
{-# INLINEABLE bhIntervalInSeconds #-}
{-# DEPRECATED intervalInSeconds "Use generic-lens or generic-optics with 'intervalInSeconds' instead"  #-}

-- | Buffer incoming data to the specified size, in MiBs, before delivering it to the destination. The default value is 5. This parameter is optional but if you specify a value for it, you must also specify a value for @IntervalInSeconds@ , and vice versa.
--
-- We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MiB/sec, the value should be 10 MiB or higher.
--
-- /Note:/ Consider using 'sizeInMBs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bhSizeInMBs :: Lens.Lens' BufferingHints (Core.Maybe Core.Natural)
bhSizeInMBs = Lens.field @"sizeInMBs"
{-# INLINEABLE bhSizeInMBs #-}
{-# DEPRECATED sizeInMBs "Use generic-lens or generic-optics with 'sizeInMBs' instead"  #-}

instance Core.FromJSON BufferingHints where
        toJSON BufferingHints{..}
          = Core.object
              (Core.catMaybes
                 [("IntervalInSeconds" Core..=) Core.<$> intervalInSeconds,
                  ("SizeInMBs" Core..=) Core.<$> sizeInMBs])

instance Core.FromJSON BufferingHints where
        parseJSON
          = Core.withObject "BufferingHints" Core.$
              \ x ->
                BufferingHints' Core.<$>
                  (x Core..:? "IntervalInSeconds") Core.<*> x Core..:? "SizeInMBs"
