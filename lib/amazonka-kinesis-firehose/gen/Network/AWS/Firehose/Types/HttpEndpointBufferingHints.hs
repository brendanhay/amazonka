{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HttpEndpointBufferingHints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.HttpEndpointBufferingHints
  ( HttpEndpointBufferingHints (..)
  -- * Smart constructor
  , mkHttpEndpointBufferingHints
  -- * Lenses
  , hebhIntervalInSeconds
  , hebhSizeInMBs
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the buffering options that can be applied before data is delivered to the HTTP endpoint destination. Kinesis Data Firehose treats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other. 
--
-- /See:/ 'mkHttpEndpointBufferingHints' smart constructor.
data HttpEndpointBufferingHints = HttpEndpointBufferingHints'
  { intervalInSeconds :: Core.Maybe Core.Natural
    -- ^ Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes). 
  , sizeInMBs :: Core.Maybe Core.Natural
    -- ^ Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5. 
--
-- We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpEndpointBufferingHints' value with any optional fields omitted.
mkHttpEndpointBufferingHints
    :: HttpEndpointBufferingHints
mkHttpEndpointBufferingHints
  = HttpEndpointBufferingHints'{intervalInSeconds = Core.Nothing,
                                sizeInMBs = Core.Nothing}

-- | Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes). 
--
-- /Note:/ Consider using 'intervalInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hebhIntervalInSeconds :: Lens.Lens' HttpEndpointBufferingHints (Core.Maybe Core.Natural)
hebhIntervalInSeconds = Lens.field @"intervalInSeconds"
{-# INLINEABLE hebhIntervalInSeconds #-}
{-# DEPRECATED intervalInSeconds "Use generic-lens or generic-optics with 'intervalInSeconds' instead"  #-}

-- | Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5. 
--
-- We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher. 
--
-- /Note:/ Consider using 'sizeInMBs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hebhSizeInMBs :: Lens.Lens' HttpEndpointBufferingHints (Core.Maybe Core.Natural)
hebhSizeInMBs = Lens.field @"sizeInMBs"
{-# INLINEABLE hebhSizeInMBs #-}
{-# DEPRECATED sizeInMBs "Use generic-lens or generic-optics with 'sizeInMBs' instead"  #-}

instance Core.FromJSON HttpEndpointBufferingHints where
        toJSON HttpEndpointBufferingHints{..}
          = Core.object
              (Core.catMaybes
                 [("IntervalInSeconds" Core..=) Core.<$> intervalInSeconds,
                  ("SizeInMBs" Core..=) Core.<$> sizeInMBs])

instance Core.FromJSON HttpEndpointBufferingHints where
        parseJSON
          = Core.withObject "HttpEndpointBufferingHints" Core.$
              \ x ->
                HttpEndpointBufferingHints' Core.<$>
                  (x Core..:? "IntervalInSeconds") Core.<*> x Core..:? "SizeInMBs"
