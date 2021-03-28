{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchBufferingHints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.ElasticsearchBufferingHints
  ( ElasticsearchBufferingHints (..)
  -- * Smart constructor
  , mkElasticsearchBufferingHints
  -- * Lenses
  , ebhIntervalInSeconds
  , ebhSizeInMBs
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the buffering to perform before delivering data to the Amazon ES destination.
--
-- /See:/ 'mkElasticsearchBufferingHints' smart constructor.
data ElasticsearchBufferingHints = ElasticsearchBufferingHints'
  { intervalInSeconds :: Core.Maybe Core.Natural
    -- ^ Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes).
  , sizeInMBs :: Core.Maybe Core.Natural
    -- ^ Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5.
--
-- We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticsearchBufferingHints' value with any optional fields omitted.
mkElasticsearchBufferingHints
    :: ElasticsearchBufferingHints
mkElasticsearchBufferingHints
  = ElasticsearchBufferingHints'{intervalInSeconds = Core.Nothing,
                                 sizeInMBs = Core.Nothing}

-- | Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes).
--
-- /Note:/ Consider using 'intervalInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebhIntervalInSeconds :: Lens.Lens' ElasticsearchBufferingHints (Core.Maybe Core.Natural)
ebhIntervalInSeconds = Lens.field @"intervalInSeconds"
{-# INLINEABLE ebhIntervalInSeconds #-}
{-# DEPRECATED intervalInSeconds "Use generic-lens or generic-optics with 'intervalInSeconds' instead"  #-}

-- | Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5.
--
-- We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher.
--
-- /Note:/ Consider using 'sizeInMBs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebhSizeInMBs :: Lens.Lens' ElasticsearchBufferingHints (Core.Maybe Core.Natural)
ebhSizeInMBs = Lens.field @"sizeInMBs"
{-# INLINEABLE ebhSizeInMBs #-}
{-# DEPRECATED sizeInMBs "Use generic-lens or generic-optics with 'sizeInMBs' instead"  #-}

instance Core.FromJSON ElasticsearchBufferingHints where
        toJSON ElasticsearchBufferingHints{..}
          = Core.object
              (Core.catMaybes
                 [("IntervalInSeconds" Core..=) Core.<$> intervalInSeconds,
                  ("SizeInMBs" Core..=) Core.<$> sizeInMBs])

instance Core.FromJSON ElasticsearchBufferingHints where
        parseJSON
          = Core.withObject "ElasticsearchBufferingHints" Core.$
              \ x ->
                ElasticsearchBufferingHints' Core.<$>
                  (x Core..:? "IntervalInSeconds") Core.<*> x Core..:? "SizeInMBs"
