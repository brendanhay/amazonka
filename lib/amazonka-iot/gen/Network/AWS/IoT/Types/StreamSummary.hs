{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StreamSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.StreamSummary
  ( StreamSummary (..)
  -- * Smart constructor
  , mkStreamSummary
  -- * Lenses
  , ssDescription
  , ssStreamArn
  , ssStreamId
  , ssStreamVersion
  ) where

import qualified Network.AWS.IoT.Types.StreamArn as Types
import qualified Network.AWS.IoT.Types.StreamDescription as Types
import qualified Network.AWS.IoT.Types.StreamId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A summary of a stream.
--
-- /See:/ 'mkStreamSummary' smart constructor.
data StreamSummary = StreamSummary'
  { description :: Core.Maybe Types.StreamDescription
    -- ^ A description of the stream.
  , streamArn :: Core.Maybe Types.StreamArn
    -- ^ The stream ARN.
  , streamId :: Core.Maybe Types.StreamId
    -- ^ The stream ID.
  , streamVersion :: Core.Maybe Core.Natural
    -- ^ The stream version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StreamSummary' value with any optional fields omitted.
mkStreamSummary
    :: StreamSummary
mkStreamSummary
  = StreamSummary'{description = Core.Nothing,
                   streamArn = Core.Nothing, streamId = Core.Nothing,
                   streamVersion = Core.Nothing}

-- | A description of the stream.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDescription :: Lens.Lens' StreamSummary (Core.Maybe Types.StreamDescription)
ssDescription = Lens.field @"description"
{-# INLINEABLE ssDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The stream ARN.
--
-- /Note:/ Consider using 'streamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStreamArn :: Lens.Lens' StreamSummary (Core.Maybe Types.StreamArn)
ssStreamArn = Lens.field @"streamArn"
{-# INLINEABLE ssStreamArn #-}
{-# DEPRECATED streamArn "Use generic-lens or generic-optics with 'streamArn' instead"  #-}

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStreamId :: Lens.Lens' StreamSummary (Core.Maybe Types.StreamId)
ssStreamId = Lens.field @"streamId"
{-# INLINEABLE ssStreamId #-}
{-# DEPRECATED streamId "Use generic-lens or generic-optics with 'streamId' instead"  #-}

-- | The stream version.
--
-- /Note:/ Consider using 'streamVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStreamVersion :: Lens.Lens' StreamSummary (Core.Maybe Core.Natural)
ssStreamVersion = Lens.field @"streamVersion"
{-# INLINEABLE ssStreamVersion #-}
{-# DEPRECATED streamVersion "Use generic-lens or generic-optics with 'streamVersion' instead"  #-}

instance Core.FromJSON StreamSummary where
        parseJSON
          = Core.withObject "StreamSummary" Core.$
              \ x ->
                StreamSummary' Core.<$>
                  (x Core..:? "description") Core.<*> x Core..:? "streamArn" Core.<*>
                    x Core..:? "streamId"
                    Core.<*> x Core..:? "streamVersion"
