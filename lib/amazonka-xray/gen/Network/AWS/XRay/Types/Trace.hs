{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Trace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Trace
  ( Trace (..),

    -- * Smart constructor
    mkTrace,

    -- * Lenses
    tDuration,
    tId,
    tLimitExceeded,
    tSegments,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.Segment as Types
import qualified Network.AWS.XRay.Types.TraceId as Types

-- | A collection of segment documents with matching trace IDs.
--
-- /See:/ 'mkTrace' smart constructor.
data Trace = Trace'
  { -- | The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
    duration :: Core.Maybe Core.Double,
    -- | The unique identifier for the request that generated the trace's segments and subsegments.
    id :: Core.Maybe Types.TraceId,
    -- | LimitExceeded is set to true when the trace has exceeded one of the defined quotas. For more information about quotas, see <https://docs.aws.amazon.com/general/latest/gr/xray.html AWS X-Ray endpoints and quotas> .
    limitExceeded :: Core.Maybe Core.Bool,
    -- | Segment documents for the segments and subsegments that comprise the trace.
    segments :: Core.Maybe [Types.Segment]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Trace' value with any optional fields omitted.
mkTrace ::
  Trace
mkTrace =
  Trace'
    { duration = Core.Nothing,
      id = Core.Nothing,
      limitExceeded = Core.Nothing,
      segments = Core.Nothing
    }

-- | The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDuration :: Lens.Lens' Trace (Core.Maybe Core.Double)
tDuration = Lens.field @"duration"
{-# DEPRECATED tDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The unique identifier for the request that generated the trace's segments and subsegments.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tId :: Lens.Lens' Trace (Core.Maybe Types.TraceId)
tId = Lens.field @"id"
{-# DEPRECATED tId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | LimitExceeded is set to true when the trace has exceeded one of the defined quotas. For more information about quotas, see <https://docs.aws.amazon.com/general/latest/gr/xray.html AWS X-Ray endpoints and quotas> .
--
-- /Note:/ Consider using 'limitExceeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLimitExceeded :: Lens.Lens' Trace (Core.Maybe Core.Bool)
tLimitExceeded = Lens.field @"limitExceeded"
{-# DEPRECATED tLimitExceeded "Use generic-lens or generic-optics with 'limitExceeded' instead." #-}

-- | Segment documents for the segments and subsegments that comprise the trace.
--
-- /Note:/ Consider using 'segments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSegments :: Lens.Lens' Trace (Core.Maybe [Types.Segment])
tSegments = Lens.field @"segments"
{-# DEPRECATED tSegments "Use generic-lens or generic-optics with 'segments' instead." #-}

instance Core.FromJSON Trace where
  parseJSON =
    Core.withObject "Trace" Core.$
      \x ->
        Trace'
          Core.<$> (x Core..:? "Duration")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "LimitExceeded")
          Core.<*> (x Core..:? "Segments")
