{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.TelemetryRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.TelemetryRecord
  ( TelemetryRecord (..),

    -- * Smart constructor
    mkTelemetryRecord,

    -- * Lenses
    trTimestamp,
    trBackendConnectionErrors,
    trSegmentsReceivedCount,
    trSegmentsRejectedCount,
    trSegmentsSentCount,
    trSegmentsSpilloverCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.BackendConnectionErrors as Types

-- |
--
-- /See:/ 'mkTelemetryRecord' smart constructor.
data TelemetryRecord = TelemetryRecord'
  { -- |
    timestamp :: Core.NominalDiffTime,
    -- |
    backendConnectionErrors :: Core.Maybe Types.BackendConnectionErrors,
    -- |
    segmentsReceivedCount :: Core.Maybe Core.Int,
    -- |
    segmentsRejectedCount :: Core.Maybe Core.Int,
    -- |
    segmentsSentCount :: Core.Maybe Core.Int,
    -- |
    segmentsSpilloverCount :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TelemetryRecord' value with any optional fields omitted.
mkTelemetryRecord ::
  -- | 'timestamp'
  Core.NominalDiffTime ->
  TelemetryRecord
mkTelemetryRecord timestamp =
  TelemetryRecord'
    { timestamp,
      backendConnectionErrors = Core.Nothing,
      segmentsReceivedCount = Core.Nothing,
      segmentsRejectedCount = Core.Nothing,
      segmentsSentCount = Core.Nothing,
      segmentsSpilloverCount = Core.Nothing
    }

-- |
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTimestamp :: Lens.Lens' TelemetryRecord Core.NominalDiffTime
trTimestamp = Lens.field @"timestamp"
{-# DEPRECATED trTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- |
--
-- /Note:/ Consider using 'backendConnectionErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trBackendConnectionErrors :: Lens.Lens' TelemetryRecord (Core.Maybe Types.BackendConnectionErrors)
trBackendConnectionErrors = Lens.field @"backendConnectionErrors"
{-# DEPRECATED trBackendConnectionErrors "Use generic-lens or generic-optics with 'backendConnectionErrors' instead." #-}

-- |
--
-- /Note:/ Consider using 'segmentsReceivedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trSegmentsReceivedCount :: Lens.Lens' TelemetryRecord (Core.Maybe Core.Int)
trSegmentsReceivedCount = Lens.field @"segmentsReceivedCount"
{-# DEPRECATED trSegmentsReceivedCount "Use generic-lens or generic-optics with 'segmentsReceivedCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'segmentsRejectedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trSegmentsRejectedCount :: Lens.Lens' TelemetryRecord (Core.Maybe Core.Int)
trSegmentsRejectedCount = Lens.field @"segmentsRejectedCount"
{-# DEPRECATED trSegmentsRejectedCount "Use generic-lens or generic-optics with 'segmentsRejectedCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'segmentsSentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trSegmentsSentCount :: Lens.Lens' TelemetryRecord (Core.Maybe Core.Int)
trSegmentsSentCount = Lens.field @"segmentsSentCount"
{-# DEPRECATED trSegmentsSentCount "Use generic-lens or generic-optics with 'segmentsSentCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'segmentsSpilloverCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trSegmentsSpilloverCount :: Lens.Lens' TelemetryRecord (Core.Maybe Core.Int)
trSegmentsSpilloverCount = Lens.field @"segmentsSpilloverCount"
{-# DEPRECATED trSegmentsSpilloverCount "Use generic-lens or generic-optics with 'segmentsSpilloverCount' instead." #-}

instance Core.FromJSON TelemetryRecord where
  toJSON TelemetryRecord {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Timestamp" Core..= timestamp),
            ("BackendConnectionErrors" Core..=)
              Core.<$> backendConnectionErrors,
            ("SegmentsReceivedCount" Core..=) Core.<$> segmentsReceivedCount,
            ("SegmentsRejectedCount" Core..=) Core.<$> segmentsRejectedCount,
            ("SegmentsSentCount" Core..=) Core.<$> segmentsSentCount,
            ("SegmentsSpilloverCount" Core..=)
              Core.<$> segmentsSpilloverCount
          ]
      )
