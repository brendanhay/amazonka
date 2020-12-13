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
    trSegmentsReceivedCount,
    trSegmentsSentCount,
    trSegmentsSpilloverCount,
    trSegmentsRejectedCount,
    trTimestamp,
    trBackendConnectionErrors,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.BackendConnectionErrors

-- |
--
-- /See:/ 'mkTelemetryRecord' smart constructor.
data TelemetryRecord = TelemetryRecord'
  { -- |
    segmentsReceivedCount :: Lude.Maybe Lude.Int,
    -- |
    segmentsSentCount :: Lude.Maybe Lude.Int,
    -- |
    segmentsSpilloverCount :: Lude.Maybe Lude.Int,
    -- |
    segmentsRejectedCount :: Lude.Maybe Lude.Int,
    -- |
    timestamp :: Lude.Timestamp,
    -- |
    backendConnectionErrors :: Lude.Maybe BackendConnectionErrors
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TelemetryRecord' with the minimum fields required to make a request.
--
-- * 'segmentsReceivedCount' -
-- * 'segmentsSentCount' -
-- * 'segmentsSpilloverCount' -
-- * 'segmentsRejectedCount' -
-- * 'timestamp' -
-- * 'backendConnectionErrors' -
mkTelemetryRecord ::
  -- | 'timestamp'
  Lude.Timestamp ->
  TelemetryRecord
mkTelemetryRecord pTimestamp_ =
  TelemetryRecord'
    { segmentsReceivedCount = Lude.Nothing,
      segmentsSentCount = Lude.Nothing,
      segmentsSpilloverCount = Lude.Nothing,
      segmentsRejectedCount = Lude.Nothing,
      timestamp = pTimestamp_,
      backendConnectionErrors = Lude.Nothing
    }

-- |
--
-- /Note:/ Consider using 'segmentsReceivedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trSegmentsReceivedCount :: Lens.Lens' TelemetryRecord (Lude.Maybe Lude.Int)
trSegmentsReceivedCount = Lens.lens (segmentsReceivedCount :: TelemetryRecord -> Lude.Maybe Lude.Int) (\s a -> s {segmentsReceivedCount = a} :: TelemetryRecord)
{-# DEPRECATED trSegmentsReceivedCount "Use generic-lens or generic-optics with 'segmentsReceivedCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'segmentsSentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trSegmentsSentCount :: Lens.Lens' TelemetryRecord (Lude.Maybe Lude.Int)
trSegmentsSentCount = Lens.lens (segmentsSentCount :: TelemetryRecord -> Lude.Maybe Lude.Int) (\s a -> s {segmentsSentCount = a} :: TelemetryRecord)
{-# DEPRECATED trSegmentsSentCount "Use generic-lens or generic-optics with 'segmentsSentCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'segmentsSpilloverCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trSegmentsSpilloverCount :: Lens.Lens' TelemetryRecord (Lude.Maybe Lude.Int)
trSegmentsSpilloverCount = Lens.lens (segmentsSpilloverCount :: TelemetryRecord -> Lude.Maybe Lude.Int) (\s a -> s {segmentsSpilloverCount = a} :: TelemetryRecord)
{-# DEPRECATED trSegmentsSpilloverCount "Use generic-lens or generic-optics with 'segmentsSpilloverCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'segmentsRejectedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trSegmentsRejectedCount :: Lens.Lens' TelemetryRecord (Lude.Maybe Lude.Int)
trSegmentsRejectedCount = Lens.lens (segmentsRejectedCount :: TelemetryRecord -> Lude.Maybe Lude.Int) (\s a -> s {segmentsRejectedCount = a} :: TelemetryRecord)
{-# DEPRECATED trSegmentsRejectedCount "Use generic-lens or generic-optics with 'segmentsRejectedCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTimestamp :: Lens.Lens' TelemetryRecord Lude.Timestamp
trTimestamp = Lens.lens (timestamp :: TelemetryRecord -> Lude.Timestamp) (\s a -> s {timestamp = a} :: TelemetryRecord)
{-# DEPRECATED trTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- |
--
-- /Note:/ Consider using 'backendConnectionErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trBackendConnectionErrors :: Lens.Lens' TelemetryRecord (Lude.Maybe BackendConnectionErrors)
trBackendConnectionErrors = Lens.lens (backendConnectionErrors :: TelemetryRecord -> Lude.Maybe BackendConnectionErrors) (\s a -> s {backendConnectionErrors = a} :: TelemetryRecord)
{-# DEPRECATED trBackendConnectionErrors "Use generic-lens or generic-optics with 'backendConnectionErrors' instead." #-}

instance Lude.ToJSON TelemetryRecord where
  toJSON TelemetryRecord' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SegmentsReceivedCount" Lude..=) Lude.<$> segmentsReceivedCount,
            ("SegmentsSentCount" Lude..=) Lude.<$> segmentsSentCount,
            ("SegmentsSpilloverCount" Lude..=) Lude.<$> segmentsSpilloverCount,
            ("SegmentsRejectedCount" Lude..=) Lude.<$> segmentsRejectedCount,
            Lude.Just ("Timestamp" Lude..= timestamp),
            ("BackendConnectionErrors" Lude..=)
              Lude.<$> backendConnectionErrors
          ]
      )
