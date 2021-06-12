{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.TelemetryRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.TelemetryRecord where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.BackendConnectionErrors

-- |
--
-- /See:/ 'newTelemetryRecord' smart constructor.
data TelemetryRecord = TelemetryRecord'
  { segmentsSpilloverCount :: Core.Maybe Core.Int,
    backendConnectionErrors :: Core.Maybe BackendConnectionErrors,
    segmentsRejectedCount :: Core.Maybe Core.Int,
    segmentsSentCount :: Core.Maybe Core.Int,
    segmentsReceivedCount :: Core.Maybe Core.Int,
    timestamp :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TelemetryRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentsSpilloverCount', 'telemetryRecord_segmentsSpilloverCount' -
--
-- 'backendConnectionErrors', 'telemetryRecord_backendConnectionErrors' -
--
-- 'segmentsRejectedCount', 'telemetryRecord_segmentsRejectedCount' -
--
-- 'segmentsSentCount', 'telemetryRecord_segmentsSentCount' -
--
-- 'segmentsReceivedCount', 'telemetryRecord_segmentsReceivedCount' -
--
-- 'timestamp', 'telemetryRecord_timestamp' -
newTelemetryRecord ::
  -- | 'timestamp'
  Core.UTCTime ->
  TelemetryRecord
newTelemetryRecord pTimestamp_ =
  TelemetryRecord'
    { segmentsSpilloverCount =
        Core.Nothing,
      backendConnectionErrors = Core.Nothing,
      segmentsRejectedCount = Core.Nothing,
      segmentsSentCount = Core.Nothing,
      segmentsReceivedCount = Core.Nothing,
      timestamp = Core._Time Lens.# pTimestamp_
    }

-- |
telemetryRecord_segmentsSpilloverCount :: Lens.Lens' TelemetryRecord (Core.Maybe Core.Int)
telemetryRecord_segmentsSpilloverCount = Lens.lens (\TelemetryRecord' {segmentsSpilloverCount} -> segmentsSpilloverCount) (\s@TelemetryRecord' {} a -> s {segmentsSpilloverCount = a} :: TelemetryRecord)

-- |
telemetryRecord_backendConnectionErrors :: Lens.Lens' TelemetryRecord (Core.Maybe BackendConnectionErrors)
telemetryRecord_backendConnectionErrors = Lens.lens (\TelemetryRecord' {backendConnectionErrors} -> backendConnectionErrors) (\s@TelemetryRecord' {} a -> s {backendConnectionErrors = a} :: TelemetryRecord)

-- |
telemetryRecord_segmentsRejectedCount :: Lens.Lens' TelemetryRecord (Core.Maybe Core.Int)
telemetryRecord_segmentsRejectedCount = Lens.lens (\TelemetryRecord' {segmentsRejectedCount} -> segmentsRejectedCount) (\s@TelemetryRecord' {} a -> s {segmentsRejectedCount = a} :: TelemetryRecord)

-- |
telemetryRecord_segmentsSentCount :: Lens.Lens' TelemetryRecord (Core.Maybe Core.Int)
telemetryRecord_segmentsSentCount = Lens.lens (\TelemetryRecord' {segmentsSentCount} -> segmentsSentCount) (\s@TelemetryRecord' {} a -> s {segmentsSentCount = a} :: TelemetryRecord)

-- |
telemetryRecord_segmentsReceivedCount :: Lens.Lens' TelemetryRecord (Core.Maybe Core.Int)
telemetryRecord_segmentsReceivedCount = Lens.lens (\TelemetryRecord' {segmentsReceivedCount} -> segmentsReceivedCount) (\s@TelemetryRecord' {} a -> s {segmentsReceivedCount = a} :: TelemetryRecord)

-- |
telemetryRecord_timestamp :: Lens.Lens' TelemetryRecord Core.UTCTime
telemetryRecord_timestamp = Lens.lens (\TelemetryRecord' {timestamp} -> timestamp) (\s@TelemetryRecord' {} a -> s {timestamp = a} :: TelemetryRecord) Core.. Core._Time

instance Core.Hashable TelemetryRecord

instance Core.NFData TelemetryRecord

instance Core.ToJSON TelemetryRecord where
  toJSON TelemetryRecord' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SegmentsSpilloverCount" Core..=)
              Core.<$> segmentsSpilloverCount,
            ("BackendConnectionErrors" Core..=)
              Core.<$> backendConnectionErrors,
            ("SegmentsRejectedCount" Core..=)
              Core.<$> segmentsRejectedCount,
            ("SegmentsSentCount" Core..=)
              Core.<$> segmentsSentCount,
            ("SegmentsReceivedCount" Core..=)
              Core.<$> segmentsReceivedCount,
            Core.Just ("Timestamp" Core..= timestamp)
          ]
      )
