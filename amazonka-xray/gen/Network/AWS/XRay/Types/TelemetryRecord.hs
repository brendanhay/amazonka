{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.XRay.Types.BackendConnectionErrors

-- |
--
-- /See:/ 'newTelemetryRecord' smart constructor.
data TelemetryRecord = TelemetryRecord'
  { segmentsSpilloverCount :: Prelude.Maybe Prelude.Int,
    backendConnectionErrors :: Prelude.Maybe BackendConnectionErrors,
    segmentsRejectedCount :: Prelude.Maybe Prelude.Int,
    segmentsSentCount :: Prelude.Maybe Prelude.Int,
    segmentsReceivedCount :: Prelude.Maybe Prelude.Int,
    timestamp :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.UTCTime ->
  TelemetryRecord
newTelemetryRecord pTimestamp_ =
  TelemetryRecord'
    { segmentsSpilloverCount =
        Prelude.Nothing,
      backendConnectionErrors = Prelude.Nothing,
      segmentsRejectedCount = Prelude.Nothing,
      segmentsSentCount = Prelude.Nothing,
      segmentsReceivedCount = Prelude.Nothing,
      timestamp = Prelude._Time Lens.# pTimestamp_
    }

-- |
telemetryRecord_segmentsSpilloverCount :: Lens.Lens' TelemetryRecord (Prelude.Maybe Prelude.Int)
telemetryRecord_segmentsSpilloverCount = Lens.lens (\TelemetryRecord' {segmentsSpilloverCount} -> segmentsSpilloverCount) (\s@TelemetryRecord' {} a -> s {segmentsSpilloverCount = a} :: TelemetryRecord)

-- |
telemetryRecord_backendConnectionErrors :: Lens.Lens' TelemetryRecord (Prelude.Maybe BackendConnectionErrors)
telemetryRecord_backendConnectionErrors = Lens.lens (\TelemetryRecord' {backendConnectionErrors} -> backendConnectionErrors) (\s@TelemetryRecord' {} a -> s {backendConnectionErrors = a} :: TelemetryRecord)

-- |
telemetryRecord_segmentsRejectedCount :: Lens.Lens' TelemetryRecord (Prelude.Maybe Prelude.Int)
telemetryRecord_segmentsRejectedCount = Lens.lens (\TelemetryRecord' {segmentsRejectedCount} -> segmentsRejectedCount) (\s@TelemetryRecord' {} a -> s {segmentsRejectedCount = a} :: TelemetryRecord)

-- |
telemetryRecord_segmentsSentCount :: Lens.Lens' TelemetryRecord (Prelude.Maybe Prelude.Int)
telemetryRecord_segmentsSentCount = Lens.lens (\TelemetryRecord' {segmentsSentCount} -> segmentsSentCount) (\s@TelemetryRecord' {} a -> s {segmentsSentCount = a} :: TelemetryRecord)

-- |
telemetryRecord_segmentsReceivedCount :: Lens.Lens' TelemetryRecord (Prelude.Maybe Prelude.Int)
telemetryRecord_segmentsReceivedCount = Lens.lens (\TelemetryRecord' {segmentsReceivedCount} -> segmentsReceivedCount) (\s@TelemetryRecord' {} a -> s {segmentsReceivedCount = a} :: TelemetryRecord)

-- |
telemetryRecord_timestamp :: Lens.Lens' TelemetryRecord Prelude.UTCTime
telemetryRecord_timestamp = Lens.lens (\TelemetryRecord' {timestamp} -> timestamp) (\s@TelemetryRecord' {} a -> s {timestamp = a} :: TelemetryRecord) Prelude.. Prelude._Time

instance Prelude.Hashable TelemetryRecord

instance Prelude.NFData TelemetryRecord

instance Prelude.ToJSON TelemetryRecord where
  toJSON TelemetryRecord' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SegmentsSpilloverCount" Prelude..=)
              Prelude.<$> segmentsSpilloverCount,
            ("BackendConnectionErrors" Prelude..=)
              Prelude.<$> backendConnectionErrors,
            ("SegmentsRejectedCount" Prelude..=)
              Prelude.<$> segmentsRejectedCount,
            ("SegmentsSentCount" Prelude..=)
              Prelude.<$> segmentsSentCount,
            ("SegmentsReceivedCount" Prelude..=)
              Prelude.<$> segmentsReceivedCount,
            Prelude.Just ("Timestamp" Prelude..= timestamp)
          ]
      )
