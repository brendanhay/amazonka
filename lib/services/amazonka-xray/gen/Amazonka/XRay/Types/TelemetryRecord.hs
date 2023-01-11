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
-- Module      : Amazonka.XRay.Types.TelemetryRecord
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.TelemetryRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.BackendConnectionErrors

-- |
--
-- /See:/ 'newTelemetryRecord' smart constructor.
data TelemetryRecord = TelemetryRecord'
  { backendConnectionErrors :: Prelude.Maybe BackendConnectionErrors,
    segmentsReceivedCount :: Prelude.Maybe Prelude.Int,
    segmentsRejectedCount :: Prelude.Maybe Prelude.Int,
    segmentsSentCount :: Prelude.Maybe Prelude.Int,
    segmentsSpilloverCount :: Prelude.Maybe Prelude.Int,
    timestamp :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TelemetryRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backendConnectionErrors', 'telemetryRecord_backendConnectionErrors' -
--
-- 'segmentsReceivedCount', 'telemetryRecord_segmentsReceivedCount' -
--
-- 'segmentsRejectedCount', 'telemetryRecord_segmentsRejectedCount' -
--
-- 'segmentsSentCount', 'telemetryRecord_segmentsSentCount' -
--
-- 'segmentsSpilloverCount', 'telemetryRecord_segmentsSpilloverCount' -
--
-- 'timestamp', 'telemetryRecord_timestamp' -
newTelemetryRecord ::
  -- | 'timestamp'
  Prelude.UTCTime ->
  TelemetryRecord
newTelemetryRecord pTimestamp_ =
  TelemetryRecord'
    { backendConnectionErrors =
        Prelude.Nothing,
      segmentsReceivedCount = Prelude.Nothing,
      segmentsRejectedCount = Prelude.Nothing,
      segmentsSentCount = Prelude.Nothing,
      segmentsSpilloverCount = Prelude.Nothing,
      timestamp = Data._Time Lens.# pTimestamp_
    }

-- |
telemetryRecord_backendConnectionErrors :: Lens.Lens' TelemetryRecord (Prelude.Maybe BackendConnectionErrors)
telemetryRecord_backendConnectionErrors = Lens.lens (\TelemetryRecord' {backendConnectionErrors} -> backendConnectionErrors) (\s@TelemetryRecord' {} a -> s {backendConnectionErrors = a} :: TelemetryRecord)

-- |
telemetryRecord_segmentsReceivedCount :: Lens.Lens' TelemetryRecord (Prelude.Maybe Prelude.Int)
telemetryRecord_segmentsReceivedCount = Lens.lens (\TelemetryRecord' {segmentsReceivedCount} -> segmentsReceivedCount) (\s@TelemetryRecord' {} a -> s {segmentsReceivedCount = a} :: TelemetryRecord)

-- |
telemetryRecord_segmentsRejectedCount :: Lens.Lens' TelemetryRecord (Prelude.Maybe Prelude.Int)
telemetryRecord_segmentsRejectedCount = Lens.lens (\TelemetryRecord' {segmentsRejectedCount} -> segmentsRejectedCount) (\s@TelemetryRecord' {} a -> s {segmentsRejectedCount = a} :: TelemetryRecord)

-- |
telemetryRecord_segmentsSentCount :: Lens.Lens' TelemetryRecord (Prelude.Maybe Prelude.Int)
telemetryRecord_segmentsSentCount = Lens.lens (\TelemetryRecord' {segmentsSentCount} -> segmentsSentCount) (\s@TelemetryRecord' {} a -> s {segmentsSentCount = a} :: TelemetryRecord)

-- |
telemetryRecord_segmentsSpilloverCount :: Lens.Lens' TelemetryRecord (Prelude.Maybe Prelude.Int)
telemetryRecord_segmentsSpilloverCount = Lens.lens (\TelemetryRecord' {segmentsSpilloverCount} -> segmentsSpilloverCount) (\s@TelemetryRecord' {} a -> s {segmentsSpilloverCount = a} :: TelemetryRecord)

-- |
telemetryRecord_timestamp :: Lens.Lens' TelemetryRecord Prelude.UTCTime
telemetryRecord_timestamp = Lens.lens (\TelemetryRecord' {timestamp} -> timestamp) (\s@TelemetryRecord' {} a -> s {timestamp = a} :: TelemetryRecord) Prelude.. Data._Time

instance Prelude.Hashable TelemetryRecord where
  hashWithSalt _salt TelemetryRecord' {..} =
    _salt
      `Prelude.hashWithSalt` backendConnectionErrors
      `Prelude.hashWithSalt` segmentsReceivedCount
      `Prelude.hashWithSalt` segmentsRejectedCount
      `Prelude.hashWithSalt` segmentsSentCount
      `Prelude.hashWithSalt` segmentsSpilloverCount
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData TelemetryRecord where
  rnf TelemetryRecord' {..} =
    Prelude.rnf backendConnectionErrors
      `Prelude.seq` Prelude.rnf segmentsReceivedCount
      `Prelude.seq` Prelude.rnf segmentsRejectedCount
      `Prelude.seq` Prelude.rnf segmentsSentCount
      `Prelude.seq` Prelude.rnf segmentsSpilloverCount
      `Prelude.seq` Prelude.rnf timestamp

instance Data.ToJSON TelemetryRecord where
  toJSON TelemetryRecord' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackendConnectionErrors" Data..=)
              Prelude.<$> backendConnectionErrors,
            ("SegmentsReceivedCount" Data..=)
              Prelude.<$> segmentsReceivedCount,
            ("SegmentsRejectedCount" Data..=)
              Prelude.<$> segmentsRejectedCount,
            ("SegmentsSentCount" Data..=)
              Prelude.<$> segmentsSentCount,
            ("SegmentsSpilloverCount" Data..=)
              Prelude.<$> segmentsSpilloverCount,
            Prelude.Just ("Timestamp" Data..= timestamp)
          ]
      )
