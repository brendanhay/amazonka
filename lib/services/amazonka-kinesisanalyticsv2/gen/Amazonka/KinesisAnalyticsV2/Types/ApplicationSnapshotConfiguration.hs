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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ApplicationSnapshotConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ApplicationSnapshotConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes whether snapshots are enabled for a Flink-based Kinesis Data
-- Analytics application.
--
-- /See:/ 'newApplicationSnapshotConfiguration' smart constructor.
data ApplicationSnapshotConfiguration = ApplicationSnapshotConfiguration'
  { -- | Describes whether snapshots are enabled for a Flink-based Kinesis Data
    -- Analytics application.
    snapshotsEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationSnapshotConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotsEnabled', 'applicationSnapshotConfiguration_snapshotsEnabled' - Describes whether snapshots are enabled for a Flink-based Kinesis Data
-- Analytics application.
newApplicationSnapshotConfiguration ::
  -- | 'snapshotsEnabled'
  Prelude.Bool ->
  ApplicationSnapshotConfiguration
newApplicationSnapshotConfiguration
  pSnapshotsEnabled_ =
    ApplicationSnapshotConfiguration'
      { snapshotsEnabled =
          pSnapshotsEnabled_
      }

-- | Describes whether snapshots are enabled for a Flink-based Kinesis Data
-- Analytics application.
applicationSnapshotConfiguration_snapshotsEnabled :: Lens.Lens' ApplicationSnapshotConfiguration Prelude.Bool
applicationSnapshotConfiguration_snapshotsEnabled = Lens.lens (\ApplicationSnapshotConfiguration' {snapshotsEnabled} -> snapshotsEnabled) (\s@ApplicationSnapshotConfiguration' {} a -> s {snapshotsEnabled = a} :: ApplicationSnapshotConfiguration)

instance
  Prelude.Hashable
    ApplicationSnapshotConfiguration
  where
  hashWithSalt
    _salt
    ApplicationSnapshotConfiguration' {..} =
      _salt `Prelude.hashWithSalt` snapshotsEnabled

instance
  Prelude.NFData
    ApplicationSnapshotConfiguration
  where
  rnf ApplicationSnapshotConfiguration' {..} =
    Prelude.rnf snapshotsEnabled

instance Data.ToJSON ApplicationSnapshotConfiguration where
  toJSON ApplicationSnapshotConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SnapshotsEnabled" Data..= snapshotsEnabled)
          ]
      )
