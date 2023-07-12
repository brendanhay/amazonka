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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ApplicationSnapshotConfigurationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ApplicationSnapshotConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes updates to whether snapshots are enabled for a Flink-based
-- Kinesis Data Analytics application.
--
-- /See:/ 'newApplicationSnapshotConfigurationUpdate' smart constructor.
data ApplicationSnapshotConfigurationUpdate = ApplicationSnapshotConfigurationUpdate'
  { -- | Describes updates to whether snapshots are enabled for an application.
    snapshotsEnabledUpdate :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationSnapshotConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotsEnabledUpdate', 'applicationSnapshotConfigurationUpdate_snapshotsEnabledUpdate' - Describes updates to whether snapshots are enabled for an application.
newApplicationSnapshotConfigurationUpdate ::
  -- | 'snapshotsEnabledUpdate'
  Prelude.Bool ->
  ApplicationSnapshotConfigurationUpdate
newApplicationSnapshotConfigurationUpdate
  pSnapshotsEnabledUpdate_ =
    ApplicationSnapshotConfigurationUpdate'
      { snapshotsEnabledUpdate =
          pSnapshotsEnabledUpdate_
      }

-- | Describes updates to whether snapshots are enabled for an application.
applicationSnapshotConfigurationUpdate_snapshotsEnabledUpdate :: Lens.Lens' ApplicationSnapshotConfigurationUpdate Prelude.Bool
applicationSnapshotConfigurationUpdate_snapshotsEnabledUpdate = Lens.lens (\ApplicationSnapshotConfigurationUpdate' {snapshotsEnabledUpdate} -> snapshotsEnabledUpdate) (\s@ApplicationSnapshotConfigurationUpdate' {} a -> s {snapshotsEnabledUpdate = a} :: ApplicationSnapshotConfigurationUpdate)

instance
  Prelude.Hashable
    ApplicationSnapshotConfigurationUpdate
  where
  hashWithSalt
    _salt
    ApplicationSnapshotConfigurationUpdate' {..} =
      _salt `Prelude.hashWithSalt` snapshotsEnabledUpdate

instance
  Prelude.NFData
    ApplicationSnapshotConfigurationUpdate
  where
  rnf ApplicationSnapshotConfigurationUpdate' {..} =
    Prelude.rnf snapshotsEnabledUpdate

instance
  Data.ToJSON
    ApplicationSnapshotConfigurationUpdate
  where
  toJSON ApplicationSnapshotConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "SnapshotsEnabledUpdate"
                  Data..= snapshotsEnabledUpdate
              )
          ]
      )
