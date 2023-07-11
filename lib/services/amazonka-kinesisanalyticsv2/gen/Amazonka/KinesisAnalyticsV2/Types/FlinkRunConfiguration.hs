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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.FlinkRunConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.FlinkRunConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the starting parameters for a Flink-based Kinesis Data
-- Analytics application.
--
-- /See:/ 'newFlinkRunConfiguration' smart constructor.
data FlinkRunConfiguration = FlinkRunConfiguration'
  { -- | When restoring from a snapshot, specifies whether the runtime is allowed
    -- to skip a state that cannot be mapped to the new program. This will
    -- happen if the program is updated between snapshots to remove stateful
    -- parameters, and state data in the snapshot no longer corresponds to
    -- valid application data. For more information, see
    -- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ops/state/savepoints.html#allowing-non-restored-state Allowing Non-Restored State>
    -- in the
    -- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ Apache Flink documentation>.
    --
    -- This value defaults to @false@. If you update your application without
    -- specifying this parameter, @AllowNonRestoredState@ will be set to
    -- @false@, even if it was previously set to @true@.
    allowNonRestoredState :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlinkRunConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowNonRestoredState', 'flinkRunConfiguration_allowNonRestoredState' - When restoring from a snapshot, specifies whether the runtime is allowed
-- to skip a state that cannot be mapped to the new program. This will
-- happen if the program is updated between snapshots to remove stateful
-- parameters, and state data in the snapshot no longer corresponds to
-- valid application data. For more information, see
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ops/state/savepoints.html#allowing-non-restored-state Allowing Non-Restored State>
-- in the
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ Apache Flink documentation>.
--
-- This value defaults to @false@. If you update your application without
-- specifying this parameter, @AllowNonRestoredState@ will be set to
-- @false@, even if it was previously set to @true@.
newFlinkRunConfiguration ::
  FlinkRunConfiguration
newFlinkRunConfiguration =
  FlinkRunConfiguration'
    { allowNonRestoredState =
        Prelude.Nothing
    }

-- | When restoring from a snapshot, specifies whether the runtime is allowed
-- to skip a state that cannot be mapped to the new program. This will
-- happen if the program is updated between snapshots to remove stateful
-- parameters, and state data in the snapshot no longer corresponds to
-- valid application data. For more information, see
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ops/state/savepoints.html#allowing-non-restored-state Allowing Non-Restored State>
-- in the
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ Apache Flink documentation>.
--
-- This value defaults to @false@. If you update your application without
-- specifying this parameter, @AllowNonRestoredState@ will be set to
-- @false@, even if it was previously set to @true@.
flinkRunConfiguration_allowNonRestoredState :: Lens.Lens' FlinkRunConfiguration (Prelude.Maybe Prelude.Bool)
flinkRunConfiguration_allowNonRestoredState = Lens.lens (\FlinkRunConfiguration' {allowNonRestoredState} -> allowNonRestoredState) (\s@FlinkRunConfiguration' {} a -> s {allowNonRestoredState = a} :: FlinkRunConfiguration)

instance Data.FromJSON FlinkRunConfiguration where
  parseJSON =
    Data.withObject
      "FlinkRunConfiguration"
      ( \x ->
          FlinkRunConfiguration'
            Prelude.<$> (x Data..:? "AllowNonRestoredState")
      )

instance Prelude.Hashable FlinkRunConfiguration where
  hashWithSalt _salt FlinkRunConfiguration' {..} =
    _salt `Prelude.hashWithSalt` allowNonRestoredState

instance Prelude.NFData FlinkRunConfiguration where
  rnf FlinkRunConfiguration' {..} =
    Prelude.rnf allowNonRestoredState

instance Data.ToJSON FlinkRunConfiguration where
  toJSON FlinkRunConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowNonRestoredState" Data..=)
              Prelude.<$> allowNonRestoredState
          ]
      )
