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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.CheckpointConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.CheckpointConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.ConfigurationType
import qualified Amazonka.Prelude as Prelude

-- | Describes an application\'s checkpointing configuration. Checkpointing
-- is the process of persisting application state for fault tolerance. For
-- more information, see
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/concepts/programming-model.html#checkpoints-for-fault-tolerance Checkpoints for Fault Tolerance>
-- in the
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ Apache Flink Documentation>.
--
-- /See:/ 'newCheckpointConfiguration' smart constructor.
data CheckpointConfiguration = CheckpointConfiguration'
  { -- | Describes the interval in milliseconds between checkpoint operations.
    --
    -- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
    -- application will use a @CheckpointInterval@ value of 60000, even if this
    -- value is set to another value using this API or in application code.
    checkpointInterval :: Prelude.Maybe Prelude.Natural,
    -- | Describes whether checkpointing is enabled for a Flink-based Kinesis
    -- Data Analytics application.
    --
    -- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
    -- application will use a @CheckpointingEnabled@ value of @true@, even if
    -- this value is set to another value using this API or in application
    -- code.
    checkpointingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Describes the minimum time in milliseconds after a checkpoint operation
    -- completes that a new checkpoint operation can start. If a checkpoint
    -- operation takes longer than the @CheckpointInterval@, the application
    -- otherwise performs continual checkpoint operations. For more
    -- information, see
    -- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ops/state/large_state_tuning.html#tuning-checkpointing Tuning Checkpointing>
    -- in the
    -- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ Apache Flink Documentation>.
    --
    -- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
    -- application will use a @MinPauseBetweenCheckpoints@ value of 5000, even
    -- if this value is set using this API or in application code.
    minPauseBetweenCheckpoints :: Prelude.Maybe Prelude.Natural,
    -- | Describes whether the application uses Kinesis Data Analytics\' default
    -- checkpointing behavior. You must set this property to @CUSTOM@ in order
    -- to set the @CheckpointingEnabled@, @CheckpointInterval@, or
    -- @MinPauseBetweenCheckpoints@ parameters.
    --
    -- If this value is set to @DEFAULT@, the application will use the
    -- following values, even if they are set to other values using APIs or
    -- application code:
    --
    -- -   __CheckpointingEnabled:__ true
    --
    -- -   __CheckpointInterval:__ 60000
    --
    -- -   __MinPauseBetweenCheckpoints:__ 5000
    configurationType :: ConfigurationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckpointConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkpointInterval', 'checkpointConfiguration_checkpointInterval' - Describes the interval in milliseconds between checkpoint operations.
--
-- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
-- application will use a @CheckpointInterval@ value of 60000, even if this
-- value is set to another value using this API or in application code.
--
-- 'checkpointingEnabled', 'checkpointConfiguration_checkpointingEnabled' - Describes whether checkpointing is enabled for a Flink-based Kinesis
-- Data Analytics application.
--
-- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
-- application will use a @CheckpointingEnabled@ value of @true@, even if
-- this value is set to another value using this API or in application
-- code.
--
-- 'minPauseBetweenCheckpoints', 'checkpointConfiguration_minPauseBetweenCheckpoints' - Describes the minimum time in milliseconds after a checkpoint operation
-- completes that a new checkpoint operation can start. If a checkpoint
-- operation takes longer than the @CheckpointInterval@, the application
-- otherwise performs continual checkpoint operations. For more
-- information, see
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ops/state/large_state_tuning.html#tuning-checkpointing Tuning Checkpointing>
-- in the
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ Apache Flink Documentation>.
--
-- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
-- application will use a @MinPauseBetweenCheckpoints@ value of 5000, even
-- if this value is set using this API or in application code.
--
-- 'configurationType', 'checkpointConfiguration_configurationType' - Describes whether the application uses Kinesis Data Analytics\' default
-- checkpointing behavior. You must set this property to @CUSTOM@ in order
-- to set the @CheckpointingEnabled@, @CheckpointInterval@, or
-- @MinPauseBetweenCheckpoints@ parameters.
--
-- If this value is set to @DEFAULT@, the application will use the
-- following values, even if they are set to other values using APIs or
-- application code:
--
-- -   __CheckpointingEnabled:__ true
--
-- -   __CheckpointInterval:__ 60000
--
-- -   __MinPauseBetweenCheckpoints:__ 5000
newCheckpointConfiguration ::
  -- | 'configurationType'
  ConfigurationType ->
  CheckpointConfiguration
newCheckpointConfiguration pConfigurationType_ =
  CheckpointConfiguration'
    { checkpointInterval =
        Prelude.Nothing,
      checkpointingEnabled = Prelude.Nothing,
      minPauseBetweenCheckpoints = Prelude.Nothing,
      configurationType = pConfigurationType_
    }

-- | Describes the interval in milliseconds between checkpoint operations.
--
-- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
-- application will use a @CheckpointInterval@ value of 60000, even if this
-- value is set to another value using this API or in application code.
checkpointConfiguration_checkpointInterval :: Lens.Lens' CheckpointConfiguration (Prelude.Maybe Prelude.Natural)
checkpointConfiguration_checkpointInterval = Lens.lens (\CheckpointConfiguration' {checkpointInterval} -> checkpointInterval) (\s@CheckpointConfiguration' {} a -> s {checkpointInterval = a} :: CheckpointConfiguration)

-- | Describes whether checkpointing is enabled for a Flink-based Kinesis
-- Data Analytics application.
--
-- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
-- application will use a @CheckpointingEnabled@ value of @true@, even if
-- this value is set to another value using this API or in application
-- code.
checkpointConfiguration_checkpointingEnabled :: Lens.Lens' CheckpointConfiguration (Prelude.Maybe Prelude.Bool)
checkpointConfiguration_checkpointingEnabled = Lens.lens (\CheckpointConfiguration' {checkpointingEnabled} -> checkpointingEnabled) (\s@CheckpointConfiguration' {} a -> s {checkpointingEnabled = a} :: CheckpointConfiguration)

-- | Describes the minimum time in milliseconds after a checkpoint operation
-- completes that a new checkpoint operation can start. If a checkpoint
-- operation takes longer than the @CheckpointInterval@, the application
-- otherwise performs continual checkpoint operations. For more
-- information, see
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ops/state/large_state_tuning.html#tuning-checkpointing Tuning Checkpointing>
-- in the
-- <https://ci.apache.org/projects/flink/flink-docs-release-1.8/ Apache Flink Documentation>.
--
-- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
-- application will use a @MinPauseBetweenCheckpoints@ value of 5000, even
-- if this value is set using this API or in application code.
checkpointConfiguration_minPauseBetweenCheckpoints :: Lens.Lens' CheckpointConfiguration (Prelude.Maybe Prelude.Natural)
checkpointConfiguration_minPauseBetweenCheckpoints = Lens.lens (\CheckpointConfiguration' {minPauseBetweenCheckpoints} -> minPauseBetweenCheckpoints) (\s@CheckpointConfiguration' {} a -> s {minPauseBetweenCheckpoints = a} :: CheckpointConfiguration)

-- | Describes whether the application uses Kinesis Data Analytics\' default
-- checkpointing behavior. You must set this property to @CUSTOM@ in order
-- to set the @CheckpointingEnabled@, @CheckpointInterval@, or
-- @MinPauseBetweenCheckpoints@ parameters.
--
-- If this value is set to @DEFAULT@, the application will use the
-- following values, even if they are set to other values using APIs or
-- application code:
--
-- -   __CheckpointingEnabled:__ true
--
-- -   __CheckpointInterval:__ 60000
--
-- -   __MinPauseBetweenCheckpoints:__ 5000
checkpointConfiguration_configurationType :: Lens.Lens' CheckpointConfiguration ConfigurationType
checkpointConfiguration_configurationType = Lens.lens (\CheckpointConfiguration' {configurationType} -> configurationType) (\s@CheckpointConfiguration' {} a -> s {configurationType = a} :: CheckpointConfiguration)

instance Prelude.Hashable CheckpointConfiguration where
  hashWithSalt _salt CheckpointConfiguration' {..} =
    _salt `Prelude.hashWithSalt` checkpointInterval
      `Prelude.hashWithSalt` checkpointingEnabled
      `Prelude.hashWithSalt` minPauseBetweenCheckpoints
      `Prelude.hashWithSalt` configurationType

instance Prelude.NFData CheckpointConfiguration where
  rnf CheckpointConfiguration' {..} =
    Prelude.rnf checkpointInterval
      `Prelude.seq` Prelude.rnf checkpointingEnabled
      `Prelude.seq` Prelude.rnf minPauseBetweenCheckpoints
      `Prelude.seq` Prelude.rnf configurationType

instance Data.ToJSON CheckpointConfiguration where
  toJSON CheckpointConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CheckpointInterval" Data..=)
              Prelude.<$> checkpointInterval,
            ("CheckpointingEnabled" Data..=)
              Prelude.<$> checkpointingEnabled,
            ("MinPauseBetweenCheckpoints" Data..=)
              Prelude.<$> minPauseBetweenCheckpoints,
            Prelude.Just
              ("ConfigurationType" Data..= configurationType)
          ]
      )
