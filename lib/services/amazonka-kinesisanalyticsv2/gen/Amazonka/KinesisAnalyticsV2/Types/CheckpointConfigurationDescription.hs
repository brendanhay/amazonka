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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.CheckpointConfigurationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.CheckpointConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.ConfigurationType
import qualified Amazonka.Prelude as Prelude

-- | Describes checkpointing parameters for a Flink-based Kinesis Data
-- Analytics application.
--
-- /See:/ 'newCheckpointConfigurationDescription' smart constructor.
data CheckpointConfigurationDescription = CheckpointConfigurationDescription'
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
    -- | Describes whether the application uses the default checkpointing
    -- behavior in Kinesis Data Analytics.
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
    configurationType :: Prelude.Maybe ConfigurationType,
    -- | Describes the minimum time in milliseconds after a checkpoint operation
    -- completes that a new checkpoint operation can start.
    --
    -- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
    -- application will use a @MinPauseBetweenCheckpoints@ value of 5000, even
    -- if this value is set using this API or in application code.
    minPauseBetweenCheckpoints :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckpointConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkpointInterval', 'checkpointConfigurationDescription_checkpointInterval' - Describes the interval in milliseconds between checkpoint operations.
--
-- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
-- application will use a @CheckpointInterval@ value of 60000, even if this
-- value is set to another value using this API or in application code.
--
-- 'checkpointingEnabled', 'checkpointConfigurationDescription_checkpointingEnabled' - Describes whether checkpointing is enabled for a Flink-based Kinesis
-- Data Analytics application.
--
-- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
-- application will use a @CheckpointingEnabled@ value of @true@, even if
-- this value is set to another value using this API or in application
-- code.
--
-- 'configurationType', 'checkpointConfigurationDescription_configurationType' - Describes whether the application uses the default checkpointing
-- behavior in Kinesis Data Analytics.
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
--
-- 'minPauseBetweenCheckpoints', 'checkpointConfigurationDescription_minPauseBetweenCheckpoints' - Describes the minimum time in milliseconds after a checkpoint operation
-- completes that a new checkpoint operation can start.
--
-- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
-- application will use a @MinPauseBetweenCheckpoints@ value of 5000, even
-- if this value is set using this API or in application code.
newCheckpointConfigurationDescription ::
  CheckpointConfigurationDescription
newCheckpointConfigurationDescription =
  CheckpointConfigurationDescription'
    { checkpointInterval =
        Prelude.Nothing,
      checkpointingEnabled = Prelude.Nothing,
      configurationType = Prelude.Nothing,
      minPauseBetweenCheckpoints =
        Prelude.Nothing
    }

-- | Describes the interval in milliseconds between checkpoint operations.
--
-- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
-- application will use a @CheckpointInterval@ value of 60000, even if this
-- value is set to another value using this API or in application code.
checkpointConfigurationDescription_checkpointInterval :: Lens.Lens' CheckpointConfigurationDescription (Prelude.Maybe Prelude.Natural)
checkpointConfigurationDescription_checkpointInterval = Lens.lens (\CheckpointConfigurationDescription' {checkpointInterval} -> checkpointInterval) (\s@CheckpointConfigurationDescription' {} a -> s {checkpointInterval = a} :: CheckpointConfigurationDescription)

-- | Describes whether checkpointing is enabled for a Flink-based Kinesis
-- Data Analytics application.
--
-- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
-- application will use a @CheckpointingEnabled@ value of @true@, even if
-- this value is set to another value using this API or in application
-- code.
checkpointConfigurationDescription_checkpointingEnabled :: Lens.Lens' CheckpointConfigurationDescription (Prelude.Maybe Prelude.Bool)
checkpointConfigurationDescription_checkpointingEnabled = Lens.lens (\CheckpointConfigurationDescription' {checkpointingEnabled} -> checkpointingEnabled) (\s@CheckpointConfigurationDescription' {} a -> s {checkpointingEnabled = a} :: CheckpointConfigurationDescription)

-- | Describes whether the application uses the default checkpointing
-- behavior in Kinesis Data Analytics.
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
checkpointConfigurationDescription_configurationType :: Lens.Lens' CheckpointConfigurationDescription (Prelude.Maybe ConfigurationType)
checkpointConfigurationDescription_configurationType = Lens.lens (\CheckpointConfigurationDescription' {configurationType} -> configurationType) (\s@CheckpointConfigurationDescription' {} a -> s {configurationType = a} :: CheckpointConfigurationDescription)

-- | Describes the minimum time in milliseconds after a checkpoint operation
-- completes that a new checkpoint operation can start.
--
-- If @CheckpointConfiguration.ConfigurationType@ is @DEFAULT@, the
-- application will use a @MinPauseBetweenCheckpoints@ value of 5000, even
-- if this value is set using this API or in application code.
checkpointConfigurationDescription_minPauseBetweenCheckpoints :: Lens.Lens' CheckpointConfigurationDescription (Prelude.Maybe Prelude.Natural)
checkpointConfigurationDescription_minPauseBetweenCheckpoints = Lens.lens (\CheckpointConfigurationDescription' {minPauseBetweenCheckpoints} -> minPauseBetweenCheckpoints) (\s@CheckpointConfigurationDescription' {} a -> s {minPauseBetweenCheckpoints = a} :: CheckpointConfigurationDescription)

instance
  Data.FromJSON
    CheckpointConfigurationDescription
  where
  parseJSON =
    Data.withObject
      "CheckpointConfigurationDescription"
      ( \x ->
          CheckpointConfigurationDescription'
            Prelude.<$> (x Data..:? "CheckpointInterval")
            Prelude.<*> (x Data..:? "CheckpointingEnabled")
            Prelude.<*> (x Data..:? "ConfigurationType")
            Prelude.<*> (x Data..:? "MinPauseBetweenCheckpoints")
      )

instance
  Prelude.Hashable
    CheckpointConfigurationDescription
  where
  hashWithSalt
    _salt
    CheckpointConfigurationDescription' {..} =
      _salt `Prelude.hashWithSalt` checkpointInterval
        `Prelude.hashWithSalt` checkpointingEnabled
        `Prelude.hashWithSalt` configurationType
        `Prelude.hashWithSalt` minPauseBetweenCheckpoints

instance
  Prelude.NFData
    CheckpointConfigurationDescription
  where
  rnf CheckpointConfigurationDescription' {..} =
    Prelude.rnf checkpointInterval
      `Prelude.seq` Prelude.rnf checkpointingEnabled
      `Prelude.seq` Prelude.rnf configurationType
      `Prelude.seq` Prelude.rnf minPauseBetweenCheckpoints
