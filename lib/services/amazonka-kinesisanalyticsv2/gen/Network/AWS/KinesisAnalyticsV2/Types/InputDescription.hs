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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.InputDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.InputDescription where

import qualified Amazonka.Core as Core
import Amazonka.KinesisAnalyticsV2.Types.InputParallelism
import Amazonka.KinesisAnalyticsV2.Types.InputProcessingConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.InputStartingPositionConfiguration
import Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseInputDescription
import Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsInputDescription
import Amazonka.KinesisAnalyticsV2.Types.SourceSchema
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the application input configuration for a SQL-based Kinesis
-- Data Analytics application.
--
-- /See:/ 'newInputDescription' smart constructor.
data InputDescription = InputDescription'
  { -- | The point at which the application is configured to read from the input
    -- stream.
    inputStartingPositionConfiguration :: Prelude.Maybe InputStartingPositionConfiguration,
    -- | Describes the configured parallelism (number of in-application streams
    -- mapped to the streaming source).
    inputParallelism :: Prelude.Maybe InputParallelism,
    -- | The input ID that is associated with the application input. This is the
    -- ID that Kinesis Data Analytics assigns to each input configuration that
    -- you add to your application.
    inputId :: Prelude.Maybe Prelude.Text,
    -- | Returns the in-application stream names that are mapped to the stream
    -- source.
    inAppStreamNames :: Prelude.Maybe [Prelude.Text],
    -- | If a Kinesis Data Firehose delivery stream is configured as a streaming
    -- source, provides the delivery stream\'s ARN.
    kinesisFirehoseInputDescription :: Prelude.Maybe KinesisFirehoseInputDescription,
    -- | Describes the format of the data in the streaming source, and how each
    -- data element maps to corresponding columns in the in-application stream
    -- that is being created.
    inputSchema :: Prelude.Maybe SourceSchema,
    -- | If a Kinesis data stream is configured as a streaming source, provides
    -- the Kinesis data stream\'s Amazon Resource Name (ARN).
    kinesisStreamsInputDescription :: Prelude.Maybe KinesisStreamsInputDescription,
    -- | The in-application name prefix.
    namePrefix :: Prelude.Maybe Prelude.Text,
    -- | The description of the preprocessor that executes on records in this
    -- input before the application\'s code is run.
    inputProcessingConfigurationDescription :: Prelude.Maybe InputProcessingConfigurationDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputStartingPositionConfiguration', 'inputDescription_inputStartingPositionConfiguration' - The point at which the application is configured to read from the input
-- stream.
--
-- 'inputParallelism', 'inputDescription_inputParallelism' - Describes the configured parallelism (number of in-application streams
-- mapped to the streaming source).
--
-- 'inputId', 'inputDescription_inputId' - The input ID that is associated with the application input. This is the
-- ID that Kinesis Data Analytics assigns to each input configuration that
-- you add to your application.
--
-- 'inAppStreamNames', 'inputDescription_inAppStreamNames' - Returns the in-application stream names that are mapped to the stream
-- source.
--
-- 'kinesisFirehoseInputDescription', 'inputDescription_kinesisFirehoseInputDescription' - If a Kinesis Data Firehose delivery stream is configured as a streaming
-- source, provides the delivery stream\'s ARN.
--
-- 'inputSchema', 'inputDescription_inputSchema' - Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns in the in-application stream
-- that is being created.
--
-- 'kinesisStreamsInputDescription', 'inputDescription_kinesisStreamsInputDescription' - If a Kinesis data stream is configured as a streaming source, provides
-- the Kinesis data stream\'s Amazon Resource Name (ARN).
--
-- 'namePrefix', 'inputDescription_namePrefix' - The in-application name prefix.
--
-- 'inputProcessingConfigurationDescription', 'inputDescription_inputProcessingConfigurationDescription' - The description of the preprocessor that executes on records in this
-- input before the application\'s code is run.
newInputDescription ::
  InputDescription
newInputDescription =
  InputDescription'
    { inputStartingPositionConfiguration =
        Prelude.Nothing,
      inputParallelism = Prelude.Nothing,
      inputId = Prelude.Nothing,
      inAppStreamNames = Prelude.Nothing,
      kinesisFirehoseInputDescription = Prelude.Nothing,
      inputSchema = Prelude.Nothing,
      kinesisStreamsInputDescription = Prelude.Nothing,
      namePrefix = Prelude.Nothing,
      inputProcessingConfigurationDescription =
        Prelude.Nothing
    }

-- | The point at which the application is configured to read from the input
-- stream.
inputDescription_inputStartingPositionConfiguration :: Lens.Lens' InputDescription (Prelude.Maybe InputStartingPositionConfiguration)
inputDescription_inputStartingPositionConfiguration = Lens.lens (\InputDescription' {inputStartingPositionConfiguration} -> inputStartingPositionConfiguration) (\s@InputDescription' {} a -> s {inputStartingPositionConfiguration = a} :: InputDescription)

-- | Describes the configured parallelism (number of in-application streams
-- mapped to the streaming source).
inputDescription_inputParallelism :: Lens.Lens' InputDescription (Prelude.Maybe InputParallelism)
inputDescription_inputParallelism = Lens.lens (\InputDescription' {inputParallelism} -> inputParallelism) (\s@InputDescription' {} a -> s {inputParallelism = a} :: InputDescription)

-- | The input ID that is associated with the application input. This is the
-- ID that Kinesis Data Analytics assigns to each input configuration that
-- you add to your application.
inputDescription_inputId :: Lens.Lens' InputDescription (Prelude.Maybe Prelude.Text)
inputDescription_inputId = Lens.lens (\InputDescription' {inputId} -> inputId) (\s@InputDescription' {} a -> s {inputId = a} :: InputDescription)

-- | Returns the in-application stream names that are mapped to the stream
-- source.
inputDescription_inAppStreamNames :: Lens.Lens' InputDescription (Prelude.Maybe [Prelude.Text])
inputDescription_inAppStreamNames = Lens.lens (\InputDescription' {inAppStreamNames} -> inAppStreamNames) (\s@InputDescription' {} a -> s {inAppStreamNames = a} :: InputDescription) Prelude.. Lens.mapping Lens.coerced

-- | If a Kinesis Data Firehose delivery stream is configured as a streaming
-- source, provides the delivery stream\'s ARN.
inputDescription_kinesisFirehoseInputDescription :: Lens.Lens' InputDescription (Prelude.Maybe KinesisFirehoseInputDescription)
inputDescription_kinesisFirehoseInputDescription = Lens.lens (\InputDescription' {kinesisFirehoseInputDescription} -> kinesisFirehoseInputDescription) (\s@InputDescription' {} a -> s {kinesisFirehoseInputDescription = a} :: InputDescription)

-- | Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns in the in-application stream
-- that is being created.
inputDescription_inputSchema :: Lens.Lens' InputDescription (Prelude.Maybe SourceSchema)
inputDescription_inputSchema = Lens.lens (\InputDescription' {inputSchema} -> inputSchema) (\s@InputDescription' {} a -> s {inputSchema = a} :: InputDescription)

-- | If a Kinesis data stream is configured as a streaming source, provides
-- the Kinesis data stream\'s Amazon Resource Name (ARN).
inputDescription_kinesisStreamsInputDescription :: Lens.Lens' InputDescription (Prelude.Maybe KinesisStreamsInputDescription)
inputDescription_kinesisStreamsInputDescription = Lens.lens (\InputDescription' {kinesisStreamsInputDescription} -> kinesisStreamsInputDescription) (\s@InputDescription' {} a -> s {kinesisStreamsInputDescription = a} :: InputDescription)

-- | The in-application name prefix.
inputDescription_namePrefix :: Lens.Lens' InputDescription (Prelude.Maybe Prelude.Text)
inputDescription_namePrefix = Lens.lens (\InputDescription' {namePrefix} -> namePrefix) (\s@InputDescription' {} a -> s {namePrefix = a} :: InputDescription)

-- | The description of the preprocessor that executes on records in this
-- input before the application\'s code is run.
inputDescription_inputProcessingConfigurationDescription :: Lens.Lens' InputDescription (Prelude.Maybe InputProcessingConfigurationDescription)
inputDescription_inputProcessingConfigurationDescription = Lens.lens (\InputDescription' {inputProcessingConfigurationDescription} -> inputProcessingConfigurationDescription) (\s@InputDescription' {} a -> s {inputProcessingConfigurationDescription = a} :: InputDescription)

instance Core.FromJSON InputDescription where
  parseJSON =
    Core.withObject
      "InputDescription"
      ( \x ->
          InputDescription'
            Prelude.<$> (x Core..:? "InputStartingPositionConfiguration")
            Prelude.<*> (x Core..:? "InputParallelism")
            Prelude.<*> (x Core..:? "InputId")
            Prelude.<*> ( x Core..:? "InAppStreamNames"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "KinesisFirehoseInputDescription")
            Prelude.<*> (x Core..:? "InputSchema")
            Prelude.<*> (x Core..:? "KinesisStreamsInputDescription")
            Prelude.<*> (x Core..:? "NamePrefix")
            Prelude.<*> ( x
                            Core..:? "InputProcessingConfigurationDescription"
                        )
      )

instance Prelude.Hashable InputDescription

instance Prelude.NFData InputDescription
