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
-- Module      : Network.AWS.KinesisAnalytics.Types.InputDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types.InputParallelism
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription
import Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import qualified Network.AWS.Lens as Lens

-- | Describes the application input configuration. For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
--
-- /See:/ 'newInputDescription' smart constructor.
data InputDescription = InputDescription'
  { -- | Describes the format of the data in the streaming source, and how each
    -- data element maps to corresponding columns in the in-application stream
    -- that is being created.
    inputSchema :: Core.Maybe SourceSchema,
    -- | Point at which the application is configured to read from the input
    -- stream.
    inputStartingPositionConfiguration :: Core.Maybe InputStartingPositionConfiguration,
    -- | The description of the preprocessor that executes on records in this
    -- input before the application\'s code is run.
    inputProcessingConfigurationDescription :: Core.Maybe InputProcessingConfigurationDescription,
    -- | Describes the configured parallelism (number of in-application streams
    -- mapped to the streaming source).
    inputParallelism :: Core.Maybe InputParallelism,
    -- | In-application name prefix.
    namePrefix :: Core.Maybe Core.Text,
    -- | If an Amazon Kinesis stream is configured as streaming source, provides
    -- Amazon Kinesis stream\'s Amazon Resource Name (ARN) and an IAM role that
    -- enables Amazon Kinesis Analytics to access the stream on your behalf.
    kinesisStreamsInputDescription :: Core.Maybe KinesisStreamsInputDescription,
    -- | If an Amazon Kinesis Firehose delivery stream is configured as a
    -- streaming source, provides the delivery stream\'s ARN and an IAM role
    -- that enables Amazon Kinesis Analytics to access the stream on your
    -- behalf.
    kinesisFirehoseInputDescription :: Core.Maybe KinesisFirehoseInputDescription,
    -- | Returns the in-application stream names that are mapped to the stream
    -- source.
    inAppStreamNames :: Core.Maybe [Core.Text],
    -- | Input ID associated with the application input. This is the ID that
    -- Amazon Kinesis Analytics assigns to each input configuration you add to
    -- your application.
    inputId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputSchema', 'inputDescription_inputSchema' - Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns in the in-application stream
-- that is being created.
--
-- 'inputStartingPositionConfiguration', 'inputDescription_inputStartingPositionConfiguration' - Point at which the application is configured to read from the input
-- stream.
--
-- 'inputProcessingConfigurationDescription', 'inputDescription_inputProcessingConfigurationDescription' - The description of the preprocessor that executes on records in this
-- input before the application\'s code is run.
--
-- 'inputParallelism', 'inputDescription_inputParallelism' - Describes the configured parallelism (number of in-application streams
-- mapped to the streaming source).
--
-- 'namePrefix', 'inputDescription_namePrefix' - In-application name prefix.
--
-- 'kinesisStreamsInputDescription', 'inputDescription_kinesisStreamsInputDescription' - If an Amazon Kinesis stream is configured as streaming source, provides
-- Amazon Kinesis stream\'s Amazon Resource Name (ARN) and an IAM role that
-- enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- 'kinesisFirehoseInputDescription', 'inputDescription_kinesisFirehoseInputDescription' - If an Amazon Kinesis Firehose delivery stream is configured as a
-- streaming source, provides the delivery stream\'s ARN and an IAM role
-- that enables Amazon Kinesis Analytics to access the stream on your
-- behalf.
--
-- 'inAppStreamNames', 'inputDescription_inAppStreamNames' - Returns the in-application stream names that are mapped to the stream
-- source.
--
-- 'inputId', 'inputDescription_inputId' - Input ID associated with the application input. This is the ID that
-- Amazon Kinesis Analytics assigns to each input configuration you add to
-- your application.
newInputDescription ::
  InputDescription
newInputDescription =
  InputDescription'
    { inputSchema = Core.Nothing,
      inputStartingPositionConfiguration = Core.Nothing,
      inputProcessingConfigurationDescription =
        Core.Nothing,
      inputParallelism = Core.Nothing,
      namePrefix = Core.Nothing,
      kinesisStreamsInputDescription = Core.Nothing,
      kinesisFirehoseInputDescription = Core.Nothing,
      inAppStreamNames = Core.Nothing,
      inputId = Core.Nothing
    }

-- | Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns in the in-application stream
-- that is being created.
inputDescription_inputSchema :: Lens.Lens' InputDescription (Core.Maybe SourceSchema)
inputDescription_inputSchema = Lens.lens (\InputDescription' {inputSchema} -> inputSchema) (\s@InputDescription' {} a -> s {inputSchema = a} :: InputDescription)

-- | Point at which the application is configured to read from the input
-- stream.
inputDescription_inputStartingPositionConfiguration :: Lens.Lens' InputDescription (Core.Maybe InputStartingPositionConfiguration)
inputDescription_inputStartingPositionConfiguration = Lens.lens (\InputDescription' {inputStartingPositionConfiguration} -> inputStartingPositionConfiguration) (\s@InputDescription' {} a -> s {inputStartingPositionConfiguration = a} :: InputDescription)

-- | The description of the preprocessor that executes on records in this
-- input before the application\'s code is run.
inputDescription_inputProcessingConfigurationDescription :: Lens.Lens' InputDescription (Core.Maybe InputProcessingConfigurationDescription)
inputDescription_inputProcessingConfigurationDescription = Lens.lens (\InputDescription' {inputProcessingConfigurationDescription} -> inputProcessingConfigurationDescription) (\s@InputDescription' {} a -> s {inputProcessingConfigurationDescription = a} :: InputDescription)

-- | Describes the configured parallelism (number of in-application streams
-- mapped to the streaming source).
inputDescription_inputParallelism :: Lens.Lens' InputDescription (Core.Maybe InputParallelism)
inputDescription_inputParallelism = Lens.lens (\InputDescription' {inputParallelism} -> inputParallelism) (\s@InputDescription' {} a -> s {inputParallelism = a} :: InputDescription)

-- | In-application name prefix.
inputDescription_namePrefix :: Lens.Lens' InputDescription (Core.Maybe Core.Text)
inputDescription_namePrefix = Lens.lens (\InputDescription' {namePrefix} -> namePrefix) (\s@InputDescription' {} a -> s {namePrefix = a} :: InputDescription)

-- | If an Amazon Kinesis stream is configured as streaming source, provides
-- Amazon Kinesis stream\'s Amazon Resource Name (ARN) and an IAM role that
-- enables Amazon Kinesis Analytics to access the stream on your behalf.
inputDescription_kinesisStreamsInputDescription :: Lens.Lens' InputDescription (Core.Maybe KinesisStreamsInputDescription)
inputDescription_kinesisStreamsInputDescription = Lens.lens (\InputDescription' {kinesisStreamsInputDescription} -> kinesisStreamsInputDescription) (\s@InputDescription' {} a -> s {kinesisStreamsInputDescription = a} :: InputDescription)

-- | If an Amazon Kinesis Firehose delivery stream is configured as a
-- streaming source, provides the delivery stream\'s ARN and an IAM role
-- that enables Amazon Kinesis Analytics to access the stream on your
-- behalf.
inputDescription_kinesisFirehoseInputDescription :: Lens.Lens' InputDescription (Core.Maybe KinesisFirehoseInputDescription)
inputDescription_kinesisFirehoseInputDescription = Lens.lens (\InputDescription' {kinesisFirehoseInputDescription} -> kinesisFirehoseInputDescription) (\s@InputDescription' {} a -> s {kinesisFirehoseInputDescription = a} :: InputDescription)

-- | Returns the in-application stream names that are mapped to the stream
-- source.
inputDescription_inAppStreamNames :: Lens.Lens' InputDescription (Core.Maybe [Core.Text])
inputDescription_inAppStreamNames = Lens.lens (\InputDescription' {inAppStreamNames} -> inAppStreamNames) (\s@InputDescription' {} a -> s {inAppStreamNames = a} :: InputDescription) Core.. Lens.mapping Lens._Coerce

-- | Input ID associated with the application input. This is the ID that
-- Amazon Kinesis Analytics assigns to each input configuration you add to
-- your application.
inputDescription_inputId :: Lens.Lens' InputDescription (Core.Maybe Core.Text)
inputDescription_inputId = Lens.lens (\InputDescription' {inputId} -> inputId) (\s@InputDescription' {} a -> s {inputId = a} :: InputDescription)

instance Core.FromJSON InputDescription where
  parseJSON =
    Core.withObject
      "InputDescription"
      ( \x ->
          InputDescription'
            Core.<$> (x Core..:? "InputSchema")
            Core.<*> (x Core..:? "InputStartingPositionConfiguration")
            Core.<*> ( x
                         Core..:? "InputProcessingConfigurationDescription"
                     )
            Core.<*> (x Core..:? "InputParallelism")
            Core.<*> (x Core..:? "NamePrefix")
            Core.<*> (x Core..:? "KinesisStreamsInputDescription")
            Core.<*> (x Core..:? "KinesisFirehoseInputDescription")
            Core.<*> (x Core..:? "InAppStreamNames" Core..!= Core.mempty)
            Core.<*> (x Core..:? "InputId")
      )

instance Core.Hashable InputDescription

instance Core.NFData InputDescription
