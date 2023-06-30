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
-- Module      : Amazonka.KinesisAnalytics.Types.InputDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.InputDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalytics.Types.InputParallelism
import Amazonka.KinesisAnalytics.Types.InputProcessingConfigurationDescription
import Amazonka.KinesisAnalytics.Types.InputStartingPositionConfiguration
import Amazonka.KinesisAnalytics.Types.KinesisFirehoseInputDescription
import Amazonka.KinesisAnalytics.Types.KinesisStreamsInputDescription
import Amazonka.KinesisAnalytics.Types.SourceSchema
import qualified Amazonka.Prelude as Prelude

-- | Describes the application input configuration. For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
--
-- /See:/ 'newInputDescription' smart constructor.
data InputDescription = InputDescription'
  { -- | Returns the in-application stream names that are mapped to the stream
    -- source.
    inAppStreamNames :: Prelude.Maybe [Prelude.Text],
    -- | Input ID associated with the application input. This is the ID that
    -- Amazon Kinesis Analytics assigns to each input configuration you add to
    -- your application.
    inputId :: Prelude.Maybe Prelude.Text,
    -- | Describes the configured parallelism (number of in-application streams
    -- mapped to the streaming source).
    inputParallelism :: Prelude.Maybe InputParallelism,
    -- | The description of the preprocessor that executes on records in this
    -- input before the application\'s code is run.
    inputProcessingConfigurationDescription :: Prelude.Maybe InputProcessingConfigurationDescription,
    -- | Describes the format of the data in the streaming source, and how each
    -- data element maps to corresponding columns in the in-application stream
    -- that is being created.
    inputSchema :: Prelude.Maybe SourceSchema,
    -- | Point at which the application is configured to read from the input
    -- stream.
    inputStartingPositionConfiguration :: Prelude.Maybe InputStartingPositionConfiguration,
    -- | If an Amazon Kinesis Firehose delivery stream is configured as a
    -- streaming source, provides the delivery stream\'s ARN and an IAM role
    -- that enables Amazon Kinesis Analytics to access the stream on your
    -- behalf.
    kinesisFirehoseInputDescription :: Prelude.Maybe KinesisFirehoseInputDescription,
    -- | If an Amazon Kinesis stream is configured as streaming source, provides
    -- Amazon Kinesis stream\'s Amazon Resource Name (ARN) and an IAM role that
    -- enables Amazon Kinesis Analytics to access the stream on your behalf.
    kinesisStreamsInputDescription :: Prelude.Maybe KinesisStreamsInputDescription,
    -- | In-application name prefix.
    namePrefix :: Prelude.Maybe Prelude.Text
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
-- 'inAppStreamNames', 'inputDescription_inAppStreamNames' - Returns the in-application stream names that are mapped to the stream
-- source.
--
-- 'inputId', 'inputDescription_inputId' - Input ID associated with the application input. This is the ID that
-- Amazon Kinesis Analytics assigns to each input configuration you add to
-- your application.
--
-- 'inputParallelism', 'inputDescription_inputParallelism' - Describes the configured parallelism (number of in-application streams
-- mapped to the streaming source).
--
-- 'inputProcessingConfigurationDescription', 'inputDescription_inputProcessingConfigurationDescription' - The description of the preprocessor that executes on records in this
-- input before the application\'s code is run.
--
-- 'inputSchema', 'inputDescription_inputSchema' - Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns in the in-application stream
-- that is being created.
--
-- 'inputStartingPositionConfiguration', 'inputDescription_inputStartingPositionConfiguration' - Point at which the application is configured to read from the input
-- stream.
--
-- 'kinesisFirehoseInputDescription', 'inputDescription_kinesisFirehoseInputDescription' - If an Amazon Kinesis Firehose delivery stream is configured as a
-- streaming source, provides the delivery stream\'s ARN and an IAM role
-- that enables Amazon Kinesis Analytics to access the stream on your
-- behalf.
--
-- 'kinesisStreamsInputDescription', 'inputDescription_kinesisStreamsInputDescription' - If an Amazon Kinesis stream is configured as streaming source, provides
-- Amazon Kinesis stream\'s Amazon Resource Name (ARN) and an IAM role that
-- enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- 'namePrefix', 'inputDescription_namePrefix' - In-application name prefix.
newInputDescription ::
  InputDescription
newInputDescription =
  InputDescription'
    { inAppStreamNames =
        Prelude.Nothing,
      inputId = Prelude.Nothing,
      inputParallelism = Prelude.Nothing,
      inputProcessingConfigurationDescription =
        Prelude.Nothing,
      inputSchema = Prelude.Nothing,
      inputStartingPositionConfiguration = Prelude.Nothing,
      kinesisFirehoseInputDescription = Prelude.Nothing,
      kinesisStreamsInputDescription = Prelude.Nothing,
      namePrefix = Prelude.Nothing
    }

-- | Returns the in-application stream names that are mapped to the stream
-- source.
inputDescription_inAppStreamNames :: Lens.Lens' InputDescription (Prelude.Maybe [Prelude.Text])
inputDescription_inAppStreamNames = Lens.lens (\InputDescription' {inAppStreamNames} -> inAppStreamNames) (\s@InputDescription' {} a -> s {inAppStreamNames = a} :: InputDescription) Prelude.. Lens.mapping Lens.coerced

-- | Input ID associated with the application input. This is the ID that
-- Amazon Kinesis Analytics assigns to each input configuration you add to
-- your application.
inputDescription_inputId :: Lens.Lens' InputDescription (Prelude.Maybe Prelude.Text)
inputDescription_inputId = Lens.lens (\InputDescription' {inputId} -> inputId) (\s@InputDescription' {} a -> s {inputId = a} :: InputDescription)

-- | Describes the configured parallelism (number of in-application streams
-- mapped to the streaming source).
inputDescription_inputParallelism :: Lens.Lens' InputDescription (Prelude.Maybe InputParallelism)
inputDescription_inputParallelism = Lens.lens (\InputDescription' {inputParallelism} -> inputParallelism) (\s@InputDescription' {} a -> s {inputParallelism = a} :: InputDescription)

-- | The description of the preprocessor that executes on records in this
-- input before the application\'s code is run.
inputDescription_inputProcessingConfigurationDescription :: Lens.Lens' InputDescription (Prelude.Maybe InputProcessingConfigurationDescription)
inputDescription_inputProcessingConfigurationDescription = Lens.lens (\InputDescription' {inputProcessingConfigurationDescription} -> inputProcessingConfigurationDescription) (\s@InputDescription' {} a -> s {inputProcessingConfigurationDescription = a} :: InputDescription)

-- | Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns in the in-application stream
-- that is being created.
inputDescription_inputSchema :: Lens.Lens' InputDescription (Prelude.Maybe SourceSchema)
inputDescription_inputSchema = Lens.lens (\InputDescription' {inputSchema} -> inputSchema) (\s@InputDescription' {} a -> s {inputSchema = a} :: InputDescription)

-- | Point at which the application is configured to read from the input
-- stream.
inputDescription_inputStartingPositionConfiguration :: Lens.Lens' InputDescription (Prelude.Maybe InputStartingPositionConfiguration)
inputDescription_inputStartingPositionConfiguration = Lens.lens (\InputDescription' {inputStartingPositionConfiguration} -> inputStartingPositionConfiguration) (\s@InputDescription' {} a -> s {inputStartingPositionConfiguration = a} :: InputDescription)

-- | If an Amazon Kinesis Firehose delivery stream is configured as a
-- streaming source, provides the delivery stream\'s ARN and an IAM role
-- that enables Amazon Kinesis Analytics to access the stream on your
-- behalf.
inputDescription_kinesisFirehoseInputDescription :: Lens.Lens' InputDescription (Prelude.Maybe KinesisFirehoseInputDescription)
inputDescription_kinesisFirehoseInputDescription = Lens.lens (\InputDescription' {kinesisFirehoseInputDescription} -> kinesisFirehoseInputDescription) (\s@InputDescription' {} a -> s {kinesisFirehoseInputDescription = a} :: InputDescription)

-- | If an Amazon Kinesis stream is configured as streaming source, provides
-- Amazon Kinesis stream\'s Amazon Resource Name (ARN) and an IAM role that
-- enables Amazon Kinesis Analytics to access the stream on your behalf.
inputDescription_kinesisStreamsInputDescription :: Lens.Lens' InputDescription (Prelude.Maybe KinesisStreamsInputDescription)
inputDescription_kinesisStreamsInputDescription = Lens.lens (\InputDescription' {kinesisStreamsInputDescription} -> kinesisStreamsInputDescription) (\s@InputDescription' {} a -> s {kinesisStreamsInputDescription = a} :: InputDescription)

-- | In-application name prefix.
inputDescription_namePrefix :: Lens.Lens' InputDescription (Prelude.Maybe Prelude.Text)
inputDescription_namePrefix = Lens.lens (\InputDescription' {namePrefix} -> namePrefix) (\s@InputDescription' {} a -> s {namePrefix = a} :: InputDescription)

instance Data.FromJSON InputDescription where
  parseJSON =
    Data.withObject
      "InputDescription"
      ( \x ->
          InputDescription'
            Prelude.<$> ( x
                            Data..:? "InAppStreamNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "InputId")
            Prelude.<*> (x Data..:? "InputParallelism")
            Prelude.<*> ( x
                            Data..:? "InputProcessingConfigurationDescription"
                        )
            Prelude.<*> (x Data..:? "InputSchema")
            Prelude.<*> (x Data..:? "InputStartingPositionConfiguration")
            Prelude.<*> (x Data..:? "KinesisFirehoseInputDescription")
            Prelude.<*> (x Data..:? "KinesisStreamsInputDescription")
            Prelude.<*> (x Data..:? "NamePrefix")
      )

instance Prelude.Hashable InputDescription where
  hashWithSalt _salt InputDescription' {..} =
    _salt
      `Prelude.hashWithSalt` inAppStreamNames
      `Prelude.hashWithSalt` inputId
      `Prelude.hashWithSalt` inputParallelism
      `Prelude.hashWithSalt` inputProcessingConfigurationDescription
      `Prelude.hashWithSalt` inputSchema
      `Prelude.hashWithSalt` inputStartingPositionConfiguration
      `Prelude.hashWithSalt` kinesisFirehoseInputDescription
      `Prelude.hashWithSalt` kinesisStreamsInputDescription
      `Prelude.hashWithSalt` namePrefix

instance Prelude.NFData InputDescription where
  rnf InputDescription' {..} =
    Prelude.rnf inAppStreamNames
      `Prelude.seq` Prelude.rnf inputId
      `Prelude.seq` Prelude.rnf inputParallelism
      `Prelude.seq` Prelude.rnf inputProcessingConfigurationDescription
      `Prelude.seq` Prelude.rnf inputSchema
      `Prelude.seq` Prelude.rnf inputStartingPositionConfiguration
      `Prelude.seq` Prelude.rnf kinesisFirehoseInputDescription
      `Prelude.seq` Prelude.rnf kinesisStreamsInputDescription
      `Prelude.seq` Prelude.rnf namePrefix
