{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputDescription
  ( InputDescription (..),

    -- * Smart constructor
    mkInputDescription,

    -- * Lenses
    idInputStartingPositionConfiguration,
    idInputParallelism,
    idInputId,
    idInAppStreamNames,
    idKinesisFirehoseInputDescription,
    idInputSchema,
    idKinesisStreamsInputDescription,
    idNamePrefix,
    idInputProcessingConfigurationDescription,
  )
where

import Network.AWS.KinesisAnalytics.Types.InputParallelism
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription
import Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the application input configuration. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
-- /See:/ 'mkInputDescription' smart constructor.
data InputDescription = InputDescription'
  { -- | Point at which the application is configured to read from the input stream.
    inputStartingPositionConfiguration :: Lude.Maybe InputStartingPositionConfiguration,
    -- | Describes the configured parallelism (number of in-application streams mapped to the streaming source).
    inputParallelism :: Lude.Maybe InputParallelism,
    -- | Input ID associated with the application input. This is the ID that Amazon Kinesis Analytics assigns to each input configuration you add to your application.
    inputId :: Lude.Maybe Lude.Text,
    -- | Returns the in-application stream names that are mapped to the stream source.
    inAppStreamNames :: Lude.Maybe [Lude.Text],
    -- | If an Amazon Kinesis Firehose delivery stream is configured as a streaming source, provides the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
    kinesisFirehoseInputDescription :: Lude.Maybe KinesisFirehoseInputDescription,
    -- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created.
    inputSchema :: Lude.Maybe SourceSchema,
    -- | If an Amazon Kinesis stream is configured as streaming source, provides Amazon Kinesis stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
    kinesisStreamsInputDescription :: Lude.Maybe KinesisStreamsInputDescription,
    -- | In-application name prefix.
    namePrefix :: Lude.Maybe Lude.Text,
    -- | The description of the preprocessor that executes on records in this input before the application's code is run.
    inputProcessingConfigurationDescription :: Lude.Maybe InputProcessingConfigurationDescription
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputDescription' with the minimum fields required to make a request.
--
-- * 'inputStartingPositionConfiguration' - Point at which the application is configured to read from the input stream.
-- * 'inputParallelism' - Describes the configured parallelism (number of in-application streams mapped to the streaming source).
-- * 'inputId' - Input ID associated with the application input. This is the ID that Amazon Kinesis Analytics assigns to each input configuration you add to your application.
-- * 'inAppStreamNames' - Returns the in-application stream names that are mapped to the stream source.
-- * 'kinesisFirehoseInputDescription' - If an Amazon Kinesis Firehose delivery stream is configured as a streaming source, provides the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
-- * 'inputSchema' - Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created.
-- * 'kinesisStreamsInputDescription' - If an Amazon Kinesis stream is configured as streaming source, provides Amazon Kinesis stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
-- * 'namePrefix' - In-application name prefix.
-- * 'inputProcessingConfigurationDescription' - The description of the preprocessor that executes on records in this input before the application's code is run.
mkInputDescription ::
  InputDescription
mkInputDescription =
  InputDescription'
    { inputStartingPositionConfiguration =
        Lude.Nothing,
      inputParallelism = Lude.Nothing,
      inputId = Lude.Nothing,
      inAppStreamNames = Lude.Nothing,
      kinesisFirehoseInputDescription = Lude.Nothing,
      inputSchema = Lude.Nothing,
      kinesisStreamsInputDescription = Lude.Nothing,
      namePrefix = Lude.Nothing,
      inputProcessingConfigurationDescription = Lude.Nothing
    }

-- | Point at which the application is configured to read from the input stream.
--
-- /Note:/ Consider using 'inputStartingPositionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInputStartingPositionConfiguration :: Lens.Lens' InputDescription (Lude.Maybe InputStartingPositionConfiguration)
idInputStartingPositionConfiguration = Lens.lens (inputStartingPositionConfiguration :: InputDescription -> Lude.Maybe InputStartingPositionConfiguration) (\s a -> s {inputStartingPositionConfiguration = a} :: InputDescription)
{-# DEPRECATED idInputStartingPositionConfiguration "Use generic-lens or generic-optics with 'inputStartingPositionConfiguration' instead." #-}

-- | Describes the configured parallelism (number of in-application streams mapped to the streaming source).
--
-- /Note:/ Consider using 'inputParallelism' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInputParallelism :: Lens.Lens' InputDescription (Lude.Maybe InputParallelism)
idInputParallelism = Lens.lens (inputParallelism :: InputDescription -> Lude.Maybe InputParallelism) (\s a -> s {inputParallelism = a} :: InputDescription)
{-# DEPRECATED idInputParallelism "Use generic-lens or generic-optics with 'inputParallelism' instead." #-}

-- | Input ID associated with the application input. This is the ID that Amazon Kinesis Analytics assigns to each input configuration you add to your application.
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInputId :: Lens.Lens' InputDescription (Lude.Maybe Lude.Text)
idInputId = Lens.lens (inputId :: InputDescription -> Lude.Maybe Lude.Text) (\s a -> s {inputId = a} :: InputDescription)
{-# DEPRECATED idInputId "Use generic-lens or generic-optics with 'inputId' instead." #-}

-- | Returns the in-application stream names that are mapped to the stream source.
--
-- /Note:/ Consider using 'inAppStreamNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInAppStreamNames :: Lens.Lens' InputDescription (Lude.Maybe [Lude.Text])
idInAppStreamNames = Lens.lens (inAppStreamNames :: InputDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {inAppStreamNames = a} :: InputDescription)
{-# DEPRECATED idInAppStreamNames "Use generic-lens or generic-optics with 'inAppStreamNames' instead." #-}

-- | If an Amazon Kinesis Firehose delivery stream is configured as a streaming source, provides the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- /Note:/ Consider using 'kinesisFirehoseInputDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idKinesisFirehoseInputDescription :: Lens.Lens' InputDescription (Lude.Maybe KinesisFirehoseInputDescription)
idKinesisFirehoseInputDescription = Lens.lens (kinesisFirehoseInputDescription :: InputDescription -> Lude.Maybe KinesisFirehoseInputDescription) (\s a -> s {kinesisFirehoseInputDescription = a} :: InputDescription)
{-# DEPRECATED idKinesisFirehoseInputDescription "Use generic-lens or generic-optics with 'kinesisFirehoseInputDescription' instead." #-}

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created.
--
-- /Note:/ Consider using 'inputSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInputSchema :: Lens.Lens' InputDescription (Lude.Maybe SourceSchema)
idInputSchema = Lens.lens (inputSchema :: InputDescription -> Lude.Maybe SourceSchema) (\s a -> s {inputSchema = a} :: InputDescription)
{-# DEPRECATED idInputSchema "Use generic-lens or generic-optics with 'inputSchema' instead." #-}

-- | If an Amazon Kinesis stream is configured as streaming source, provides Amazon Kinesis stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- /Note:/ Consider using 'kinesisStreamsInputDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idKinesisStreamsInputDescription :: Lens.Lens' InputDescription (Lude.Maybe KinesisStreamsInputDescription)
idKinesisStreamsInputDescription = Lens.lens (kinesisStreamsInputDescription :: InputDescription -> Lude.Maybe KinesisStreamsInputDescription) (\s a -> s {kinesisStreamsInputDescription = a} :: InputDescription)
{-# DEPRECATED idKinesisStreamsInputDescription "Use generic-lens or generic-optics with 'kinesisStreamsInputDescription' instead." #-}

-- | In-application name prefix.
--
-- /Note:/ Consider using 'namePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idNamePrefix :: Lens.Lens' InputDescription (Lude.Maybe Lude.Text)
idNamePrefix = Lens.lens (namePrefix :: InputDescription -> Lude.Maybe Lude.Text) (\s a -> s {namePrefix = a} :: InputDescription)
{-# DEPRECATED idNamePrefix "Use generic-lens or generic-optics with 'namePrefix' instead." #-}

-- | The description of the preprocessor that executes on records in this input before the application's code is run.
--
-- /Note:/ Consider using 'inputProcessingConfigurationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInputProcessingConfigurationDescription :: Lens.Lens' InputDescription (Lude.Maybe InputProcessingConfigurationDescription)
idInputProcessingConfigurationDescription = Lens.lens (inputProcessingConfigurationDescription :: InputDescription -> Lude.Maybe InputProcessingConfigurationDescription) (\s a -> s {inputProcessingConfigurationDescription = a} :: InputDescription)
{-# DEPRECATED idInputProcessingConfigurationDescription "Use generic-lens or generic-optics with 'inputProcessingConfigurationDescription' instead." #-}

instance Lude.FromJSON InputDescription where
  parseJSON =
    Lude.withObject
      "InputDescription"
      ( \x ->
          InputDescription'
            Lude.<$> (x Lude..:? "InputStartingPositionConfiguration")
            Lude.<*> (x Lude..:? "InputParallelism")
            Lude.<*> (x Lude..:? "InputId")
            Lude.<*> (x Lude..:? "InAppStreamNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "KinesisFirehoseInputDescription")
            Lude.<*> (x Lude..:? "InputSchema")
            Lude.<*> (x Lude..:? "KinesisStreamsInputDescription")
            Lude.<*> (x Lude..:? "NamePrefix")
            Lude.<*> (x Lude..:? "InputProcessingConfigurationDescription")
      )
