{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.InputDescription
  ( InputDescription (..)
  -- * Smart constructor
  , mkInputDescription
  -- * Lenses
  , idInAppStreamNames
  , idInputId
  , idInputParallelism
  , idInputProcessingConfigurationDescription
  , idInputSchema
  , idInputStartingPositionConfiguration
  , idKinesisFirehoseInputDescription
  , idKinesisStreamsInputDescription
  , idNamePrefix
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.Id as Types
import qualified Network.AWS.KinesisAnalytics.Types.InAppStreamName as Types
import qualified Network.AWS.KinesisAnalytics.Types.InputParallelism as Types
import qualified Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription as Types
import qualified Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration as Types
import qualified Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription as Types
import qualified Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription as Types
import qualified Network.AWS.KinesisAnalytics.Types.SourceSchema as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the application input configuration. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> . 
--
-- /See:/ 'mkInputDescription' smart constructor.
data InputDescription = InputDescription'
  { inAppStreamNames :: Core.Maybe [Types.InAppStreamName]
    -- ^ Returns the in-application stream names that are mapped to the stream source.
  , inputId :: Core.Maybe Types.Id
    -- ^ Input ID associated with the application input. This is the ID that Amazon Kinesis Analytics assigns to each input configuration you add to your application. 
  , inputParallelism :: Core.Maybe Types.InputParallelism
    -- ^ Describes the configured parallelism (number of in-application streams mapped to the streaming source).
  , inputProcessingConfigurationDescription :: Core.Maybe Types.InputProcessingConfigurationDescription
    -- ^ The description of the preprocessor that executes on records in this input before the application's code is run.
  , inputSchema :: Core.Maybe Types.SourceSchema
    -- ^ Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created. 
  , inputStartingPositionConfiguration :: Core.Maybe Types.InputStartingPositionConfiguration
    -- ^ Point at which the application is configured to read from the input stream.
  , kinesisFirehoseInputDescription :: Core.Maybe Types.KinesisFirehoseInputDescription
    -- ^ If an Amazon Kinesis Firehose delivery stream is configured as a streaming source, provides the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
  , kinesisStreamsInputDescription :: Core.Maybe Types.KinesisStreamsInputDescription
    -- ^ If an Amazon Kinesis stream is configured as streaming source, provides Amazon Kinesis stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
  , namePrefix :: Core.Maybe Types.InAppStreamName
    -- ^ In-application name prefix.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputDescription' value with any optional fields omitted.
mkInputDescription
    :: InputDescription
mkInputDescription
  = InputDescription'{inAppStreamNames = Core.Nothing,
                      inputId = Core.Nothing, inputParallelism = Core.Nothing,
                      inputProcessingConfigurationDescription = Core.Nothing,
                      inputSchema = Core.Nothing,
                      inputStartingPositionConfiguration = Core.Nothing,
                      kinesisFirehoseInputDescription = Core.Nothing,
                      kinesisStreamsInputDescription = Core.Nothing,
                      namePrefix = Core.Nothing}

-- | Returns the in-application stream names that are mapped to the stream source.
--
-- /Note:/ Consider using 'inAppStreamNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInAppStreamNames :: Lens.Lens' InputDescription (Core.Maybe [Types.InAppStreamName])
idInAppStreamNames = Lens.field @"inAppStreamNames"
{-# INLINEABLE idInAppStreamNames #-}
{-# DEPRECATED inAppStreamNames "Use generic-lens or generic-optics with 'inAppStreamNames' instead"  #-}

-- | Input ID associated with the application input. This is the ID that Amazon Kinesis Analytics assigns to each input configuration you add to your application. 
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInputId :: Lens.Lens' InputDescription (Core.Maybe Types.Id)
idInputId = Lens.field @"inputId"
{-# INLINEABLE idInputId #-}
{-# DEPRECATED inputId "Use generic-lens or generic-optics with 'inputId' instead"  #-}

-- | Describes the configured parallelism (number of in-application streams mapped to the streaming source).
--
-- /Note:/ Consider using 'inputParallelism' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInputParallelism :: Lens.Lens' InputDescription (Core.Maybe Types.InputParallelism)
idInputParallelism = Lens.field @"inputParallelism"
{-# INLINEABLE idInputParallelism #-}
{-# DEPRECATED inputParallelism "Use generic-lens or generic-optics with 'inputParallelism' instead"  #-}

-- | The description of the preprocessor that executes on records in this input before the application's code is run.
--
-- /Note:/ Consider using 'inputProcessingConfigurationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInputProcessingConfigurationDescription :: Lens.Lens' InputDescription (Core.Maybe Types.InputProcessingConfigurationDescription)
idInputProcessingConfigurationDescription = Lens.field @"inputProcessingConfigurationDescription"
{-# INLINEABLE idInputProcessingConfigurationDescription #-}
{-# DEPRECATED inputProcessingConfigurationDescription "Use generic-lens or generic-optics with 'inputProcessingConfigurationDescription' instead"  #-}

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created. 
--
-- /Note:/ Consider using 'inputSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInputSchema :: Lens.Lens' InputDescription (Core.Maybe Types.SourceSchema)
idInputSchema = Lens.field @"inputSchema"
{-# INLINEABLE idInputSchema #-}
{-# DEPRECATED inputSchema "Use generic-lens or generic-optics with 'inputSchema' instead"  #-}

-- | Point at which the application is configured to read from the input stream.
--
-- /Note:/ Consider using 'inputStartingPositionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInputStartingPositionConfiguration :: Lens.Lens' InputDescription (Core.Maybe Types.InputStartingPositionConfiguration)
idInputStartingPositionConfiguration = Lens.field @"inputStartingPositionConfiguration"
{-# INLINEABLE idInputStartingPositionConfiguration #-}
{-# DEPRECATED inputStartingPositionConfiguration "Use generic-lens or generic-optics with 'inputStartingPositionConfiguration' instead"  #-}

-- | If an Amazon Kinesis Firehose delivery stream is configured as a streaming source, provides the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- /Note:/ Consider using 'kinesisFirehoseInputDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idKinesisFirehoseInputDescription :: Lens.Lens' InputDescription (Core.Maybe Types.KinesisFirehoseInputDescription)
idKinesisFirehoseInputDescription = Lens.field @"kinesisFirehoseInputDescription"
{-# INLINEABLE idKinesisFirehoseInputDescription #-}
{-# DEPRECATED kinesisFirehoseInputDescription "Use generic-lens or generic-optics with 'kinesisFirehoseInputDescription' instead"  #-}

-- | If an Amazon Kinesis stream is configured as streaming source, provides Amazon Kinesis stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- /Note:/ Consider using 'kinesisStreamsInputDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idKinesisStreamsInputDescription :: Lens.Lens' InputDescription (Core.Maybe Types.KinesisStreamsInputDescription)
idKinesisStreamsInputDescription = Lens.field @"kinesisStreamsInputDescription"
{-# INLINEABLE idKinesisStreamsInputDescription #-}
{-# DEPRECATED kinesisStreamsInputDescription "Use generic-lens or generic-optics with 'kinesisStreamsInputDescription' instead"  #-}

-- | In-application name prefix.
--
-- /Note:/ Consider using 'namePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idNamePrefix :: Lens.Lens' InputDescription (Core.Maybe Types.InAppStreamName)
idNamePrefix = Lens.field @"namePrefix"
{-# INLINEABLE idNamePrefix #-}
{-# DEPRECATED namePrefix "Use generic-lens or generic-optics with 'namePrefix' instead"  #-}

instance Core.FromJSON InputDescription where
        parseJSON
          = Core.withObject "InputDescription" Core.$
              \ x ->
                InputDescription' Core.<$>
                  (x Core..:? "InAppStreamNames") Core.<*> x Core..:? "InputId"
                    Core.<*> x Core..:? "InputParallelism"
                    Core.<*> x Core..:? "InputProcessingConfigurationDescription"
                    Core.<*> x Core..:? "InputSchema"
                    Core.<*> x Core..:? "InputStartingPositionConfiguration"
                    Core.<*> x Core..:? "KinesisFirehoseInputDescription"
                    Core.<*> x Core..:? "KinesisStreamsInputDescription"
                    Core.<*> x Core..:? "NamePrefix"
