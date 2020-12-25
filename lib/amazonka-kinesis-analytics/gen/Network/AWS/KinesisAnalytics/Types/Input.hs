{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.Input
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.Input
  ( Input (..),

    -- * Smart constructor
    mkInput,

    -- * Lenses
    iNamePrefix,
    iInputSchema,
    iInputParallelism,
    iInputProcessingConfiguration,
    iKinesisFirehoseInput,
    iKinesisStreamsInput,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types.InputParallelism as Types
import qualified Network.AWS.KinesisAnalytics.Types.InputProcessingConfiguration as Types
import qualified Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput as Types
import qualified Network.AWS.KinesisAnalytics.Types.KinesisStreamsInput as Types
import qualified Network.AWS.KinesisAnalytics.Types.NamePrefix as Types
import qualified Network.AWS.KinesisAnalytics.Types.SourceSchema as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | When you configure the application input, you specify the streaming source, the in-application stream name that is created, and the mapping between the two. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
-- /See:/ 'mkInput' smart constructor.
data Input = Input'
  { -- | Name prefix to use when creating an in-application stream. Suppose that you specify a prefix "MyInApplicationStream." Amazon Kinesis Analytics then creates one or more (as per the @InputParallelism@ count you specified) in-application streams with names "MyInApplicationStream_001," "MyInApplicationStream_002," and so on.
    namePrefix :: Types.NamePrefix,
    -- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created.
    --
    -- Also used to describe the format of the reference data source.
    inputSchema :: Types.SourceSchema,
    -- | Describes the number of in-application streams to create.
    --
    -- Data from your source is routed to these in-application input streams.
    -- (see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
    inputParallelism :: Core.Maybe Types.InputParallelism,
    -- | The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> for the input. An input processor transforms records as they are received from the stream, before the application's SQL code executes. Currently, the only input processing configuration available is <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> .
    inputProcessingConfiguration :: Core.Maybe Types.InputProcessingConfiguration,
    -- | If the streaming source is an Amazon Kinesis Firehose delivery stream, identifies the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
    --
    -- Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
    kinesisFirehoseInput :: Core.Maybe Types.KinesisFirehoseInput,
    -- | If the streaming source is an Amazon Kinesis stream, identifies the stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
    --
    -- Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
    kinesisStreamsInput :: Core.Maybe Types.KinesisStreamsInput
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Input' value with any optional fields omitted.
mkInput ::
  -- | 'namePrefix'
  Types.NamePrefix ->
  -- | 'inputSchema'
  Types.SourceSchema ->
  Input
mkInput namePrefix inputSchema =
  Input'
    { namePrefix,
      inputSchema,
      inputParallelism = Core.Nothing,
      inputProcessingConfiguration = Core.Nothing,
      kinesisFirehoseInput = Core.Nothing,
      kinesisStreamsInput = Core.Nothing
    }

-- | Name prefix to use when creating an in-application stream. Suppose that you specify a prefix "MyInApplicationStream." Amazon Kinesis Analytics then creates one or more (as per the @InputParallelism@ count you specified) in-application streams with names "MyInApplicationStream_001," "MyInApplicationStream_002," and so on.
--
-- /Note:/ Consider using 'namePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iNamePrefix :: Lens.Lens' Input Types.NamePrefix
iNamePrefix = Lens.field @"namePrefix"
{-# DEPRECATED iNamePrefix "Use generic-lens or generic-optics with 'namePrefix' instead." #-}

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created.
--
-- Also used to describe the format of the reference data source.
--
-- /Note:/ Consider using 'inputSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInputSchema :: Lens.Lens' Input Types.SourceSchema
iInputSchema = Lens.field @"inputSchema"
{-# DEPRECATED iInputSchema "Use generic-lens or generic-optics with 'inputSchema' instead." #-}

-- | Describes the number of in-application streams to create.
--
-- Data from your source is routed to these in-application input streams.
-- (see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
-- /Note:/ Consider using 'inputParallelism' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInputParallelism :: Lens.Lens' Input (Core.Maybe Types.InputParallelism)
iInputParallelism = Lens.field @"inputParallelism"
{-# DEPRECATED iInputParallelism "Use generic-lens or generic-optics with 'inputParallelism' instead." #-}

-- | The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> for the input. An input processor transforms records as they are received from the stream, before the application's SQL code executes. Currently, the only input processing configuration available is <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> .
--
-- /Note:/ Consider using 'inputProcessingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInputProcessingConfiguration :: Lens.Lens' Input (Core.Maybe Types.InputProcessingConfiguration)
iInputProcessingConfiguration = Lens.field @"inputProcessingConfiguration"
{-# DEPRECATED iInputProcessingConfiguration "Use generic-lens or generic-optics with 'inputProcessingConfiguration' instead." #-}

-- | If the streaming source is an Amazon Kinesis Firehose delivery stream, identifies the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
--
-- /Note:/ Consider using 'kinesisFirehoseInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iKinesisFirehoseInput :: Lens.Lens' Input (Core.Maybe Types.KinesisFirehoseInput)
iKinesisFirehoseInput = Lens.field @"kinesisFirehoseInput"
{-# DEPRECATED iKinesisFirehoseInput "Use generic-lens or generic-optics with 'kinesisFirehoseInput' instead." #-}

-- | If the streaming source is an Amazon Kinesis stream, identifies the stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
--
-- /Note:/ Consider using 'kinesisStreamsInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iKinesisStreamsInput :: Lens.Lens' Input (Core.Maybe Types.KinesisStreamsInput)
iKinesisStreamsInput = Lens.field @"kinesisStreamsInput"
{-# DEPRECATED iKinesisStreamsInput "Use generic-lens or generic-optics with 'kinesisStreamsInput' instead." #-}

instance Core.FromJSON Input where
  toJSON Input {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("NamePrefix" Core..= namePrefix),
            Core.Just ("InputSchema" Core..= inputSchema),
            ("InputParallelism" Core..=) Core.<$> inputParallelism,
            ("InputProcessingConfiguration" Core..=)
              Core.<$> inputProcessingConfiguration,
            ("KinesisFirehoseInput" Core..=) Core.<$> kinesisFirehoseInput,
            ("KinesisStreamsInput" Core..=) Core.<$> kinesisStreamsInput
          ]
      )
