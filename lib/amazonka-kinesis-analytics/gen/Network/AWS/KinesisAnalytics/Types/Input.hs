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
    iInputParallelism,
    iInputProcessingConfiguration,
    iKinesisStreamsInput,
    iKinesisFirehoseInput,
    iNamePrefix,
    iInputSchema,
  )
where

import Network.AWS.KinesisAnalytics.Types.InputParallelism
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfiguration
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInput
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When you configure the application input, you specify the streaming source, the in-application stream name that is created, and the mapping between the two. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
-- /See:/ 'mkInput' smart constructor.
data Input = Input'
  { inputParallelism ::
      Lude.Maybe InputParallelism,
    inputProcessingConfiguration ::
      Lude.Maybe InputProcessingConfiguration,
    kinesisStreamsInput :: Lude.Maybe KinesisStreamsInput,
    kinesisFirehoseInput :: Lude.Maybe KinesisFirehoseInput,
    namePrefix :: Lude.Text,
    inputSchema :: SourceSchema
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Input' with the minimum fields required to make a request.
--
-- * 'inputParallelism' - Describes the number of in-application streams to create.
--
-- Data from your source is routed to these in-application input streams.
-- (see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
-- * 'inputProcessingConfiguration' - The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> for the input. An input processor transforms records as they are received from the stream, before the application's SQL code executes. Currently, the only input processing configuration available is <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> .
-- * 'inputSchema' - Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created.
--
-- Also used to describe the format of the reference data source.
-- * 'kinesisFirehoseInput' - If the streaming source is an Amazon Kinesis Firehose delivery stream, identifies the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
-- * 'kinesisStreamsInput' - If the streaming source is an Amazon Kinesis stream, identifies the stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
-- * 'namePrefix' - Name prefix to use when creating an in-application stream. Suppose that you specify a prefix "MyInApplicationStream." Amazon Kinesis Analytics then creates one or more (as per the @InputParallelism@ count you specified) in-application streams with names "MyInApplicationStream_001," "MyInApplicationStream_002," and so on.
mkInput ::
  -- | 'namePrefix'
  Lude.Text ->
  -- | 'inputSchema'
  SourceSchema ->
  Input
mkInput pNamePrefix_ pInputSchema_ =
  Input'
    { inputParallelism = Lude.Nothing,
      inputProcessingConfiguration = Lude.Nothing,
      kinesisStreamsInput = Lude.Nothing,
      kinesisFirehoseInput = Lude.Nothing,
      namePrefix = pNamePrefix_,
      inputSchema = pInputSchema_
    }

-- | Describes the number of in-application streams to create.
--
-- Data from your source is routed to these in-application input streams.
-- (see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
-- /Note:/ Consider using 'inputParallelism' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInputParallelism :: Lens.Lens' Input (Lude.Maybe InputParallelism)
iInputParallelism = Lens.lens (inputParallelism :: Input -> Lude.Maybe InputParallelism) (\s a -> s {inputParallelism = a} :: Input)
{-# DEPRECATED iInputParallelism "Use generic-lens or generic-optics with 'inputParallelism' instead." #-}

-- | The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> for the input. An input processor transforms records as they are received from the stream, before the application's SQL code executes. Currently, the only input processing configuration available is <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> .
--
-- /Note:/ Consider using 'inputProcessingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInputProcessingConfiguration :: Lens.Lens' Input (Lude.Maybe InputProcessingConfiguration)
iInputProcessingConfiguration = Lens.lens (inputProcessingConfiguration :: Input -> Lude.Maybe InputProcessingConfiguration) (\s a -> s {inputProcessingConfiguration = a} :: Input)
{-# DEPRECATED iInputProcessingConfiguration "Use generic-lens or generic-optics with 'inputProcessingConfiguration' instead." #-}

-- | If the streaming source is an Amazon Kinesis stream, identifies the stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
--
-- /Note:/ Consider using 'kinesisStreamsInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iKinesisStreamsInput :: Lens.Lens' Input (Lude.Maybe KinesisStreamsInput)
iKinesisStreamsInput = Lens.lens (kinesisStreamsInput :: Input -> Lude.Maybe KinesisStreamsInput) (\s a -> s {kinesisStreamsInput = a} :: Input)
{-# DEPRECATED iKinesisStreamsInput "Use generic-lens or generic-optics with 'kinesisStreamsInput' instead." #-}

-- | If the streaming source is an Amazon Kinesis Firehose delivery stream, identifies the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
--
-- /Note:/ Consider using 'kinesisFirehoseInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iKinesisFirehoseInput :: Lens.Lens' Input (Lude.Maybe KinesisFirehoseInput)
iKinesisFirehoseInput = Lens.lens (kinesisFirehoseInput :: Input -> Lude.Maybe KinesisFirehoseInput) (\s a -> s {kinesisFirehoseInput = a} :: Input)
{-# DEPRECATED iKinesisFirehoseInput "Use generic-lens or generic-optics with 'kinesisFirehoseInput' instead." #-}

-- | Name prefix to use when creating an in-application stream. Suppose that you specify a prefix "MyInApplicationStream." Amazon Kinesis Analytics then creates one or more (as per the @InputParallelism@ count you specified) in-application streams with names "MyInApplicationStream_001," "MyInApplicationStream_002," and so on.
--
-- /Note:/ Consider using 'namePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iNamePrefix :: Lens.Lens' Input Lude.Text
iNamePrefix = Lens.lens (namePrefix :: Input -> Lude.Text) (\s a -> s {namePrefix = a} :: Input)
{-# DEPRECATED iNamePrefix "Use generic-lens or generic-optics with 'namePrefix' instead." #-}

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created.
--
-- Also used to describe the format of the reference data source.
--
-- /Note:/ Consider using 'inputSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInputSchema :: Lens.Lens' Input SourceSchema
iInputSchema = Lens.lens (inputSchema :: Input -> SourceSchema) (\s a -> s {inputSchema = a} :: Input)
{-# DEPRECATED iInputSchema "Use generic-lens or generic-optics with 'inputSchema' instead." #-}

instance Lude.ToJSON Input where
  toJSON Input' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InputParallelism" Lude..=) Lude.<$> inputParallelism,
            ("InputProcessingConfiguration" Lude..=)
              Lude.<$> inputProcessingConfiguration,
            ("KinesisStreamsInput" Lude..=) Lude.<$> kinesisStreamsInput,
            ("KinesisFirehoseInput" Lude..=) Lude.<$> kinesisFirehoseInput,
            Lude.Just ("NamePrefix" Lude..= namePrefix),
            Lude.Just ("InputSchema" Lude..= inputSchema)
          ]
      )
