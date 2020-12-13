{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.OutputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.OutputDescription
  ( OutputDescription (..),

    -- * Smart constructor
    mkOutputDescription,

    -- * Lenses
    odOutputId,
    odDestinationSchema,
    odKinesisFirehoseOutputDescription,
    odKinesisStreamsOutputDescription,
    odName,
    odLambdaOutputDescription,
  )
where

import Network.AWS.KinesisAnalytics.Types.DestinationSchema
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription
import Network.AWS.KinesisAnalytics.Types.LambdaOutputDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the application output configuration, which includes the in-application stream name and the destination where the stream data is written. The destination can be an Amazon Kinesis stream or an Amazon Kinesis Firehose delivery stream.
--
-- /See:/ 'mkOutputDescription' smart constructor.
data OutputDescription = OutputDescription'
  { -- | A unique identifier for the output configuration.
    outputId :: Lude.Maybe Lude.Text,
    -- | Data format used for writing data to the destination.
    destinationSchema :: Lude.Maybe DestinationSchema,
    -- | Describes the Amazon Kinesis Firehose delivery stream configured as the destination where output is written.
    kinesisFirehoseOutputDescription :: Lude.Maybe KinesisFirehoseOutputDescription,
    -- | Describes Amazon Kinesis stream configured as the destination where output is written.
    kinesisStreamsOutputDescription :: Lude.Maybe KinesisStreamsOutputDescription,
    -- | Name of the in-application stream configured as output.
    name :: Lude.Maybe Lude.Text,
    -- | Describes the AWS Lambda function configured as the destination where output is written.
    lambdaOutputDescription :: Lude.Maybe LambdaOutputDescription
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputDescription' with the minimum fields required to make a request.
--
-- * 'outputId' - A unique identifier for the output configuration.
-- * 'destinationSchema' - Data format used for writing data to the destination.
-- * 'kinesisFirehoseOutputDescription' - Describes the Amazon Kinesis Firehose delivery stream configured as the destination where output is written.
-- * 'kinesisStreamsOutputDescription' - Describes Amazon Kinesis stream configured as the destination where output is written.
-- * 'name' - Name of the in-application stream configured as output.
-- * 'lambdaOutputDescription' - Describes the AWS Lambda function configured as the destination where output is written.
mkOutputDescription ::
  OutputDescription
mkOutputDescription =
  OutputDescription'
    { outputId = Lude.Nothing,
      destinationSchema = Lude.Nothing,
      kinesisFirehoseOutputDescription = Lude.Nothing,
      kinesisStreamsOutputDescription = Lude.Nothing,
      name = Lude.Nothing,
      lambdaOutputDescription = Lude.Nothing
    }

-- | A unique identifier for the output configuration.
--
-- /Note:/ Consider using 'outputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odOutputId :: Lens.Lens' OutputDescription (Lude.Maybe Lude.Text)
odOutputId = Lens.lens (outputId :: OutputDescription -> Lude.Maybe Lude.Text) (\s a -> s {outputId = a} :: OutputDescription)
{-# DEPRECATED odOutputId "Use generic-lens or generic-optics with 'outputId' instead." #-}

-- | Data format used for writing data to the destination.
--
-- /Note:/ Consider using 'destinationSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odDestinationSchema :: Lens.Lens' OutputDescription (Lude.Maybe DestinationSchema)
odDestinationSchema = Lens.lens (destinationSchema :: OutputDescription -> Lude.Maybe DestinationSchema) (\s a -> s {destinationSchema = a} :: OutputDescription)
{-# DEPRECATED odDestinationSchema "Use generic-lens or generic-optics with 'destinationSchema' instead." #-}

-- | Describes the Amazon Kinesis Firehose delivery stream configured as the destination where output is written.
--
-- /Note:/ Consider using 'kinesisFirehoseOutputDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odKinesisFirehoseOutputDescription :: Lens.Lens' OutputDescription (Lude.Maybe KinesisFirehoseOutputDescription)
odKinesisFirehoseOutputDescription = Lens.lens (kinesisFirehoseOutputDescription :: OutputDescription -> Lude.Maybe KinesisFirehoseOutputDescription) (\s a -> s {kinesisFirehoseOutputDescription = a} :: OutputDescription)
{-# DEPRECATED odKinesisFirehoseOutputDescription "Use generic-lens or generic-optics with 'kinesisFirehoseOutputDescription' instead." #-}

-- | Describes Amazon Kinesis stream configured as the destination where output is written.
--
-- /Note:/ Consider using 'kinesisStreamsOutputDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odKinesisStreamsOutputDescription :: Lens.Lens' OutputDescription (Lude.Maybe KinesisStreamsOutputDescription)
odKinesisStreamsOutputDescription = Lens.lens (kinesisStreamsOutputDescription :: OutputDescription -> Lude.Maybe KinesisStreamsOutputDescription) (\s a -> s {kinesisStreamsOutputDescription = a} :: OutputDescription)
{-# DEPRECATED odKinesisStreamsOutputDescription "Use generic-lens or generic-optics with 'kinesisStreamsOutputDescription' instead." #-}

-- | Name of the in-application stream configured as output.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odName :: Lens.Lens' OutputDescription (Lude.Maybe Lude.Text)
odName = Lens.lens (name :: OutputDescription -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: OutputDescription)
{-# DEPRECATED odName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Describes the AWS Lambda function configured as the destination where output is written.
--
-- /Note:/ Consider using 'lambdaOutputDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odLambdaOutputDescription :: Lens.Lens' OutputDescription (Lude.Maybe LambdaOutputDescription)
odLambdaOutputDescription = Lens.lens (lambdaOutputDescription :: OutputDescription -> Lude.Maybe LambdaOutputDescription) (\s a -> s {lambdaOutputDescription = a} :: OutputDescription)
{-# DEPRECATED odLambdaOutputDescription "Use generic-lens or generic-optics with 'lambdaOutputDescription' instead." #-}

instance Lude.FromJSON OutputDescription where
  parseJSON =
    Lude.withObject
      "OutputDescription"
      ( \x ->
          OutputDescription'
            Lude.<$> (x Lude..:? "OutputId")
            Lude.<*> (x Lude..:? "DestinationSchema")
            Lude.<*> (x Lude..:? "KinesisFirehoseOutputDescription")
            Lude.<*> (x Lude..:? "KinesisStreamsOutputDescription")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "LambdaOutputDescription")
      )
