-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.OutputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.OutputUpdate
  ( OutputUpdate (..),

    -- * Smart constructor
    mkOutputUpdate,

    -- * Lenses
    ouKinesisStreamsOutputUpdate,
    ouDestinationSchemaUpdate,
    ouKinesisFirehoseOutputUpdate,
    ouNameUpdate,
    ouLambdaOutputUpdate,
    ouOutputId,
  )
where

import Network.AWS.KinesisAnalytics.Types.DestinationSchema
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
import Network.AWS.KinesisAnalytics.Types.LambdaOutputUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes updates to the output configuration identified by the @OutputId@ .
--
-- /See:/ 'mkOutputUpdate' smart constructor.
data OutputUpdate = OutputUpdate'
  { kinesisStreamsOutputUpdate ::
      Lude.Maybe KinesisStreamsOutputUpdate,
    destinationSchemaUpdate :: Lude.Maybe DestinationSchema,
    kinesisFirehoseOutputUpdate ::
      Lude.Maybe KinesisFirehoseOutputUpdate,
    nameUpdate :: Lude.Maybe Lude.Text,
    lambdaOutputUpdate :: Lude.Maybe LambdaOutputUpdate,
    outputId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputUpdate' with the minimum fields required to make a request.
--
-- * 'destinationSchemaUpdate' - Describes the data format when records are written to the destination. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
-- * 'kinesisFirehoseOutputUpdate' - Describes an Amazon Kinesis Firehose delivery stream as the destination for the output.
-- * 'kinesisStreamsOutputUpdate' - Describes an Amazon Kinesis stream as the destination for the output.
-- * 'lambdaOutputUpdate' - Describes an AWS Lambda function as the destination for the output.
-- * 'nameUpdate' - If you want to specify a different in-application stream for this output configuration, use this field to specify the new in-application stream name.
-- * 'outputId' - Identifies the specific output configuration that you want to update.
mkOutputUpdate ::
  -- | 'outputId'
  Lude.Text ->
  OutputUpdate
mkOutputUpdate pOutputId_ =
  OutputUpdate'
    { kinesisStreamsOutputUpdate = Lude.Nothing,
      destinationSchemaUpdate = Lude.Nothing,
      kinesisFirehoseOutputUpdate = Lude.Nothing,
      nameUpdate = Lude.Nothing,
      lambdaOutputUpdate = Lude.Nothing,
      outputId = pOutputId_
    }

-- | Describes an Amazon Kinesis stream as the destination for the output.
--
-- /Note:/ Consider using 'kinesisStreamsOutputUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouKinesisStreamsOutputUpdate :: Lens.Lens' OutputUpdate (Lude.Maybe KinesisStreamsOutputUpdate)
ouKinesisStreamsOutputUpdate = Lens.lens (kinesisStreamsOutputUpdate :: OutputUpdate -> Lude.Maybe KinesisStreamsOutputUpdate) (\s a -> s {kinesisStreamsOutputUpdate = a} :: OutputUpdate)
{-# DEPRECATED ouKinesisStreamsOutputUpdate "Use generic-lens or generic-optics with 'kinesisStreamsOutputUpdate' instead." #-}

-- | Describes the data format when records are written to the destination. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
--
-- /Note:/ Consider using 'destinationSchemaUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouDestinationSchemaUpdate :: Lens.Lens' OutputUpdate (Lude.Maybe DestinationSchema)
ouDestinationSchemaUpdate = Lens.lens (destinationSchemaUpdate :: OutputUpdate -> Lude.Maybe DestinationSchema) (\s a -> s {destinationSchemaUpdate = a} :: OutputUpdate)
{-# DEPRECATED ouDestinationSchemaUpdate "Use generic-lens or generic-optics with 'destinationSchemaUpdate' instead." #-}

-- | Describes an Amazon Kinesis Firehose delivery stream as the destination for the output.
--
-- /Note:/ Consider using 'kinesisFirehoseOutputUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouKinesisFirehoseOutputUpdate :: Lens.Lens' OutputUpdate (Lude.Maybe KinesisFirehoseOutputUpdate)
ouKinesisFirehoseOutputUpdate = Lens.lens (kinesisFirehoseOutputUpdate :: OutputUpdate -> Lude.Maybe KinesisFirehoseOutputUpdate) (\s a -> s {kinesisFirehoseOutputUpdate = a} :: OutputUpdate)
{-# DEPRECATED ouKinesisFirehoseOutputUpdate "Use generic-lens or generic-optics with 'kinesisFirehoseOutputUpdate' instead." #-}

-- | If you want to specify a different in-application stream for this output configuration, use this field to specify the new in-application stream name.
--
-- /Note:/ Consider using 'nameUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouNameUpdate :: Lens.Lens' OutputUpdate (Lude.Maybe Lude.Text)
ouNameUpdate = Lens.lens (nameUpdate :: OutputUpdate -> Lude.Maybe Lude.Text) (\s a -> s {nameUpdate = a} :: OutputUpdate)
{-# DEPRECATED ouNameUpdate "Use generic-lens or generic-optics with 'nameUpdate' instead." #-}

-- | Describes an AWS Lambda function as the destination for the output.
--
-- /Note:/ Consider using 'lambdaOutputUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouLambdaOutputUpdate :: Lens.Lens' OutputUpdate (Lude.Maybe LambdaOutputUpdate)
ouLambdaOutputUpdate = Lens.lens (lambdaOutputUpdate :: OutputUpdate -> Lude.Maybe LambdaOutputUpdate) (\s a -> s {lambdaOutputUpdate = a} :: OutputUpdate)
{-# DEPRECATED ouLambdaOutputUpdate "Use generic-lens or generic-optics with 'lambdaOutputUpdate' instead." #-}

-- | Identifies the specific output configuration that you want to update.
--
-- /Note:/ Consider using 'outputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouOutputId :: Lens.Lens' OutputUpdate Lude.Text
ouOutputId = Lens.lens (outputId :: OutputUpdate -> Lude.Text) (\s a -> s {outputId = a} :: OutputUpdate)
{-# DEPRECATED ouOutputId "Use generic-lens or generic-optics with 'outputId' instead." #-}

instance Lude.ToJSON OutputUpdate where
  toJSON OutputUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KinesisStreamsOutputUpdate" Lude..=)
              Lude.<$> kinesisStreamsOutputUpdate,
            ("DestinationSchemaUpdate" Lude..=)
              Lude.<$> destinationSchemaUpdate,
            ("KinesisFirehoseOutputUpdate" Lude..=)
              Lude.<$> kinesisFirehoseOutputUpdate,
            ("NameUpdate" Lude..=) Lude.<$> nameUpdate,
            ("LambdaOutputUpdate" Lude..=) Lude.<$> lambdaOutputUpdate,
            Lude.Just ("OutputId" Lude..= outputId)
          ]
      )
