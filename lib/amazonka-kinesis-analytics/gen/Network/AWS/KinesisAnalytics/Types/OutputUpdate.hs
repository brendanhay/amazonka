{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ouOutputId,
    ouDestinationSchemaUpdate,
    ouKinesisFirehoseOutputUpdate,
    ouKinesisStreamsOutputUpdate,
    ouLambdaOutputUpdate,
    ouNameUpdate,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types.DestinationSchema as Types
import qualified Network.AWS.KinesisAnalytics.Types.Id as Types
import qualified Network.AWS.KinesisAnalytics.Types.InAppStreamName as Types
import qualified Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate as Types
import qualified Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputUpdate as Types
import qualified Network.AWS.KinesisAnalytics.Types.LambdaOutputUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes updates to the output configuration identified by the @OutputId@ .
--
-- /See:/ 'mkOutputUpdate' smart constructor.
data OutputUpdate = OutputUpdate'
  { -- | Identifies the specific output configuration that you want to update.
    outputId :: Types.Id,
    -- | Describes the data format when records are written to the destination. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
    destinationSchemaUpdate :: Core.Maybe Types.DestinationSchema,
    -- | Describes an Amazon Kinesis Firehose delivery stream as the destination for the output.
    kinesisFirehoseOutputUpdate :: Core.Maybe Types.KinesisFirehoseOutputUpdate,
    -- | Describes an Amazon Kinesis stream as the destination for the output.
    kinesisStreamsOutputUpdate :: Core.Maybe Types.KinesisStreamsOutputUpdate,
    -- | Describes an AWS Lambda function as the destination for the output.
    lambdaOutputUpdate :: Core.Maybe Types.LambdaOutputUpdate,
    -- | If you want to specify a different in-application stream for this output configuration, use this field to specify the new in-application stream name.
    nameUpdate :: Core.Maybe Types.InAppStreamName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputUpdate' value with any optional fields omitted.
mkOutputUpdate ::
  -- | 'outputId'
  Types.Id ->
  OutputUpdate
mkOutputUpdate outputId =
  OutputUpdate'
    { outputId,
      destinationSchemaUpdate = Core.Nothing,
      kinesisFirehoseOutputUpdate = Core.Nothing,
      kinesisStreamsOutputUpdate = Core.Nothing,
      lambdaOutputUpdate = Core.Nothing,
      nameUpdate = Core.Nothing
    }

-- | Identifies the specific output configuration that you want to update.
--
-- /Note:/ Consider using 'outputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouOutputId :: Lens.Lens' OutputUpdate Types.Id
ouOutputId = Lens.field @"outputId"
{-# DEPRECATED ouOutputId "Use generic-lens or generic-optics with 'outputId' instead." #-}

-- | Describes the data format when records are written to the destination. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
--
-- /Note:/ Consider using 'destinationSchemaUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouDestinationSchemaUpdate :: Lens.Lens' OutputUpdate (Core.Maybe Types.DestinationSchema)
ouDestinationSchemaUpdate = Lens.field @"destinationSchemaUpdate"
{-# DEPRECATED ouDestinationSchemaUpdate "Use generic-lens or generic-optics with 'destinationSchemaUpdate' instead." #-}

-- | Describes an Amazon Kinesis Firehose delivery stream as the destination for the output.
--
-- /Note:/ Consider using 'kinesisFirehoseOutputUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouKinesisFirehoseOutputUpdate :: Lens.Lens' OutputUpdate (Core.Maybe Types.KinesisFirehoseOutputUpdate)
ouKinesisFirehoseOutputUpdate = Lens.field @"kinesisFirehoseOutputUpdate"
{-# DEPRECATED ouKinesisFirehoseOutputUpdate "Use generic-lens or generic-optics with 'kinesisFirehoseOutputUpdate' instead." #-}

-- | Describes an Amazon Kinesis stream as the destination for the output.
--
-- /Note:/ Consider using 'kinesisStreamsOutputUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouKinesisStreamsOutputUpdate :: Lens.Lens' OutputUpdate (Core.Maybe Types.KinesisStreamsOutputUpdate)
ouKinesisStreamsOutputUpdate = Lens.field @"kinesisStreamsOutputUpdate"
{-# DEPRECATED ouKinesisStreamsOutputUpdate "Use generic-lens or generic-optics with 'kinesisStreamsOutputUpdate' instead." #-}

-- | Describes an AWS Lambda function as the destination for the output.
--
-- /Note:/ Consider using 'lambdaOutputUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouLambdaOutputUpdate :: Lens.Lens' OutputUpdate (Core.Maybe Types.LambdaOutputUpdate)
ouLambdaOutputUpdate = Lens.field @"lambdaOutputUpdate"
{-# DEPRECATED ouLambdaOutputUpdate "Use generic-lens or generic-optics with 'lambdaOutputUpdate' instead." #-}

-- | If you want to specify a different in-application stream for this output configuration, use this field to specify the new in-application stream name.
--
-- /Note:/ Consider using 'nameUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ouNameUpdate :: Lens.Lens' OutputUpdate (Core.Maybe Types.InAppStreamName)
ouNameUpdate = Lens.field @"nameUpdate"
{-# DEPRECATED ouNameUpdate "Use generic-lens or generic-optics with 'nameUpdate' instead." #-}

instance Core.FromJSON OutputUpdate where
  toJSON OutputUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OutputId" Core..= outputId),
            ("DestinationSchemaUpdate" Core..=)
              Core.<$> destinationSchemaUpdate,
            ("KinesisFirehoseOutputUpdate" Core..=)
              Core.<$> kinesisFirehoseOutputUpdate,
            ("KinesisStreamsOutputUpdate" Core..=)
              Core.<$> kinesisStreamsOutputUpdate,
            ("LambdaOutputUpdate" Core..=) Core.<$> lambdaOutputUpdate,
            ("NameUpdate" Core..=) Core.<$> nameUpdate
          ]
      )
