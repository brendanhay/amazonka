{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.OutputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.OutputDescription
  ( OutputDescription (..)
  -- * Smart constructor
  , mkOutputDescription
  -- * Lenses
  , odDestinationSchema
  , odKinesisFirehoseOutputDescription
  , odKinesisStreamsOutputDescription
  , odLambdaOutputDescription
  , odName
  , odOutputId
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.DestinationSchema as Types
import qualified Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription as Types
import qualified Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription as Types
import qualified Network.AWS.KinesisAnalytics.Types.LambdaOutputDescription as Types
import qualified Network.AWS.KinesisAnalytics.Types.Name as Types
import qualified Network.AWS.KinesisAnalytics.Types.OutputId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the application output configuration, which includes the in-application stream name and the destination where the stream data is written. The destination can be an Amazon Kinesis stream or an Amazon Kinesis Firehose delivery stream. 
--
-- /See:/ 'mkOutputDescription' smart constructor.
data OutputDescription = OutputDescription'
  { destinationSchema :: Core.Maybe Types.DestinationSchema
    -- ^ Data format used for writing data to the destination.
  , kinesisFirehoseOutputDescription :: Core.Maybe Types.KinesisFirehoseOutputDescription
    -- ^ Describes the Amazon Kinesis Firehose delivery stream configured as the destination where output is written.
  , kinesisStreamsOutputDescription :: Core.Maybe Types.KinesisStreamsOutputDescription
    -- ^ Describes Amazon Kinesis stream configured as the destination where output is written.
  , lambdaOutputDescription :: Core.Maybe Types.LambdaOutputDescription
    -- ^ Describes the AWS Lambda function configured as the destination where output is written.
  , name :: Core.Maybe Types.Name
    -- ^ Name of the in-application stream configured as output.
  , outputId :: Core.Maybe Types.OutputId
    -- ^ A unique identifier for the output configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputDescription' value with any optional fields omitted.
mkOutputDescription
    :: OutputDescription
mkOutputDescription
  = OutputDescription'{destinationSchema = Core.Nothing,
                       kinesisFirehoseOutputDescription = Core.Nothing,
                       kinesisStreamsOutputDescription = Core.Nothing,
                       lambdaOutputDescription = Core.Nothing, name = Core.Nothing,
                       outputId = Core.Nothing}

-- | Data format used for writing data to the destination.
--
-- /Note:/ Consider using 'destinationSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odDestinationSchema :: Lens.Lens' OutputDescription (Core.Maybe Types.DestinationSchema)
odDestinationSchema = Lens.field @"destinationSchema"
{-# INLINEABLE odDestinationSchema #-}
{-# DEPRECATED destinationSchema "Use generic-lens or generic-optics with 'destinationSchema' instead"  #-}

-- | Describes the Amazon Kinesis Firehose delivery stream configured as the destination where output is written.
--
-- /Note:/ Consider using 'kinesisFirehoseOutputDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odKinesisFirehoseOutputDescription :: Lens.Lens' OutputDescription (Core.Maybe Types.KinesisFirehoseOutputDescription)
odKinesisFirehoseOutputDescription = Lens.field @"kinesisFirehoseOutputDescription"
{-# INLINEABLE odKinesisFirehoseOutputDescription #-}
{-# DEPRECATED kinesisFirehoseOutputDescription "Use generic-lens or generic-optics with 'kinesisFirehoseOutputDescription' instead"  #-}

-- | Describes Amazon Kinesis stream configured as the destination where output is written.
--
-- /Note:/ Consider using 'kinesisStreamsOutputDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odKinesisStreamsOutputDescription :: Lens.Lens' OutputDescription (Core.Maybe Types.KinesisStreamsOutputDescription)
odKinesisStreamsOutputDescription = Lens.field @"kinesisStreamsOutputDescription"
{-# INLINEABLE odKinesisStreamsOutputDescription #-}
{-# DEPRECATED kinesisStreamsOutputDescription "Use generic-lens or generic-optics with 'kinesisStreamsOutputDescription' instead"  #-}

-- | Describes the AWS Lambda function configured as the destination where output is written.
--
-- /Note:/ Consider using 'lambdaOutputDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odLambdaOutputDescription :: Lens.Lens' OutputDescription (Core.Maybe Types.LambdaOutputDescription)
odLambdaOutputDescription = Lens.field @"lambdaOutputDescription"
{-# INLINEABLE odLambdaOutputDescription #-}
{-# DEPRECATED lambdaOutputDescription "Use generic-lens or generic-optics with 'lambdaOutputDescription' instead"  #-}

-- | Name of the in-application stream configured as output.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odName :: Lens.Lens' OutputDescription (Core.Maybe Types.Name)
odName = Lens.field @"name"
{-# INLINEABLE odName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A unique identifier for the output configuration.
--
-- /Note:/ Consider using 'outputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odOutputId :: Lens.Lens' OutputDescription (Core.Maybe Types.OutputId)
odOutputId = Lens.field @"outputId"
{-# INLINEABLE odOutputId #-}
{-# DEPRECATED outputId "Use generic-lens or generic-optics with 'outputId' instead"  #-}

instance Core.FromJSON OutputDescription where
        parseJSON
          = Core.withObject "OutputDescription" Core.$
              \ x ->
                OutputDescription' Core.<$>
                  (x Core..:? "DestinationSchema") Core.<*>
                    x Core..:? "KinesisFirehoseOutputDescription"
                    Core.<*> x Core..:? "KinesisStreamsOutputDescription"
                    Core.<*> x Core..:? "LambdaOutputDescription"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "OutputId"
