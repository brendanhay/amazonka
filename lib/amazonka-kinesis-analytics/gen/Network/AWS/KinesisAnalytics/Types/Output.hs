{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.Output
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.Output
  ( Output (..)
  -- * Smart constructor
  , mkOutput
  -- * Lenses
  , oName
  , oDestinationSchema
  , oKinesisFirehoseOutput
  , oKinesisStreamsOutput
  , oLambdaOutput
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.DestinationSchema as Types
import qualified Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutput as Types
import qualified Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutput as Types
import qualified Network.AWS.KinesisAnalytics.Types.LambdaOutput as Types
import qualified Network.AWS.KinesisAnalytics.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes application output configuration in which you identify an in-application stream and a destination where you want the in-application stream data to be written. The destination can be an Amazon Kinesis stream or an Amazon Kinesis Firehose delivery stream. 
--
--
-- For limits on how many destinations an application can write and other limitations, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> . 
--
-- /See:/ 'mkOutput' smart constructor.
data Output = Output'
  { name :: Types.Name
    -- ^ Name of the in-application stream.
  , destinationSchema :: Types.DestinationSchema
    -- ^ Describes the data format when records are written to the destination. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
  , kinesisFirehoseOutput :: Core.Maybe Types.KinesisFirehoseOutput
    -- ^ Identifies an Amazon Kinesis Firehose delivery stream as the destination.
  , kinesisStreamsOutput :: Core.Maybe Types.KinesisStreamsOutput
    -- ^ Identifies an Amazon Kinesis stream as the destination.
  , lambdaOutput :: Core.Maybe Types.LambdaOutput
    -- ^ Identifies an AWS Lambda function as the destination.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Output' value with any optional fields omitted.
mkOutput
    :: Types.Name -- ^ 'name'
    -> Types.DestinationSchema -- ^ 'destinationSchema'
    -> Output
mkOutput name destinationSchema
  = Output'{name, destinationSchema,
            kinesisFirehoseOutput = Core.Nothing,
            kinesisStreamsOutput = Core.Nothing, lambdaOutput = Core.Nothing}

-- | Name of the in-application stream.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oName :: Lens.Lens' Output Types.Name
oName = Lens.field @"name"
{-# INLINEABLE oName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Describes the data format when records are written to the destination. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
--
-- /Note:/ Consider using 'destinationSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oDestinationSchema :: Lens.Lens' Output Types.DestinationSchema
oDestinationSchema = Lens.field @"destinationSchema"
{-# INLINEABLE oDestinationSchema #-}
{-# DEPRECATED destinationSchema "Use generic-lens or generic-optics with 'destinationSchema' instead"  #-}

-- | Identifies an Amazon Kinesis Firehose delivery stream as the destination.
--
-- /Note:/ Consider using 'kinesisFirehoseOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oKinesisFirehoseOutput :: Lens.Lens' Output (Core.Maybe Types.KinesisFirehoseOutput)
oKinesisFirehoseOutput = Lens.field @"kinesisFirehoseOutput"
{-# INLINEABLE oKinesisFirehoseOutput #-}
{-# DEPRECATED kinesisFirehoseOutput "Use generic-lens or generic-optics with 'kinesisFirehoseOutput' instead"  #-}

-- | Identifies an Amazon Kinesis stream as the destination.
--
-- /Note:/ Consider using 'kinesisStreamsOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oKinesisStreamsOutput :: Lens.Lens' Output (Core.Maybe Types.KinesisStreamsOutput)
oKinesisStreamsOutput = Lens.field @"kinesisStreamsOutput"
{-# INLINEABLE oKinesisStreamsOutput #-}
{-# DEPRECATED kinesisStreamsOutput "Use generic-lens or generic-optics with 'kinesisStreamsOutput' instead"  #-}

-- | Identifies an AWS Lambda function as the destination.
--
-- /Note:/ Consider using 'lambdaOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oLambdaOutput :: Lens.Lens' Output (Core.Maybe Types.LambdaOutput)
oLambdaOutput = Lens.field @"lambdaOutput"
{-# INLINEABLE oLambdaOutput #-}
{-# DEPRECATED lambdaOutput "Use generic-lens or generic-optics with 'lambdaOutput' instead"  #-}

instance Core.FromJSON Output where
        toJSON Output{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("DestinationSchema" Core..= destinationSchema),
                  ("KinesisFirehoseOutput" Core..=) Core.<$> kinesisFirehoseOutput,
                  ("KinesisStreamsOutput" Core..=) Core.<$> kinesisStreamsOutput,
                  ("LambdaOutput" Core..=) Core.<$> lambdaOutput])
