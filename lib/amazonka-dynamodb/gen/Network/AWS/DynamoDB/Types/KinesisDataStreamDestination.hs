{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.KinesisDataStreamDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.KinesisDataStreamDestination
  ( KinesisDataStreamDestination (..)
  -- * Smart constructor
  , mkKinesisDataStreamDestination
  -- * Lenses
  , kdsdDestinationStatus
  , kdsdDestinationStatusDescription
  , kdsdStreamArn
  ) where

import qualified Network.AWS.DynamoDB.Types.DestinationStatus as Types
import qualified Network.AWS.DynamoDB.Types.StreamArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Kinesis data stream destination.
--
-- /See:/ 'mkKinesisDataStreamDestination' smart constructor.
data KinesisDataStreamDestination = KinesisDataStreamDestination'
  { destinationStatus :: Core.Maybe Types.DestinationStatus
    -- ^ The current status of replication.
  , destinationStatusDescription :: Core.Maybe Core.Text
    -- ^ The human-readable string that corresponds to the replica status.
  , streamArn :: Core.Maybe Types.StreamArn
    -- ^ The ARN for a specific Kinesis data stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisDataStreamDestination' value with any optional fields omitted.
mkKinesisDataStreamDestination
    :: KinesisDataStreamDestination
mkKinesisDataStreamDestination
  = KinesisDataStreamDestination'{destinationStatus = Core.Nothing,
                                  destinationStatusDescription = Core.Nothing,
                                  streamArn = Core.Nothing}

-- | The current status of replication.
--
-- /Note:/ Consider using 'destinationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kdsdDestinationStatus :: Lens.Lens' KinesisDataStreamDestination (Core.Maybe Types.DestinationStatus)
kdsdDestinationStatus = Lens.field @"destinationStatus"
{-# INLINEABLE kdsdDestinationStatus #-}
{-# DEPRECATED destinationStatus "Use generic-lens or generic-optics with 'destinationStatus' instead"  #-}

-- | The human-readable string that corresponds to the replica status.
--
-- /Note:/ Consider using 'destinationStatusDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kdsdDestinationStatusDescription :: Lens.Lens' KinesisDataStreamDestination (Core.Maybe Core.Text)
kdsdDestinationStatusDescription = Lens.field @"destinationStatusDescription"
{-# INLINEABLE kdsdDestinationStatusDescription #-}
{-# DEPRECATED destinationStatusDescription "Use generic-lens or generic-optics with 'destinationStatusDescription' instead"  #-}

-- | The ARN for a specific Kinesis data stream.
--
-- /Note:/ Consider using 'streamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kdsdStreamArn :: Lens.Lens' KinesisDataStreamDestination (Core.Maybe Types.StreamArn)
kdsdStreamArn = Lens.field @"streamArn"
{-# INLINEABLE kdsdStreamArn #-}
{-# DEPRECATED streamArn "Use generic-lens or generic-optics with 'streamArn' instead"  #-}

instance Core.FromJSON KinesisDataStreamDestination where
        parseJSON
          = Core.withObject "KinesisDataStreamDestination" Core.$
              \ x ->
                KinesisDataStreamDestination' Core.<$>
                  (x Core..:? "DestinationStatus") Core.<*>
                    x Core..:? "DestinationStatusDescription"
                    Core.<*> x Core..:? "StreamArn"
