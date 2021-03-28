{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.KinesisParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.KinesisParameters
  ( KinesisParameters (..)
  -- * Smart constructor
  , mkKinesisParameters
  -- * Lenses
  , kpPartitionKeyPath
  ) where

import qualified Network.AWS.CloudWatchEvents.Types.PartitionKeyPath as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This object enables you to specify a JSON path to extract from the event and use as the partition key for the Amazon Kinesis data stream, so that you can control the shard to which the event goes. If you do not include this parameter, the default is to use the @eventId@ as the partition key.
--
-- /See:/ 'mkKinesisParameters' smart constructor.
newtype KinesisParameters = KinesisParameters'
  { partitionKeyPath :: Types.PartitionKeyPath
    -- ^ The JSON path to be extracted from the event and used as the partition key. For more information, see <https://docs.aws.amazon.com/streams/latest/dev/key-concepts.html#partition-key Amazon Kinesis Streams Key Concepts> in the /Amazon Kinesis Streams Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisParameters' value with any optional fields omitted.
mkKinesisParameters
    :: Types.PartitionKeyPath -- ^ 'partitionKeyPath'
    -> KinesisParameters
mkKinesisParameters partitionKeyPath
  = KinesisParameters'{partitionKeyPath}

-- | The JSON path to be extracted from the event and used as the partition key. For more information, see <https://docs.aws.amazon.com/streams/latest/dev/key-concepts.html#partition-key Amazon Kinesis Streams Key Concepts> in the /Amazon Kinesis Streams Developer Guide/ .
--
-- /Note:/ Consider using 'partitionKeyPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpPartitionKeyPath :: Lens.Lens' KinesisParameters Types.PartitionKeyPath
kpPartitionKeyPath = Lens.field @"partitionKeyPath"
{-# INLINEABLE kpPartitionKeyPath #-}
{-# DEPRECATED partitionKeyPath "Use generic-lens or generic-optics with 'partitionKeyPath' instead"  #-}

instance Core.FromJSON KinesisParameters where
        toJSON KinesisParameters{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PartitionKeyPath" Core..= partitionKeyPath)])

instance Core.FromJSON KinesisParameters where
        parseJSON
          = Core.withObject "KinesisParameters" Core.$
              \ x -> KinesisParameters' Core.<$> (x Core..: "PartitionKeyPath")
