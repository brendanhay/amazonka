{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.KinesisStreamingDestinationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.KinesisStreamingDestinationOutput
  ( KinesisStreamingDestinationOutput (..)
  -- * Smart constructor
  , mkKinesisStreamingDestinationOutput
  -- * Lenses
  , ksdoDestinationStatus
  , ksdoStreamArn
  , ksdoTableName
  ) where

import qualified Network.AWS.DynamoDB.Types.DestinationStatus as Types
import qualified Network.AWS.DynamoDB.Types.StreamArn as Types
import qualified Network.AWS.DynamoDB.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkKinesisStreamingDestinationOutput' smart constructor.
data KinesisStreamingDestinationOutput = KinesisStreamingDestinationOutput'
  { destinationStatus :: Core.Maybe Types.DestinationStatus
    -- ^ The current status of the replication.
  , streamArn :: Core.Maybe Types.StreamArn
    -- ^ The ARN for the specific Kinesis data stream.
  , tableName :: Core.Maybe Types.TableName
    -- ^ The name of the table being modified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisStreamingDestinationOutput' value with any optional fields omitted.
mkKinesisStreamingDestinationOutput
    :: KinesisStreamingDestinationOutput
mkKinesisStreamingDestinationOutput
  = KinesisStreamingDestinationOutput'{destinationStatus =
                                         Core.Nothing,
                                       streamArn = Core.Nothing, tableName = Core.Nothing}

-- | The current status of the replication.
--
-- /Note:/ Consider using 'destinationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksdoDestinationStatus :: Lens.Lens' KinesisStreamingDestinationOutput (Core.Maybe Types.DestinationStatus)
ksdoDestinationStatus = Lens.field @"destinationStatus"
{-# INLINEABLE ksdoDestinationStatus #-}
{-# DEPRECATED destinationStatus "Use generic-lens or generic-optics with 'destinationStatus' instead"  #-}

-- | The ARN for the specific Kinesis data stream.
--
-- /Note:/ Consider using 'streamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksdoStreamArn :: Lens.Lens' KinesisStreamingDestinationOutput (Core.Maybe Types.StreamArn)
ksdoStreamArn = Lens.field @"streamArn"
{-# INLINEABLE ksdoStreamArn #-}
{-# DEPRECATED streamArn "Use generic-lens or generic-optics with 'streamArn' instead"  #-}

-- | The name of the table being modified.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksdoTableName :: Lens.Lens' KinesisStreamingDestinationOutput (Core.Maybe Types.TableName)
ksdoTableName = Lens.field @"tableName"
{-# INLINEABLE ksdoTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

instance Core.FromJSON KinesisStreamingDestinationOutput where
        parseJSON
          = Core.withObject "KinesisStreamingDestinationOutput" Core.$
              \ x ->
                KinesisStreamingDestinationOutput' Core.<$>
                  (x Core..:? "DestinationStatus") Core.<*> x Core..:? "StreamArn"
                    Core.<*> x Core..:? "TableName"
