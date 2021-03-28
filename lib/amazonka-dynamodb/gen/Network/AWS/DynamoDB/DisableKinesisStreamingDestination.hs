{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DisableKinesisStreamingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops replication from the DynamoDB table to the Kinesis data stream. This is done without deleting either of the resources.
module Network.AWS.DynamoDB.DisableKinesisStreamingDestination
    (
    -- * Creating a request
      DisableKinesisStreamingDestination (..)
    , mkDisableKinesisStreamingDestination
    -- ** Request lenses
    , dksdTableName
    , dksdStreamArn

     -- * Destructuring the response
    , Types.KinesisStreamingDestinationOutput (..)
    , Types.mkKinesisStreamingDestinationOutput
    -- ** Response lenses
    , Types.ksdoDestinationStatus
    , Types.ksdoStreamArn
    , Types.ksdoTableName
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableKinesisStreamingDestination' smart constructor.
data DisableKinesisStreamingDestination = DisableKinesisStreamingDestination'
  { tableName :: Types.TableName
    -- ^ The name of the DynamoDB table.
  , streamArn :: Types.StreamArn
    -- ^ The ARN for a Kinesis data stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableKinesisStreamingDestination' value with any optional fields omitted.
mkDisableKinesisStreamingDestination
    :: Types.TableName -- ^ 'tableName'
    -> Types.StreamArn -- ^ 'streamArn'
    -> DisableKinesisStreamingDestination
mkDisableKinesisStreamingDestination tableName streamArn
  = DisableKinesisStreamingDestination'{tableName, streamArn}

-- | The name of the DynamoDB table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dksdTableName :: Lens.Lens' DisableKinesisStreamingDestination Types.TableName
dksdTableName = Lens.field @"tableName"
{-# INLINEABLE dksdTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | The ARN for a Kinesis data stream.
--
-- /Note:/ Consider using 'streamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dksdStreamArn :: Lens.Lens' DisableKinesisStreamingDestination Types.StreamArn
dksdStreamArn = Lens.field @"streamArn"
{-# INLINEABLE dksdStreamArn #-}
{-# DEPRECATED streamArn "Use generic-lens or generic-optics with 'streamArn' instead"  #-}

instance Core.ToQuery DisableKinesisStreamingDestination where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisableKinesisStreamingDestination where
        toHeaders DisableKinesisStreamingDestination{..}
          = Core.pure
              ("X-Amz-Target",
               "DynamoDB_20120810.DisableKinesisStreamingDestination")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DisableKinesisStreamingDestination where
        toJSON DisableKinesisStreamingDestination{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TableName" Core..= tableName),
                  Core.Just ("StreamArn" Core..= streamArn)])

instance Core.AWSRequest DisableKinesisStreamingDestination where
        type Rs DisableKinesisStreamingDestination =
             Types.KinesisStreamingDestinationOutput
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
