{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateTimeToLive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateTimeToLive@ method enables or disables Time to Live (TTL) for the specified table. A successful @UpdateTimeToLive@ call returns the current @TimeToLiveSpecification@ . It can take up to one hour for the change to fully process. Any additional @UpdateTimeToLive@ calls for the same table during this one hour duration result in a @ValidationException@ . 
--
-- TTL compares the current time in epoch time format to the time stored in the TTL attribute of an item. If the epoch time value stored in the attribute is less than the current time, the item is marked as expired and subsequently deleted.
-- DynamoDB deletes expired items on a best-effort basis to ensure availability of throughput for other data operations. 
-- /Important:/ DynamoDB typically deletes expired items within two days of expiration. The exact duration within which an item gets deleted after expiration is specific to the nature of the workload. Items that have expired and not been deleted will still show up in reads, queries, and scans.
-- As items are deleted, they are removed from any local secondary index and global secondary index immediately in the same eventually consistent way as a standard delete operation.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/TTL.html Time To Live> in the Amazon DynamoDB Developer Guide. 
module Network.AWS.DynamoDB.UpdateTimeToLive
    (
    -- * Creating a request
      UpdateTimeToLive (..)
    , mkUpdateTimeToLive
    -- ** Request lenses
    , uttlTableName
    , uttlTimeToLiveSpecification

    -- * Destructuring the response
    , UpdateTimeToLiveResponse (..)
    , mkUpdateTimeToLiveResponse
    -- ** Response lenses
    , uttlrrsTimeToLiveSpecification
    , uttlrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an @UpdateTimeToLive@ operation.
--
-- /See:/ 'mkUpdateTimeToLive' smart constructor.
data UpdateTimeToLive = UpdateTimeToLive'
  { tableName :: Types.TableName
    -- ^ The name of the table to be configured.
  , timeToLiveSpecification :: Types.TimeToLiveSpecification
    -- ^ Represents the settings used to enable or disable Time to Live for the specified table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTimeToLive' value with any optional fields omitted.
mkUpdateTimeToLive
    :: Types.TableName -- ^ 'tableName'
    -> Types.TimeToLiveSpecification -- ^ 'timeToLiveSpecification'
    -> UpdateTimeToLive
mkUpdateTimeToLive tableName timeToLiveSpecification
  = UpdateTimeToLive'{tableName, timeToLiveSpecification}

-- | The name of the table to be configured.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uttlTableName :: Lens.Lens' UpdateTimeToLive Types.TableName
uttlTableName = Lens.field @"tableName"
{-# INLINEABLE uttlTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | Represents the settings used to enable or disable Time to Live for the specified table.
--
-- /Note:/ Consider using 'timeToLiveSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uttlTimeToLiveSpecification :: Lens.Lens' UpdateTimeToLive Types.TimeToLiveSpecification
uttlTimeToLiveSpecification = Lens.field @"timeToLiveSpecification"
{-# INLINEABLE uttlTimeToLiveSpecification #-}
{-# DEPRECATED timeToLiveSpecification "Use generic-lens or generic-optics with 'timeToLiveSpecification' instead"  #-}

instance Core.ToQuery UpdateTimeToLive where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateTimeToLive where
        toHeaders UpdateTimeToLive{..}
          = Core.pure ("X-Amz-Target", "DynamoDB_20120810.UpdateTimeToLive")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON UpdateTimeToLive where
        toJSON UpdateTimeToLive{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TableName" Core..= tableName),
                  Core.Just
                    ("TimeToLiveSpecification" Core..= timeToLiveSpecification)])

instance Core.AWSRequest UpdateTimeToLive where
        type Rs UpdateTimeToLive = UpdateTimeToLiveResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateTimeToLiveResponse' Core.<$>
                   (x Core..:? "TimeToLiveSpecification") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateTimeToLiveResponse' smart constructor.
data UpdateTimeToLiveResponse = UpdateTimeToLiveResponse'
  { timeToLiveSpecification :: Core.Maybe Types.TimeToLiveSpecification
    -- ^ Represents the output of an @UpdateTimeToLive@ operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTimeToLiveResponse' value with any optional fields omitted.
mkUpdateTimeToLiveResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateTimeToLiveResponse
mkUpdateTimeToLiveResponse responseStatus
  = UpdateTimeToLiveResponse'{timeToLiveSpecification = Core.Nothing,
                              responseStatus}

-- | Represents the output of an @UpdateTimeToLive@ operation.
--
-- /Note:/ Consider using 'timeToLiveSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uttlrrsTimeToLiveSpecification :: Lens.Lens' UpdateTimeToLiveResponse (Core.Maybe Types.TimeToLiveSpecification)
uttlrrsTimeToLiveSpecification = Lens.field @"timeToLiveSpecification"
{-# INLINEABLE uttlrrsTimeToLiveSpecification #-}
{-# DEPRECATED timeToLiveSpecification "Use generic-lens or generic-optics with 'timeToLiveSpecification' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uttlrrsResponseStatus :: Lens.Lens' UpdateTimeToLiveResponse Core.Int
uttlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uttlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
