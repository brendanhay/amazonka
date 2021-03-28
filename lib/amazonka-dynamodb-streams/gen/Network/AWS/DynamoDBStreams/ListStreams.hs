{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.ListStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of stream ARNs associated with the current account and endpoint. If the @TableName@ parameter is present, then @ListStreams@ will return only the streams ARNs for that table.
module Network.AWS.DynamoDBStreams.ListStreams
    (
    -- * Creating a request
      ListStreams (..)
    , mkListStreams
    -- ** Request lenses
    , lsExclusiveStartStreamArn
    , lsLimit
    , lsTableName

    -- * Destructuring the response
    , ListStreamsResponse (..)
    , mkListStreamsResponse
    -- ** Response lenses
    , lsrrsLastEvaluatedStreamArn
    , lsrrsStreams
    , lsrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDBStreams.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListStreams@ operation.
--
-- /See:/ 'mkListStreams' smart constructor.
data ListStreams = ListStreams'
  { exclusiveStartStreamArn :: Core.Maybe Types.StreamArn
    -- ^ The ARN (Amazon Resource Name) of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedStreamArn@ in the previous operation. 
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of streams to return. The upper limit is 100.
  , tableName :: Core.Maybe Types.TableName
    -- ^ If this parameter is provided, then only the streams associated with this table name are returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStreams' value with any optional fields omitted.
mkListStreams
    :: ListStreams
mkListStreams
  = ListStreams'{exclusiveStartStreamArn = Core.Nothing,
                 limit = Core.Nothing, tableName = Core.Nothing}

-- | The ARN (Amazon Resource Name) of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedStreamArn@ in the previous operation. 
--
-- /Note:/ Consider using 'exclusiveStartStreamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsExclusiveStartStreamArn :: Lens.Lens' ListStreams (Core.Maybe Types.StreamArn)
lsExclusiveStartStreamArn = Lens.field @"exclusiveStartStreamArn"
{-# INLINEABLE lsExclusiveStartStreamArn #-}
{-# DEPRECATED exclusiveStartStreamArn "Use generic-lens or generic-optics with 'exclusiveStartStreamArn' instead"  #-}

-- | The maximum number of streams to return. The upper limit is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLimit :: Lens.Lens' ListStreams (Core.Maybe Core.Natural)
lsLimit = Lens.field @"limit"
{-# INLINEABLE lsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | If this parameter is provided, then only the streams associated with this table name are returned.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsTableName :: Lens.Lens' ListStreams (Core.Maybe Types.TableName)
lsTableName = Lens.field @"tableName"
{-# INLINEABLE lsTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

instance Core.ToQuery ListStreams where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListStreams where
        toHeaders ListStreams{..}
          = Core.pure
              ("X-Amz-Target", "DynamoDBStreams_20120810.ListStreams")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON ListStreams where
        toJSON ListStreams{..}
          = Core.object
              (Core.catMaybes
                 [("ExclusiveStartStreamArn" Core..=) Core.<$>
                    exclusiveStartStreamArn,
                  ("Limit" Core..=) Core.<$> limit,
                  ("TableName" Core..=) Core.<$> tableName])

instance Core.AWSRequest ListStreams where
        type Rs ListStreams = ListStreamsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListStreamsResponse' Core.<$>
                   (x Core..:? "LastEvaluatedStreamArn") Core.<*> x Core..:? "Streams"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @ListStreams@ operation.
--
-- /See:/ 'mkListStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { lastEvaluatedStreamArn :: Core.Maybe Types.LastEvaluatedStreamArn
    -- ^ The stream ARN of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request.
--
-- If @LastEvaluatedStreamArn@ is empty, then the "last page" of results has been processed and there is no more data to be retrieved.
-- If @LastEvaluatedStreamArn@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedStreamArn@ is empty.
  , streams :: Core.Maybe [Types.Stream]
    -- ^ A list of stream descriptors associated with the current account and endpoint.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStreamsResponse' value with any optional fields omitted.
mkListStreamsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListStreamsResponse
mkListStreamsResponse responseStatus
  = ListStreamsResponse'{lastEvaluatedStreamArn = Core.Nothing,
                         streams = Core.Nothing, responseStatus}

-- | The stream ARN of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request.
--
-- If @LastEvaluatedStreamArn@ is empty, then the "last page" of results has been processed and there is no more data to be retrieved.
-- If @LastEvaluatedStreamArn@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedStreamArn@ is empty.
--
-- /Note:/ Consider using 'lastEvaluatedStreamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsLastEvaluatedStreamArn :: Lens.Lens' ListStreamsResponse (Core.Maybe Types.LastEvaluatedStreamArn)
lsrrsLastEvaluatedStreamArn = Lens.field @"lastEvaluatedStreamArn"
{-# INLINEABLE lsrrsLastEvaluatedStreamArn #-}
{-# DEPRECATED lastEvaluatedStreamArn "Use generic-lens or generic-optics with 'lastEvaluatedStreamArn' instead"  #-}

-- | A list of stream descriptors associated with the current account and endpoint.
--
-- /Note:/ Consider using 'streams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsStreams :: Lens.Lens' ListStreamsResponse (Core.Maybe [Types.Stream])
lsrrsStreams = Lens.field @"streams"
{-# INLINEABLE lsrrsStreams #-}
{-# DEPRECATED streams "Use generic-lens or generic-optics with 'streams' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsResponseStatus :: Lens.Lens' ListStreamsResponse Core.Int
lsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
