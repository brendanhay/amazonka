{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.BatchGetQueryExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of a single query execution or a list of up to 50 query executions, which you provide as an array of query execution ID strings. Requires you to have access to the workgroup in which the queries ran. To get a list of query execution IDs, use 'ListQueryExecutionsInput$WorkGroup' . Query executions differ from named (saved) queries. Use 'BatchGetNamedQueryInput' to get details about named queries.
module Network.AWS.Athena.BatchGetQueryExecution
    (
    -- * Creating a request
      BatchGetQueryExecution (..)
    , mkBatchGetQueryExecution
    -- ** Request lenses
    , bgqeQueryExecutionIds

    -- * Destructuring the response
    , BatchGetQueryExecutionResponse (..)
    , mkBatchGetQueryExecutionResponse
    -- ** Response lenses
    , bgqerrsQueryExecutions
    , bgqerrsUnprocessedQueryExecutionIds
    , bgqerrsResponseStatus
    ) where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetQueryExecution' smart constructor.
newtype BatchGetQueryExecution = BatchGetQueryExecution'
  { queryExecutionIds :: Core.NonEmpty Types.QueryExecutionId
    -- ^ An array of query execution IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetQueryExecution' value with any optional fields omitted.
mkBatchGetQueryExecution
    :: Core.NonEmpty Types.QueryExecutionId -- ^ 'queryExecutionIds'
    -> BatchGetQueryExecution
mkBatchGetQueryExecution queryExecutionIds
  = BatchGetQueryExecution'{queryExecutionIds}

-- | An array of query execution IDs.
--
-- /Note:/ Consider using 'queryExecutionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgqeQueryExecutionIds :: Lens.Lens' BatchGetQueryExecution (Core.NonEmpty Types.QueryExecutionId)
bgqeQueryExecutionIds = Lens.field @"queryExecutionIds"
{-# INLINEABLE bgqeQueryExecutionIds #-}
{-# DEPRECATED queryExecutionIds "Use generic-lens or generic-optics with 'queryExecutionIds' instead"  #-}

instance Core.ToQuery BatchGetQueryExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetQueryExecution where
        toHeaders BatchGetQueryExecution{..}
          = Core.pure ("X-Amz-Target", "AmazonAthena.BatchGetQueryExecution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetQueryExecution where
        toJSON BatchGetQueryExecution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("QueryExecutionIds" Core..= queryExecutionIds)])

instance Core.AWSRequest BatchGetQueryExecution where
        type Rs BatchGetQueryExecution = BatchGetQueryExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetQueryExecutionResponse' Core.<$>
                   (x Core..:? "QueryExecutions") Core.<*>
                     x Core..:? "UnprocessedQueryExecutionIds"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchGetQueryExecutionResponse' smart constructor.
data BatchGetQueryExecutionResponse = BatchGetQueryExecutionResponse'
  { queryExecutions :: Core.Maybe [Types.QueryExecution]
    -- ^ Information about a query execution.
  , unprocessedQueryExecutionIds :: Core.Maybe [Types.UnprocessedQueryExecutionId]
    -- ^ Information about the query executions that failed to run.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchGetQueryExecutionResponse' value with any optional fields omitted.
mkBatchGetQueryExecutionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetQueryExecutionResponse
mkBatchGetQueryExecutionResponse responseStatus
  = BatchGetQueryExecutionResponse'{queryExecutions = Core.Nothing,
                                    unprocessedQueryExecutionIds = Core.Nothing, responseStatus}

-- | Information about a query execution.
--
-- /Note:/ Consider using 'queryExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgqerrsQueryExecutions :: Lens.Lens' BatchGetQueryExecutionResponse (Core.Maybe [Types.QueryExecution])
bgqerrsQueryExecutions = Lens.field @"queryExecutions"
{-# INLINEABLE bgqerrsQueryExecutions #-}
{-# DEPRECATED queryExecutions "Use generic-lens or generic-optics with 'queryExecutions' instead"  #-}

-- | Information about the query executions that failed to run.
--
-- /Note:/ Consider using 'unprocessedQueryExecutionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgqerrsUnprocessedQueryExecutionIds :: Lens.Lens' BatchGetQueryExecutionResponse (Core.Maybe [Types.UnprocessedQueryExecutionId])
bgqerrsUnprocessedQueryExecutionIds = Lens.field @"unprocessedQueryExecutionIds"
{-# INLINEABLE bgqerrsUnprocessedQueryExecutionIds #-}
{-# DEPRECATED unprocessedQueryExecutionIds "Use generic-lens or generic-optics with 'unprocessedQueryExecutionIds' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgqerrsResponseStatus :: Lens.Lens' BatchGetQueryExecutionResponse Core.Int
bgqerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgqerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
