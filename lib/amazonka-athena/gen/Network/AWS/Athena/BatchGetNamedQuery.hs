{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.BatchGetNamedQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of a single named query or a list of up to 50 queries, which you provide as an array of query ID strings. Requires you to have access to the workgroup in which the queries were saved. Use 'ListNamedQueriesInput' to get the list of named query IDs in the specified workgroup. If information could not be retrieved for a submitted query ID, information about the query ID submitted is listed under 'UnprocessedNamedQueryId' . Named queries differ from executed queries. Use 'BatchGetQueryExecutionInput' to get details about each unique query execution, and 'ListQueryExecutionsInput' to get a list of query execution IDs.
module Network.AWS.Athena.BatchGetNamedQuery
    (
    -- * Creating a request
      BatchGetNamedQuery (..)
    , mkBatchGetNamedQuery
    -- ** Request lenses
    , bgnqNamedQueryIds

    -- * Destructuring the response
    , BatchGetNamedQueryResponse (..)
    , mkBatchGetNamedQueryResponse
    -- ** Response lenses
    , bgnqrrsNamedQueries
    , bgnqrrsUnprocessedNamedQueryIds
    , bgnqrrsResponseStatus
    ) where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetNamedQuery' smart constructor.
newtype BatchGetNamedQuery = BatchGetNamedQuery'
  { namedQueryIds :: Core.NonEmpty Types.NamedQueryId
    -- ^ An array of query IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetNamedQuery' value with any optional fields omitted.
mkBatchGetNamedQuery
    :: Core.NonEmpty Types.NamedQueryId -- ^ 'namedQueryIds'
    -> BatchGetNamedQuery
mkBatchGetNamedQuery namedQueryIds
  = BatchGetNamedQuery'{namedQueryIds}

-- | An array of query IDs.
--
-- /Note:/ Consider using 'namedQueryIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgnqNamedQueryIds :: Lens.Lens' BatchGetNamedQuery (Core.NonEmpty Types.NamedQueryId)
bgnqNamedQueryIds = Lens.field @"namedQueryIds"
{-# INLINEABLE bgnqNamedQueryIds #-}
{-# DEPRECATED namedQueryIds "Use generic-lens or generic-optics with 'namedQueryIds' instead"  #-}

instance Core.ToQuery BatchGetNamedQuery where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetNamedQuery where
        toHeaders BatchGetNamedQuery{..}
          = Core.pure ("X-Amz-Target", "AmazonAthena.BatchGetNamedQuery")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetNamedQuery where
        toJSON BatchGetNamedQuery{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("NamedQueryIds" Core..= namedQueryIds)])

instance Core.AWSRequest BatchGetNamedQuery where
        type Rs BatchGetNamedQuery = BatchGetNamedQueryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetNamedQueryResponse' Core.<$>
                   (x Core..:? "NamedQueries") Core.<*>
                     x Core..:? "UnprocessedNamedQueryIds"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchGetNamedQueryResponse' smart constructor.
data BatchGetNamedQueryResponse = BatchGetNamedQueryResponse'
  { namedQueries :: Core.Maybe [Types.NamedQuery]
    -- ^ Information about the named query IDs submitted.
  , unprocessedNamedQueryIds :: Core.Maybe [Types.UnprocessedNamedQueryId]
    -- ^ Information about provided query IDs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetNamedQueryResponse' value with any optional fields omitted.
mkBatchGetNamedQueryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetNamedQueryResponse
mkBatchGetNamedQueryResponse responseStatus
  = BatchGetNamedQueryResponse'{namedQueries = Core.Nothing,
                                unprocessedNamedQueryIds = Core.Nothing, responseStatus}

-- | Information about the named query IDs submitted.
--
-- /Note:/ Consider using 'namedQueries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgnqrrsNamedQueries :: Lens.Lens' BatchGetNamedQueryResponse (Core.Maybe [Types.NamedQuery])
bgnqrrsNamedQueries = Lens.field @"namedQueries"
{-# INLINEABLE bgnqrrsNamedQueries #-}
{-# DEPRECATED namedQueries "Use generic-lens or generic-optics with 'namedQueries' instead"  #-}

-- | Information about provided query IDs.
--
-- /Note:/ Consider using 'unprocessedNamedQueryIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgnqrrsUnprocessedNamedQueryIds :: Lens.Lens' BatchGetNamedQueryResponse (Core.Maybe [Types.UnprocessedNamedQueryId])
bgnqrrsUnprocessedNamedQueryIds = Lens.field @"unprocessedNamedQueryIds"
{-# INLINEABLE bgnqrrsUnprocessedNamedQueryIds #-}
{-# DEPRECATED unprocessedNamedQueryIds "Use generic-lens or generic-optics with 'unprocessedNamedQueryIds' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgnqrrsResponseStatus :: Lens.Lens' BatchGetNamedQueryResponse Core.Int
bgnqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgnqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
