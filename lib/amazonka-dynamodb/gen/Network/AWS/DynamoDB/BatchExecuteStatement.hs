{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.BatchExecuteStatement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows you to perform batch reads and writes on data stored in DynamoDB, using PartiQL. 
module Network.AWS.DynamoDB.BatchExecuteStatement
    (
    -- * Creating a request
      BatchExecuteStatement (..)
    , mkBatchExecuteStatement
    -- ** Request lenses
    , besStatements

    -- * Destructuring the response
    , BatchExecuteStatementResponse (..)
    , mkBatchExecuteStatementResponse
    -- ** Response lenses
    , besrrsResponses
    , besrrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchExecuteStatement' smart constructor.
newtype BatchExecuteStatement = BatchExecuteStatement'
  { statements :: Core.NonEmpty Types.BatchStatementRequest
    -- ^ The list of PartiQL statements representing the batch to run. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchExecuteStatement' value with any optional fields omitted.
mkBatchExecuteStatement
    :: Core.NonEmpty Types.BatchStatementRequest -- ^ 'statements'
    -> BatchExecuteStatement
mkBatchExecuteStatement statements
  = BatchExecuteStatement'{statements}

-- | The list of PartiQL statements representing the batch to run. 
--
-- /Note:/ Consider using 'statements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
besStatements :: Lens.Lens' BatchExecuteStatement (Core.NonEmpty Types.BatchStatementRequest)
besStatements = Lens.field @"statements"
{-# INLINEABLE besStatements #-}
{-# DEPRECATED statements "Use generic-lens or generic-optics with 'statements' instead"  #-}

instance Core.ToQuery BatchExecuteStatement where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchExecuteStatement where
        toHeaders BatchExecuteStatement{..}
          = Core.pure
              ("X-Amz-Target", "DynamoDB_20120810.BatchExecuteStatement")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON BatchExecuteStatement where
        toJSON BatchExecuteStatement{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Statements" Core..= statements)])

instance Core.AWSRequest BatchExecuteStatement where
        type Rs BatchExecuteStatement = BatchExecuteStatementResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchExecuteStatementResponse' Core.<$>
                   (x Core..:? "Responses") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchExecuteStatementResponse' smart constructor.
data BatchExecuteStatementResponse = BatchExecuteStatementResponse'
  { responses :: Core.Maybe [Types.BatchStatementResponse]
    -- ^ The response to each PartiQL statement in the batch. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchExecuteStatementResponse' value with any optional fields omitted.
mkBatchExecuteStatementResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchExecuteStatementResponse
mkBatchExecuteStatementResponse responseStatus
  = BatchExecuteStatementResponse'{responses = Core.Nothing,
                                   responseStatus}

-- | The response to each PartiQL statement in the batch. 
--
-- /Note:/ Consider using 'responses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
besrrsResponses :: Lens.Lens' BatchExecuteStatementResponse (Core.Maybe [Types.BatchStatementResponse])
besrrsResponses = Lens.field @"responses"
{-# INLINEABLE besrrsResponses #-}
{-# DEPRECATED responses "Use generic-lens or generic-optics with 'responses' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
besrrsResponseStatus :: Lens.Lens' BatchExecuteStatementResponse Core.Int
besrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE besrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
