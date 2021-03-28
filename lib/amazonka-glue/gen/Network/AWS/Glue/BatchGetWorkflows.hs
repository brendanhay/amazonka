{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchGetWorkflows
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of workflow names. After calling the @ListWorkflows@ operation, you can call this operation to access the data to which you have been granted permissions. This operation supports all IAM permissions, including permission conditions that uses tags.
module Network.AWS.Glue.BatchGetWorkflows
    (
    -- * Creating a request
      BatchGetWorkflows (..)
    , mkBatchGetWorkflows
    -- ** Request lenses
    , bgwNames
    , bgwIncludeGraph

    -- * Destructuring the response
    , BatchGetWorkflowsResponse (..)
    , mkBatchGetWorkflowsResponse
    -- ** Response lenses
    , bgwrrsMissingWorkflows
    , bgwrrsWorkflows
    , bgwrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetWorkflows' smart constructor.
data BatchGetWorkflows = BatchGetWorkflows'
  { names :: Core.NonEmpty Types.NameString
    -- ^ A list of workflow names, which may be the names returned from the @ListWorkflows@ operation.
  , includeGraph :: Core.Maybe Core.Bool
    -- ^ Specifies whether to include a graph when returning the workflow resource metadata.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetWorkflows' value with any optional fields omitted.
mkBatchGetWorkflows
    :: Core.NonEmpty Types.NameString -- ^ 'names'
    -> BatchGetWorkflows
mkBatchGetWorkflows names
  = BatchGetWorkflows'{names, includeGraph = Core.Nothing}

-- | A list of workflow names, which may be the names returned from the @ListWorkflows@ operation.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgwNames :: Lens.Lens' BatchGetWorkflows (Core.NonEmpty Types.NameString)
bgwNames = Lens.field @"names"
{-# INLINEABLE bgwNames #-}
{-# DEPRECATED names "Use generic-lens or generic-optics with 'names' instead"  #-}

-- | Specifies whether to include a graph when returning the workflow resource metadata.
--
-- /Note:/ Consider using 'includeGraph' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgwIncludeGraph :: Lens.Lens' BatchGetWorkflows (Core.Maybe Core.Bool)
bgwIncludeGraph = Lens.field @"includeGraph"
{-# INLINEABLE bgwIncludeGraph #-}
{-# DEPRECATED includeGraph "Use generic-lens or generic-optics with 'includeGraph' instead"  #-}

instance Core.ToQuery BatchGetWorkflows where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetWorkflows where
        toHeaders BatchGetWorkflows{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.BatchGetWorkflows") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetWorkflows where
        toJSON BatchGetWorkflows{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Names" Core..= names),
                  ("IncludeGraph" Core..=) Core.<$> includeGraph])

instance Core.AWSRequest BatchGetWorkflows where
        type Rs BatchGetWorkflows = BatchGetWorkflowsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetWorkflowsResponse' Core.<$>
                   (x Core..:? "MissingWorkflows") Core.<*> x Core..:? "Workflows"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchGetWorkflowsResponse' smart constructor.
data BatchGetWorkflowsResponse = BatchGetWorkflowsResponse'
  { missingWorkflows :: Core.Maybe (Core.NonEmpty Types.NameString)
    -- ^ A list of names of workflows not found.
  , workflows :: Core.Maybe (Core.NonEmpty Types.Workflow)
    -- ^ A list of workflow resource metadata.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchGetWorkflowsResponse' value with any optional fields omitted.
mkBatchGetWorkflowsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetWorkflowsResponse
mkBatchGetWorkflowsResponse responseStatus
  = BatchGetWorkflowsResponse'{missingWorkflows = Core.Nothing,
                               workflows = Core.Nothing, responseStatus}

-- | A list of names of workflows not found.
--
-- /Note:/ Consider using 'missingWorkflows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgwrrsMissingWorkflows :: Lens.Lens' BatchGetWorkflowsResponse (Core.Maybe (Core.NonEmpty Types.NameString))
bgwrrsMissingWorkflows = Lens.field @"missingWorkflows"
{-# INLINEABLE bgwrrsMissingWorkflows #-}
{-# DEPRECATED missingWorkflows "Use generic-lens or generic-optics with 'missingWorkflows' instead"  #-}

-- | A list of workflow resource metadata.
--
-- /Note:/ Consider using 'workflows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgwrrsWorkflows :: Lens.Lens' BatchGetWorkflowsResponse (Core.Maybe (Core.NonEmpty Types.Workflow))
bgwrrsWorkflows = Lens.field @"workflows"
{-# INLINEABLE bgwrrsWorkflows #-}
{-# DEPRECATED workflows "Use generic-lens or generic-optics with 'workflows' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgwrrsResponseStatus :: Lens.Lens' BatchGetWorkflowsResponse Core.Int
bgwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
