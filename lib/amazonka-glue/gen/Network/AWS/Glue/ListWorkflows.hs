{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListWorkflows
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists names of workflows created in the account.
module Network.AWS.Glue.ListWorkflows
    (
    -- * Creating a request
      ListWorkflows (..)
    , mkListWorkflows
    -- ** Request lenses
    , lwMaxResults
    , lwNextToken

    -- * Destructuring the response
    , ListWorkflowsResponse (..)
    , mkListWorkflowsResponse
    -- ** Response lenses
    , lwrrsNextToken
    , lwrrsWorkflows
    , lwrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListWorkflows' smart constructor.
data ListWorkflows = ListWorkflows'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum size of a list to return.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token, if this is a continuation request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListWorkflows' value with any optional fields omitted.
mkListWorkflows
    :: ListWorkflows
mkListWorkflows
  = ListWorkflows'{maxResults = Core.Nothing,
                   nextToken = Core.Nothing}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwMaxResults :: Lens.Lens' ListWorkflows (Core.Maybe Core.Natural)
lwMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lwMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwNextToken :: Lens.Lens' ListWorkflows (Core.Maybe Types.NextToken)
lwNextToken = Lens.field @"nextToken"
{-# INLINEABLE lwNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListWorkflows where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListWorkflows where
        toHeaders ListWorkflows{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.ListWorkflows") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListWorkflows where
        toJSON ListWorkflows{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListWorkflows where
        type Rs ListWorkflows = ListWorkflowsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListWorkflowsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Workflows" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListWorkflowsResponse' smart constructor.
data ListWorkflowsResponse = ListWorkflowsResponse'
  { nextToken :: Core.Maybe Types.GenericString
    -- ^ A continuation token, if not all workflow names have been returned.
  , workflows :: Core.Maybe (Core.NonEmpty Types.NameString)
    -- ^ List of names of workflows in the account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListWorkflowsResponse' value with any optional fields omitted.
mkListWorkflowsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListWorkflowsResponse
mkListWorkflowsResponse responseStatus
  = ListWorkflowsResponse'{nextToken = Core.Nothing,
                           workflows = Core.Nothing, responseStatus}

-- | A continuation token, if not all workflow names have been returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrrsNextToken :: Lens.Lens' ListWorkflowsResponse (Core.Maybe Types.GenericString)
lwrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lwrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | List of names of workflows in the account.
--
-- /Note:/ Consider using 'workflows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrrsWorkflows :: Lens.Lens' ListWorkflowsResponse (Core.Maybe (Core.NonEmpty Types.NameString))
lwrrsWorkflows = Lens.field @"workflows"
{-# INLINEABLE lwrrsWorkflows #-}
{-# DEPRECATED workflows "Use generic-lens or generic-optics with 'workflows' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrrsResponseStatus :: Lens.Lens' ListWorkflowsResponse Core.Int
lwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
