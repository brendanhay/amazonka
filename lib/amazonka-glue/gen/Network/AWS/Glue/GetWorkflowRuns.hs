{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetWorkflowRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for all runs of a given workflow.
module Network.AWS.Glue.GetWorkflowRuns
    (
    -- * Creating a request
      GetWorkflowRuns (..)
    , mkGetWorkflowRuns
    -- ** Request lenses
    , gwrName
    , gwrIncludeGraph
    , gwrMaxResults
    , gwrNextToken

    -- * Destructuring the response
    , GetWorkflowRunsResponse (..)
    , mkGetWorkflowRunsResponse
    -- ** Response lenses
    , gwrrrsNextToken
    , gwrrrsRuns
    , gwrrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetWorkflowRuns' smart constructor.
data GetWorkflowRuns = GetWorkflowRuns'
  { name :: Types.NameString
    -- ^ Name of the workflow whose metadata of runs should be returned.
  , includeGraph :: Core.Maybe Core.Bool
    -- ^ Specifies whether to include the workflow graph in response or not.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of workflow runs to be included in the response.
  , nextToken :: Core.Maybe Types.GenericString
    -- ^ The maximum size of the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetWorkflowRuns' value with any optional fields omitted.
mkGetWorkflowRuns
    :: Types.NameString -- ^ 'name'
    -> GetWorkflowRuns
mkGetWorkflowRuns name
  = GetWorkflowRuns'{name, includeGraph = Core.Nothing,
                     maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Name of the workflow whose metadata of runs should be returned.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrName :: Lens.Lens' GetWorkflowRuns Types.NameString
gwrName = Lens.field @"name"
{-# INLINEABLE gwrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Specifies whether to include the workflow graph in response or not.
--
-- /Note:/ Consider using 'includeGraph' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrIncludeGraph :: Lens.Lens' GetWorkflowRuns (Core.Maybe Core.Bool)
gwrIncludeGraph = Lens.field @"includeGraph"
{-# INLINEABLE gwrIncludeGraph #-}
{-# DEPRECATED includeGraph "Use generic-lens or generic-optics with 'includeGraph' instead"  #-}

-- | The maximum number of workflow runs to be included in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrMaxResults :: Lens.Lens' GetWorkflowRuns (Core.Maybe Core.Natural)
gwrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gwrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The maximum size of the response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrNextToken :: Lens.Lens' GetWorkflowRuns (Core.Maybe Types.GenericString)
gwrNextToken = Lens.field @"nextToken"
{-# INLINEABLE gwrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetWorkflowRuns where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetWorkflowRuns where
        toHeaders GetWorkflowRuns{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetWorkflowRuns") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetWorkflowRuns where
        toJSON GetWorkflowRuns{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("IncludeGraph" Core..=) Core.<$> includeGraph,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetWorkflowRuns where
        type Rs GetWorkflowRuns = GetWorkflowRunsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetWorkflowRunsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Runs" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetWorkflowRunsResponse' smart constructor.
data GetWorkflowRunsResponse = GetWorkflowRunsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token, if not all requested workflow runs have been returned.
  , runs :: Core.Maybe (Core.NonEmpty Types.WorkflowRun)
    -- ^ A list of workflow run metadata objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetWorkflowRunsResponse' value with any optional fields omitted.
mkGetWorkflowRunsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetWorkflowRunsResponse
mkGetWorkflowRunsResponse responseStatus
  = GetWorkflowRunsResponse'{nextToken = Core.Nothing,
                             runs = Core.Nothing, responseStatus}

-- | A continuation token, if not all requested workflow runs have been returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrrrsNextToken :: Lens.Lens' GetWorkflowRunsResponse (Core.Maybe Types.NextToken)
gwrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gwrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of workflow run metadata objects.
--
-- /Note:/ Consider using 'runs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrrrsRuns :: Lens.Lens' GetWorkflowRunsResponse (Core.Maybe (Core.NonEmpty Types.WorkflowRun))
gwrrrsRuns = Lens.field @"runs"
{-# INLINEABLE gwrrrsRuns #-}
{-# DEPRECATED runs "Use generic-lens or generic-optics with 'runs' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrrrsResponseStatus :: Lens.Lens' GetWorkflowRunsResponse Core.Int
gwrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gwrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
