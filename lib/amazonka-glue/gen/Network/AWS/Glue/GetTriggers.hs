{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTriggers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all the triggers associated with a job.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetTriggers
    (
    -- * Creating a request
      GetTriggers (..)
    , mkGetTriggers
    -- ** Request lenses
    , gtDependentJobName
    , gtMaxResults
    , gtNextToken

    -- * Destructuring the response
    , GetTriggersResponse (..)
    , mkGetTriggersResponse
    -- ** Response lenses
    , gtrhrsNextToken
    , gtrhrsTriggers
    , gtrhrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTriggers' smart constructor.
data GetTriggers = GetTriggers'
  { dependentJobName :: Core.Maybe Types.DependentJobName
    -- ^ The name of the job to retrieve triggers for. The trigger that can start this job is returned, and if there is no such trigger, all triggers are returned.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum size of the response.
  , nextToken :: Core.Maybe Types.GenericString
    -- ^ A continuation token, if this is a continuation call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTriggers' value with any optional fields omitted.
mkGetTriggers
    :: GetTriggers
mkGetTriggers
  = GetTriggers'{dependentJobName = Core.Nothing,
                 maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The name of the job to retrieve triggers for. The trigger that can start this job is returned, and if there is no such trigger, all triggers are returned.
--
-- /Note:/ Consider using 'dependentJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtDependentJobName :: Lens.Lens' GetTriggers (Core.Maybe Types.DependentJobName)
gtDependentJobName = Lens.field @"dependentJobName"
{-# INLINEABLE gtDependentJobName #-}
{-# DEPRECATED dependentJobName "Use generic-lens or generic-optics with 'dependentJobName' instead"  #-}

-- | The maximum size of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtMaxResults :: Lens.Lens' GetTriggers (Core.Maybe Core.Natural)
gtMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gtMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtNextToken :: Lens.Lens' GetTriggers (Core.Maybe Types.GenericString)
gtNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetTriggers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTriggers where
        toHeaders GetTriggers{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetTriggers") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetTriggers where
        toJSON GetTriggers{..}
          = Core.object
              (Core.catMaybes
                 [("DependentJobName" Core..=) Core.<$> dependentJobName,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetTriggers where
        type Rs GetTriggers = GetTriggersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTriggersResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Triggers" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetTriggers where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"triggers" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetTriggersResponse' smart constructor.
data GetTriggersResponse = GetTriggersResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token, if not all the requested triggers have yet been returned.
  , triggers :: Core.Maybe [Types.Trigger]
    -- ^ A list of triggers for the specified job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTriggersResponse' value with any optional fields omitted.
mkGetTriggersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTriggersResponse
mkGetTriggersResponse responseStatus
  = GetTriggersResponse'{nextToken = Core.Nothing,
                         triggers = Core.Nothing, responseStatus}

-- | A continuation token, if not all the requested triggers have yet been returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrhrsNextToken :: Lens.Lens' GetTriggersResponse (Core.Maybe Types.NextToken)
gtrhrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtrhrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of triggers for the specified job.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrhrsTriggers :: Lens.Lens' GetTriggersResponse (Core.Maybe [Types.Trigger])
gtrhrsTriggers = Lens.field @"triggers"
{-# INLINEABLE gtrhrsTriggers #-}
{-# DEPRECATED triggers "Use generic-lens or generic-optics with 'triggers' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrhrsResponseStatus :: Lens.Lens' GetTriggersResponse Core.Int
gtrhrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtrhrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
