{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListTriggers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of all trigger resources in this AWS account, or the resources with the specified tag. This operation allows you to see which resources are available in your account, and their names.
--
-- This operation takes the optional @Tags@ field, which you can use as a filter on the response so that tagged resources can be retrieved as a group. If you choose to use tags filtering, only resources with the tag are retrieved.
module Network.AWS.Glue.ListTriggers
    (
    -- * Creating a request
      ListTriggers (..)
    , mkListTriggers
    -- ** Request lenses
    , ltDependentJobName
    , ltMaxResults
    , ltNextToken
    , ltTags

    -- * Destructuring the response
    , ListTriggersResponse (..)
    , mkListTriggersResponse
    -- ** Response lenses
    , ltrrsNextToken
    , ltrrsTriggerNames
    , ltrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTriggers' smart constructor.
data ListTriggers = ListTriggers'
  { dependentJobName :: Core.Maybe Types.DependentJobName
    -- ^ The name of the job for which to retrieve triggers. The trigger that can start this job is returned. If there is no such trigger, all triggers are returned.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum size of a list to return.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token, if this is a continuation request.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ Specifies to return only these tagged resources.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTriggers' value with any optional fields omitted.
mkListTriggers
    :: ListTriggers
mkListTriggers
  = ListTriggers'{dependentJobName = Core.Nothing,
                  maxResults = Core.Nothing, nextToken = Core.Nothing,
                  tags = Core.Nothing}

-- | The name of the job for which to retrieve triggers. The trigger that can start this job is returned. If there is no such trigger, all triggers are returned.
--
-- /Note:/ Consider using 'dependentJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltDependentJobName :: Lens.Lens' ListTriggers (Core.Maybe Types.DependentJobName)
ltDependentJobName = Lens.field @"dependentJobName"
{-# INLINEABLE ltDependentJobName #-}
{-# DEPRECATED dependentJobName "Use generic-lens or generic-optics with 'dependentJobName' instead"  #-}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTriggers (Core.Maybe Core.Natural)
ltMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTriggers (Core.Maybe Types.NextToken)
ltNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Specifies to return only these tagged resources.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTags :: Lens.Lens' ListTriggers (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ltTags = Lens.field @"tags"
{-# INLINEABLE ltTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery ListTriggers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTriggers where
        toHeaders ListTriggers{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.ListTriggers") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTriggers where
        toJSON ListTriggers{..}
          = Core.object
              (Core.catMaybes
                 [("DependentJobName" Core..=) Core.<$> dependentJobName,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest ListTriggers where
        type Rs ListTriggers = ListTriggersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTriggersResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "TriggerNames"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTriggersResponse' smart constructor.
data ListTriggersResponse = ListTriggersResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token, if the returned list does not contain the last metric available.
  , triggerNames :: Core.Maybe [Types.NameString]
    -- ^ The names of all triggers in the account, or the triggers with the specified tags.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTriggersResponse' value with any optional fields omitted.
mkListTriggersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTriggersResponse
mkListTriggersResponse responseStatus
  = ListTriggersResponse'{nextToken = Core.Nothing,
                          triggerNames = Core.Nothing, responseStatus}

-- | A continuation token, if the returned list does not contain the last metric available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListTriggersResponse (Core.Maybe Types.NextToken)
ltrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The names of all triggers in the account, or the triggers with the specified tags.
--
-- /Note:/ Consider using 'triggerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTriggerNames :: Lens.Lens' ListTriggersResponse (Core.Maybe [Types.NameString])
ltrrsTriggerNames = Lens.field @"triggerNames"
{-# INLINEABLE ltrrsTriggerNames #-}
{-# DEPRECATED triggerNames "Use generic-lens or generic-optics with 'triggerNames' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTriggersResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
