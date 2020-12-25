{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListTriggers (..),
    mkListTriggers,

    -- ** Request lenses
    ltDependentJobName,
    ltMaxResults,
    ltNextToken,
    ltTags,

    -- * Destructuring the response
    ListTriggersResponse (..),
    mkListTriggersResponse,

    -- ** Response lenses
    ltrrsNextToken,
    ltrrsTriggerNames,
    ltrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTriggers' smart constructor.
data ListTriggers = ListTriggers'
  { -- | The name of the job for which to retrieve triggers. The trigger that can start this job is returned. If there is no such trigger, all triggers are returned.
    dependentJobName :: Core.Maybe Types.DependentJobName,
    -- | The maximum size of a list to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | A continuation token, if this is a continuation request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Specifies to return only these tagged resources.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTriggers' value with any optional fields omitted.
mkListTriggers ::
  ListTriggers
mkListTriggers =
  ListTriggers'
    { dependentJobName = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the job for which to retrieve triggers. The trigger that can start this job is returned. If there is no such trigger, all triggers are returned.
--
-- /Note:/ Consider using 'dependentJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltDependentJobName :: Lens.Lens' ListTriggers (Core.Maybe Types.DependentJobName)
ltDependentJobName = Lens.field @"dependentJobName"
{-# DEPRECATED ltDependentJobName "Use generic-lens or generic-optics with 'dependentJobName' instead." #-}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTriggers (Core.Maybe Core.Natural)
ltMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTriggers (Core.Maybe Types.NextToken)
ltNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies to return only these tagged resources.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTags :: Lens.Lens' ListTriggers (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ltTags = Lens.field @"tags"
{-# DEPRECATED ltTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON ListTriggers where
  toJSON ListTriggers {..} =
    Core.object
      ( Core.catMaybes
          [ ("DependentJobName" Core..=) Core.<$> dependentJobName,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest ListTriggers where
  type Rs ListTriggers = ListTriggersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.ListTriggers")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTriggersResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "TriggerNames")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListTriggersResponse' smart constructor.
data ListTriggersResponse = ListTriggersResponse'
  { -- | A continuation token, if the returned list does not contain the last metric available.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The names of all triggers in the account, or the triggers with the specified tags.
    triggerNames :: Core.Maybe [Types.NameString],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTriggersResponse' value with any optional fields omitted.
mkListTriggersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTriggersResponse
mkListTriggersResponse responseStatus =
  ListTriggersResponse'
    { nextToken = Core.Nothing,
      triggerNames = Core.Nothing,
      responseStatus
    }

-- | A continuation token, if the returned list does not contain the last metric available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListTriggersResponse (Core.Maybe Types.NextToken)
ltrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of all triggers in the account, or the triggers with the specified tags.
--
-- /Note:/ Consider using 'triggerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTriggerNames :: Lens.Lens' ListTriggersResponse (Core.Maybe [Types.NameString])
ltrrsTriggerNames = Lens.field @"triggerNames"
{-# DEPRECATED ltrrsTriggerNames "Use generic-lens or generic-optics with 'triggerNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTriggersResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
