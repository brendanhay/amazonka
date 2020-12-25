{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.ListJobTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your job templates. This will return the templates themselves, not just a list of them. To retrieve the next twenty templates, use the nextToken string returned with the array
--
-- This operation returns paginated results.
module Network.AWS.MediaConvert.ListJobTemplates
  ( -- * Creating a request
    ListJobTemplates (..),
    mkListJobTemplates,

    -- ** Request lenses
    ljtCategory,
    ljtListBy,
    ljtMaxResults,
    ljtNextToken,
    ljtOrder,

    -- * Destructuring the response
    ListJobTemplatesResponse (..),
    mkListJobTemplatesResponse,

    -- ** Response lenses
    ljtrrsJobTemplates,
    ljtrrsNextToken,
    ljtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListJobTemplates' smart constructor.
data ListJobTemplates = ListJobTemplates'
  { -- | Optionally, specify a job template category to limit responses to only job templates from that category.
    category :: Core.Maybe Core.Text,
    -- | Optional. When you request a list of job templates, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
    listBy :: Core.Maybe Types.JobTemplateListBy,
    -- | Optional. Number of job templates, up to twenty, that will be returned at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | Use this string, provided with the response to a previous request, to request the next batch of job templates.
    nextToken :: Core.Maybe Core.Text,
    -- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
    order :: Core.Maybe Types.Order
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJobTemplates' value with any optional fields omitted.
mkListJobTemplates ::
  ListJobTemplates
mkListJobTemplates =
  ListJobTemplates'
    { category = Core.Nothing,
      listBy = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      order = Core.Nothing
    }

-- | Optionally, specify a job template category to limit responses to only job templates from that category.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtCategory :: Lens.Lens' ListJobTemplates (Core.Maybe Core.Text)
ljtCategory = Lens.field @"category"
{-# DEPRECATED ljtCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | Optional. When you request a list of job templates, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
--
-- /Note:/ Consider using 'listBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtListBy :: Lens.Lens' ListJobTemplates (Core.Maybe Types.JobTemplateListBy)
ljtListBy = Lens.field @"listBy"
{-# DEPRECATED ljtListBy "Use generic-lens or generic-optics with 'listBy' instead." #-}

-- | Optional. Number of job templates, up to twenty, that will be returned at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtMaxResults :: Lens.Lens' ListJobTemplates (Core.Maybe Core.Natural)
ljtMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ljtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Use this string, provided with the response to a previous request, to request the next batch of job templates.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtNextToken :: Lens.Lens' ListJobTemplates (Core.Maybe Core.Text)
ljtNextToken = Lens.field @"nextToken"
{-# DEPRECATED ljtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtOrder :: Lens.Lens' ListJobTemplates (Core.Maybe Types.Order)
ljtOrder = Lens.field @"order"
{-# DEPRECATED ljtOrder "Use generic-lens or generic-optics with 'order' instead." #-}

instance Core.AWSRequest ListJobTemplates where
  type Rs ListJobTemplates = ListJobTemplatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2017-08-29/jobTemplates",
        Core._rqQuery =
          Core.toQueryValue "category" Core.<$> category
            Core.<> (Core.toQueryValue "listBy" Core.<$> listBy)
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "order" Core.<$> order),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobTemplatesResponse'
            Core.<$> (x Core..:? "jobTemplates")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListJobTemplates where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"jobTemplates" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListJobTemplatesResponse' smart constructor.
data ListJobTemplatesResponse = ListJobTemplatesResponse'
  { -- | List of Job templates.
    jobTemplates :: Core.Maybe [Types.JobTemplate],
    -- | Use this string to request the next batch of job templates.
    nextToken :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListJobTemplatesResponse' value with any optional fields omitted.
mkListJobTemplatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListJobTemplatesResponse
mkListJobTemplatesResponse responseStatus =
  ListJobTemplatesResponse'
    { jobTemplates = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | List of Job templates.
--
-- /Note:/ Consider using 'jobTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtrrsJobTemplates :: Lens.Lens' ListJobTemplatesResponse (Core.Maybe [Types.JobTemplate])
ljtrrsJobTemplates = Lens.field @"jobTemplates"
{-# DEPRECATED ljtrrsJobTemplates "Use generic-lens or generic-optics with 'jobTemplates' instead." #-}

-- | Use this string to request the next batch of job templates.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtrrsNextToken :: Lens.Lens' ListJobTemplatesResponse (Core.Maybe Core.Text)
ljtrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ljtrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtrrsResponseStatus :: Lens.Lens' ListJobTemplatesResponse Core.Int
ljtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ljtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
