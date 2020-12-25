{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.ListTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the message templates that are associated with your Amazon Pinpoint account.
module Network.AWS.Pinpoint.ListTemplates
  ( -- * Creating a request
    ListTemplates (..),
    mkListTemplates,

    -- ** Request lenses
    ltNextToken,
    ltPageSize,
    ltPrefix,
    ltTemplateType,

    -- * Destructuring the response
    ListTemplatesResponse (..),
    mkListTemplatesResponse,

    -- ** Response lenses
    ltrrsTemplatesResponse,
    ltrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTemplates' smart constructor.
data ListTemplates = ListTemplates'
  { -- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The substring to match in the names of the message templates to include in the results. If you specify this value, Amazon Pinpoint returns only those templates whose names begin with the value that you specify.
    prefix :: Core.Maybe Core.Text,
    -- | The type of message template to include in the results. Valid values are: EMAIL, PUSH, SMS, and VOICE. To include all types of templates in the results, don't include this parameter in your request.
    templateType :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTemplates' value with any optional fields omitted.
mkListTemplates ::
  ListTemplates
mkListTemplates =
  ListTemplates'
    { nextToken = Core.Nothing,
      pageSize = Core.Nothing,
      prefix = Core.Nothing,
      templateType = Core.Nothing
    }

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTemplates (Core.Maybe Core.Text)
ltNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltPageSize :: Lens.Lens' ListTemplates (Core.Maybe Core.Text)
ltPageSize = Lens.field @"pageSize"
{-# DEPRECATED ltPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The substring to match in the names of the message templates to include in the results. If you specify this value, Amazon Pinpoint returns only those templates whose names begin with the value that you specify.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltPrefix :: Lens.Lens' ListTemplates (Core.Maybe Core.Text)
ltPrefix = Lens.field @"prefix"
{-# DEPRECATED ltPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The type of message template to include in the results. Valid values are: EMAIL, PUSH, SMS, and VOICE. To include all types of templates in the results, don't include this parameter in your request.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTemplateType :: Lens.Lens' ListTemplates (Core.Maybe Core.Text)
ltTemplateType = Lens.field @"templateType"
{-# DEPRECATED ltTemplateType "Use generic-lens or generic-optics with 'templateType' instead." #-}

instance Core.AWSRequest ListTemplates where
  type Rs ListTemplates = ListTemplatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/v1/templates",
        Core._rqQuery =
          Core.toQueryValue "next-token" Core.<$> nextToken
            Core.<> (Core.toQueryValue "page-size" Core.<$> pageSize)
            Core.<> (Core.toQueryValue "prefix" Core.<$> prefix)
            Core.<> (Core.toQueryValue "template-type" Core.<$> templateType),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTemplatesResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListTemplatesResponse' smart constructor.
data ListTemplatesResponse = ListTemplatesResponse'
  { templatesResponse :: Types.TemplatesResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTemplatesResponse' value with any optional fields omitted.
mkListTemplatesResponse ::
  -- | 'templatesResponse'
  Types.TemplatesResponse ->
  -- | 'responseStatus'
  Core.Int ->
  ListTemplatesResponse
mkListTemplatesResponse templatesResponse responseStatus =
  ListTemplatesResponse' {templatesResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'templatesResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTemplatesResponse :: Lens.Lens' ListTemplatesResponse Types.TemplatesResponse
ltrrsTemplatesResponse = Lens.field @"templatesResponse"
{-# DEPRECATED ltrrsTemplatesResponse "Use generic-lens or generic-optics with 'templatesResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTemplatesResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
