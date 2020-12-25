{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.ListTemplateVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the versions of a specific message template.
module Network.AWS.Pinpoint.ListTemplateVersions
  ( -- * Creating a request
    ListTemplateVersions (..),
    mkListTemplateVersions,

    -- ** Request lenses
    ltvTemplateName,
    ltvTemplateType,
    ltvNextToken,
    ltvPageSize,

    -- * Destructuring the response
    ListTemplateVersionsResponse (..),
    mkListTemplateVersionsResponse,

    -- ** Response lenses
    ltvrrsTemplateVersionsResponse,
    ltvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTemplateVersions' smart constructor.
data ListTemplateVersions = ListTemplateVersions'
  { -- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
    templateName :: Core.Text,
    -- | The type of channel that the message template is designed for. Valid values are: EMAIL, PUSH, SMS, and VOICE.
    templateType :: Core.Text,
    -- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTemplateVersions' value with any optional fields omitted.
mkListTemplateVersions ::
  -- | 'templateName'
  Core.Text ->
  -- | 'templateType'
  Core.Text ->
  ListTemplateVersions
mkListTemplateVersions templateName templateType =
  ListTemplateVersions'
    { templateName,
      templateType,
      nextToken = Core.Nothing,
      pageSize = Core.Nothing
    }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvTemplateName :: Lens.Lens' ListTemplateVersions Core.Text
ltvTemplateName = Lens.field @"templateName"
{-# DEPRECATED ltvTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The type of channel that the message template is designed for. Valid values are: EMAIL, PUSH, SMS, and VOICE.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvTemplateType :: Lens.Lens' ListTemplateVersions Core.Text
ltvTemplateType = Lens.field @"templateType"
{-# DEPRECATED ltvTemplateType "Use generic-lens or generic-optics with 'templateType' instead." #-}

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvNextToken :: Lens.Lens' ListTemplateVersions (Core.Maybe Core.Text)
ltvNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvPageSize :: Lens.Lens' ListTemplateVersions (Core.Maybe Core.Text)
ltvPageSize = Lens.field @"pageSize"
{-# DEPRECATED ltvPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Core.AWSRequest ListTemplateVersions where
  type Rs ListTemplateVersions = ListTemplateVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/templates/" Core.<> (Core.toText templateName) Core.<> ("/")
                Core.<> (Core.toText templateType)
                Core.<> ("/versions")
            ),
        Core._rqQuery =
          Core.toQueryValue "next-token" Core.<$> nextToken
            Core.<> (Core.toQueryValue "page-size" Core.<$> pageSize),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTemplateVersionsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListTemplateVersionsResponse' smart constructor.
data ListTemplateVersionsResponse = ListTemplateVersionsResponse'
  { templateVersionsResponse :: Types.TemplateVersionsResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTemplateVersionsResponse' value with any optional fields omitted.
mkListTemplateVersionsResponse ::
  -- | 'templateVersionsResponse'
  Types.TemplateVersionsResponse ->
  -- | 'responseStatus'
  Core.Int ->
  ListTemplateVersionsResponse
mkListTemplateVersionsResponse
  templateVersionsResponse
  responseStatus =
    ListTemplateVersionsResponse'
      { templateVersionsResponse,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'templateVersionsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvrrsTemplateVersionsResponse :: Lens.Lens' ListTemplateVersionsResponse Types.TemplateVersionsResponse
ltvrrsTemplateVersionsResponse = Lens.field @"templateVersionsResponse"
{-# DEPRECATED ltvrrsTemplateVersionsResponse "Use generic-lens or generic-optics with 'templateVersionsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvrrsResponseStatus :: Lens.Lens' ListTemplateVersionsResponse Core.Int
ltvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
