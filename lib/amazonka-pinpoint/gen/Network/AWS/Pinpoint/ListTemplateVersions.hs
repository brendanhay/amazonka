{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListTemplateVersions (..)
    , mkListTemplateVersions
    -- ** Request lenses
    , ltvTemplateName
    , ltvTemplateType
    , ltvNextToken
    , ltvPageSize

    -- * Destructuring the response
    , ListTemplateVersionsResponse (..)
    , mkListTemplateVersionsResponse
    -- ** Response lenses
    , ltvrrsTemplateVersionsResponse
    , ltvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTemplateVersions' smart constructor.
data ListTemplateVersions = ListTemplateVersions'
  { templateName :: Core.Text
    -- ^ The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
  , templateType :: Core.Text
    -- ^ The type of channel that the message template is designed for. Valid values are: EMAIL, PUSH, SMS, and VOICE.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
  , pageSize :: Core.Maybe Core.Text
    -- ^ The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTemplateVersions' value with any optional fields omitted.
mkListTemplateVersions
    :: Core.Text -- ^ 'templateName'
    -> Core.Text -- ^ 'templateType'
    -> ListTemplateVersions
mkListTemplateVersions templateName templateType
  = ListTemplateVersions'{templateName, templateType,
                          nextToken = Core.Nothing, pageSize = Core.Nothing}

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvTemplateName :: Lens.Lens' ListTemplateVersions Core.Text
ltvTemplateName = Lens.field @"templateName"
{-# INLINEABLE ltvTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The type of channel that the message template is designed for. Valid values are: EMAIL, PUSH, SMS, and VOICE.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvTemplateType :: Lens.Lens' ListTemplateVersions Core.Text
ltvTemplateType = Lens.field @"templateType"
{-# INLINEABLE ltvTemplateType #-}
{-# DEPRECATED templateType "Use generic-lens or generic-optics with 'templateType' instead"  #-}

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvNextToken :: Lens.Lens' ListTemplateVersions (Core.Maybe Core.Text)
ltvNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltvNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvPageSize :: Lens.Lens' ListTemplateVersions (Core.Maybe Core.Text)
ltvPageSize = Lens.field @"pageSize"
{-# INLINEABLE ltvPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

instance Core.ToQuery ListTemplateVersions where
        toQuery ListTemplateVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "next-token") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "page-size") pageSize

instance Core.ToHeaders ListTemplateVersions where
        toHeaders ListTemplateVersions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListTemplateVersions where
        type Rs ListTemplateVersions = ListTemplateVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/templates/" Core.<> Core.toText templateName Core.<> "/"
                             Core.<> Core.toText templateType
                             Core.<> "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTemplateVersionsResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTemplateVersionsResponse' smart constructor.
data ListTemplateVersionsResponse = ListTemplateVersionsResponse'
  { templateVersionsResponse :: Types.TemplateVersionsResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTemplateVersionsResponse' value with any optional fields omitted.
mkListTemplateVersionsResponse
    :: Types.TemplateVersionsResponse -- ^ 'templateVersionsResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> ListTemplateVersionsResponse
mkListTemplateVersionsResponse templateVersionsResponse
  responseStatus
  = ListTemplateVersionsResponse'{templateVersionsResponse,
                                  responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'templateVersionsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvrrsTemplateVersionsResponse :: Lens.Lens' ListTemplateVersionsResponse Types.TemplateVersionsResponse
ltvrrsTemplateVersionsResponse = Lens.field @"templateVersionsResponse"
{-# INLINEABLE ltvrrsTemplateVersionsResponse #-}
{-# DEPRECATED templateVersionsResponse "Use generic-lens or generic-optics with 'templateVersionsResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvrrsResponseStatus :: Lens.Lens' ListTemplateVersionsResponse Core.Int
ltvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
