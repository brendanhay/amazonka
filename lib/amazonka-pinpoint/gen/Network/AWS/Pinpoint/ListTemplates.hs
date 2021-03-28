{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListTemplates (..)
    , mkListTemplates
    -- ** Request lenses
    , ltNextToken
    , ltPageSize
    , ltPrefix
    , ltTemplateType

    -- * Destructuring the response
    , ListTemplatesResponse (..)
    , mkListTemplatesResponse
    -- ** Response lenses
    , ltrrsTemplatesResponse
    , ltrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTemplates' smart constructor.
data ListTemplates = ListTemplates'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
  , pageSize :: Core.Maybe Core.Text
    -- ^ The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
  , prefix :: Core.Maybe Core.Text
    -- ^ The substring to match in the names of the message templates to include in the results. If you specify this value, Amazon Pinpoint returns only those templates whose names begin with the value that you specify.
  , templateType :: Core.Maybe Core.Text
    -- ^ The type of message template to include in the results. Valid values are: EMAIL, PUSH, SMS, and VOICE. To include all types of templates in the results, don't include this parameter in your request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTemplates' value with any optional fields omitted.
mkListTemplates
    :: ListTemplates
mkListTemplates
  = ListTemplates'{nextToken = Core.Nothing, pageSize = Core.Nothing,
                   prefix = Core.Nothing, templateType = Core.Nothing}

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTemplates (Core.Maybe Core.Text)
ltNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltPageSize :: Lens.Lens' ListTemplates (Core.Maybe Core.Text)
ltPageSize = Lens.field @"pageSize"
{-# INLINEABLE ltPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The substring to match in the names of the message templates to include in the results. If you specify this value, Amazon Pinpoint returns only those templates whose names begin with the value that you specify.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltPrefix :: Lens.Lens' ListTemplates (Core.Maybe Core.Text)
ltPrefix = Lens.field @"prefix"
{-# INLINEABLE ltPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | The type of message template to include in the results. Valid values are: EMAIL, PUSH, SMS, and VOICE. To include all types of templates in the results, don't include this parameter in your request.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTemplateType :: Lens.Lens' ListTemplates (Core.Maybe Core.Text)
ltTemplateType = Lens.field @"templateType"
{-# INLINEABLE ltTemplateType #-}
{-# DEPRECATED templateType "Use generic-lens or generic-optics with 'templateType' instead"  #-}

instance Core.ToQuery ListTemplates where
        toQuery ListTemplates{..}
          = Core.maybe Core.mempty (Core.toQueryPair "next-token") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "page-size") pageSize
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "prefix") prefix
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "template-type")
                templateType

instance Core.ToHeaders ListTemplates where
        toHeaders ListTemplates{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListTemplates where
        type Rs ListTemplates = ListTemplatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/v1/templates",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTemplatesResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTemplatesResponse' smart constructor.
data ListTemplatesResponse = ListTemplatesResponse'
  { templatesResponse :: Types.TemplatesResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTemplatesResponse' value with any optional fields omitted.
mkListTemplatesResponse
    :: Types.TemplatesResponse -- ^ 'templatesResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> ListTemplatesResponse
mkListTemplatesResponse templatesResponse responseStatus
  = ListTemplatesResponse'{templatesResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'templatesResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTemplatesResponse :: Lens.Lens' ListTemplatesResponse Types.TemplatesResponse
ltrrsTemplatesResponse = Lens.field @"templatesResponse"
{-# INLINEABLE ltrrsTemplatesResponse #-}
{-# DEPRECATED templatesResponse "Use generic-lens or generic-optics with 'templatesResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTemplatesResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
