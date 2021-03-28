{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.ListTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all tags for the given Elasticsearch domain.
module Network.AWS.ElasticSearch.ListTags
    (
    -- * Creating a request
      ListTags (..)
    , mkListTags
    -- ** Request lenses
    , ltARN

    -- * Destructuring the response
    , ListTagsResponse (..)
    , mkListTagsResponse
    -- ** Response lenses
    , ltrrsTagList
    , ltrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'ListTags' @ operation. Specify the @ARN@ for the Elasticsearch domain to which the tags are attached that you want to view are attached.
--
-- /See:/ 'mkListTags' smart constructor.
newtype ListTags = ListTags'
  { arn :: Types.ARN
    -- ^ Specify the @ARN@ for the Elasticsearch domain to which the tags are attached that you want to view.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListTags' value with any optional fields omitted.
mkListTags
    :: Types.ARN -- ^ 'arn'
    -> ListTags
mkListTags arn = ListTags'{arn}

-- | Specify the @ARN@ for the Elasticsearch domain to which the tags are attached that you want to view.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltARN :: Lens.Lens' ListTags Types.ARN
ltARN = Lens.field @"arn"
{-# INLINEABLE ltARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery ListTags where
        toQuery ListTags{..} = Core.toQueryPair "arn" arn

instance Core.ToHeaders ListTags where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListTags where
        type Rs ListTags = ListTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/2015-01-01/tags/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTagsResponse' Core.<$>
                   (x Core..:? "TagList") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @ListTags@ operation. Contains tags for all requested Elasticsearch domains.
--
-- /See:/ 'mkListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { tagList :: Core.Maybe [Types.Tag]
    -- ^ List of @Tag@ for the requested Elasticsearch domain.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsResponse' value with any optional fields omitted.
mkListTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTagsResponse
mkListTagsResponse responseStatus
  = ListTagsResponse'{tagList = Core.Nothing, responseStatus}

-- | List of @Tag@ for the requested Elasticsearch domain.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTagList :: Lens.Lens' ListTagsResponse (Core.Maybe [Types.Tag])
ltrrsTagList = Lens.field @"tagList"
{-# INLINEABLE ltrrsTagList #-}
{-# DEPRECATED tagList "Use generic-lens or generic-optics with 'tagList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTagsResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
