{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.ListTagsForDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns all of the tags that are associated with the specified domain.
--
-- All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.
module Network.AWS.Route53Domains.ListTagsForDomain
    (
    -- * Creating a request
      ListTagsForDomain (..)
    , mkListTagsForDomain
    -- ** Request lenses
    , ltfdDomainName

    -- * Destructuring the response
    , ListTagsForDomainResponse (..)
    , mkListTagsForDomainResponse
    -- ** Response lenses
    , ltfdrrsTagList
    , ltfdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The ListTagsForDomainRequest includes the following elements.
--
-- /See:/ 'mkListTagsForDomain' smart constructor.
newtype ListTagsForDomain = ListTagsForDomain'
  { domainName :: Types.DomainName
    -- ^ The domain for which you want to get a list of tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForDomain' value with any optional fields omitted.
mkListTagsForDomain
    :: Types.DomainName -- ^ 'domainName'
    -> ListTagsForDomain
mkListTagsForDomain domainName = ListTagsForDomain'{domainName}

-- | The domain for which you want to get a list of tags.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdDomainName :: Lens.Lens' ListTagsForDomain Types.DomainName
ltfdDomainName = Lens.field @"domainName"
{-# INLINEABLE ltfdDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery ListTagsForDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTagsForDomain where
        toHeaders ListTagsForDomain{..}
          = Core.pure
              ("X-Amz-Target", "Route53Domains_v20140515.ListTagsForDomain")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTagsForDomain where
        toJSON ListTagsForDomain{..}
          = Core.object
              (Core.catMaybes [Core.Just ("DomainName" Core..= domainName)])

instance Core.AWSRequest ListTagsForDomain where
        type Rs ListTagsForDomain = ListTagsForDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTagsForDomainResponse' Core.<$>
                   (x Core..:? "TagList" Core..!= Core.mempty) Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The ListTagsForDomain response includes the following elements.
--
-- /See:/ 'mkListTagsForDomainResponse' smart constructor.
data ListTagsForDomainResponse = ListTagsForDomainResponse'
  { tagList :: [Types.Tag]
    -- ^ A list of the tags that are associated with the specified domain.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForDomainResponse' value with any optional fields omitted.
mkListTagsForDomainResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTagsForDomainResponse
mkListTagsForDomainResponse responseStatus
  = ListTagsForDomainResponse'{tagList = Core.mempty, responseStatus}

-- | A list of the tags that are associated with the specified domain.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdrrsTagList :: Lens.Lens' ListTagsForDomainResponse [Types.Tag]
ltfdrrsTagList = Lens.field @"tagList"
{-# INLINEABLE ltfdrrsTagList #-}
{-# DEPRECATED tagList "Use generic-lens or generic-optics with 'tagList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdrrsResponseStatus :: Lens.Lens' ListTagsForDomainResponse Core.Int
ltfdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltfdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
