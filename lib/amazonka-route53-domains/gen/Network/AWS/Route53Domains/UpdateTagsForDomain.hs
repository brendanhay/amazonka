{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.UpdateTagsForDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation adds or updates tags for a specified domain.
--
-- All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.
module Network.AWS.Route53Domains.UpdateTagsForDomain
    (
    -- * Creating a request
      UpdateTagsForDomain (..)
    , mkUpdateTagsForDomain
    -- ** Request lenses
    , utfdDomainName
    , utfdTagsToUpdate

    -- * Destructuring the response
    , UpdateTagsForDomainResponse (..)
    , mkUpdateTagsForDomainResponse
    -- ** Response lenses
    , utfdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The UpdateTagsForDomainRequest includes the following elements.
--
-- /See:/ 'mkUpdateTagsForDomain' smart constructor.
data UpdateTagsForDomain = UpdateTagsForDomain'
  { domainName :: Types.DomainName
    -- ^ The domain for which you want to add or update tags.
  , tagsToUpdate :: Core.Maybe [Types.Tag]
    -- ^ A list of the tag keys and values that you want to add or update. If you specify a key that already exists, the corresponding value will be replaced.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTagsForDomain' value with any optional fields omitted.
mkUpdateTagsForDomain
    :: Types.DomainName -- ^ 'domainName'
    -> UpdateTagsForDomain
mkUpdateTagsForDomain domainName
  = UpdateTagsForDomain'{domainName, tagsToUpdate = Core.Nothing}

-- | The domain for which you want to add or update tags.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utfdDomainName :: Lens.Lens' UpdateTagsForDomain Types.DomainName
utfdDomainName = Lens.field @"domainName"
{-# INLINEABLE utfdDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | A list of the tag keys and values that you want to add or update. If you specify a key that already exists, the corresponding value will be replaced.
--
-- /Note:/ Consider using 'tagsToUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utfdTagsToUpdate :: Lens.Lens' UpdateTagsForDomain (Core.Maybe [Types.Tag])
utfdTagsToUpdate = Lens.field @"tagsToUpdate"
{-# INLINEABLE utfdTagsToUpdate #-}
{-# DEPRECATED tagsToUpdate "Use generic-lens or generic-optics with 'tagsToUpdate' instead"  #-}

instance Core.ToQuery UpdateTagsForDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateTagsForDomain where
        toHeaders UpdateTagsForDomain{..}
          = Core.pure
              ("X-Amz-Target", "Route53Domains_v20140515.UpdateTagsForDomain")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateTagsForDomain where
        toJSON UpdateTagsForDomain{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainName" Core..= domainName),
                  ("TagsToUpdate" Core..=) Core.<$> tagsToUpdate])

instance Core.AWSRequest UpdateTagsForDomain where
        type Rs UpdateTagsForDomain = UpdateTagsForDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateTagsForDomainResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateTagsForDomainResponse' smart constructor.
newtype UpdateTagsForDomainResponse = UpdateTagsForDomainResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTagsForDomainResponse' value with any optional fields omitted.
mkUpdateTagsForDomainResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateTagsForDomainResponse
mkUpdateTagsForDomainResponse responseStatus
  = UpdateTagsForDomainResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utfdrrsResponseStatus :: Lens.Lens' UpdateTagsForDomainResponse Core.Int
utfdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utfdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
