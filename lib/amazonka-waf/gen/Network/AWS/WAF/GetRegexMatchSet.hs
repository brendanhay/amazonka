{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetRegexMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'RegexMatchSet' specified by @RegexMatchSetId@ .
module Network.AWS.WAF.GetRegexMatchSet
    (
    -- * Creating a request
      GetRegexMatchSet (..)
    , mkGetRegexMatchSet
    -- ** Request lenses
    , grmsRegexMatchSetId

    -- * Destructuring the response
    , GetRegexMatchSetResponse (..)
    , mkGetRegexMatchSetResponse
    -- ** Response lenses
    , grmsrrsRegexMatchSet
    , grmsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkGetRegexMatchSet' smart constructor.
newtype GetRegexMatchSet = GetRegexMatchSet'
  { regexMatchSetId :: Types.ResourceId
    -- ^ The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to get. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRegexMatchSet' value with any optional fields omitted.
mkGetRegexMatchSet
    :: Types.ResourceId -- ^ 'regexMatchSetId'
    -> GetRegexMatchSet
mkGetRegexMatchSet regexMatchSetId
  = GetRegexMatchSet'{regexMatchSetId}

-- | The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to get. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
--
-- /Note:/ Consider using 'regexMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grmsRegexMatchSetId :: Lens.Lens' GetRegexMatchSet Types.ResourceId
grmsRegexMatchSetId = Lens.field @"regexMatchSetId"
{-# INLINEABLE grmsRegexMatchSetId #-}
{-# DEPRECATED regexMatchSetId "Use generic-lens or generic-optics with 'regexMatchSetId' instead"  #-}

instance Core.ToQuery GetRegexMatchSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRegexMatchSet where
        toHeaders GetRegexMatchSet{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.GetRegexMatchSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetRegexMatchSet where
        toJSON GetRegexMatchSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RegexMatchSetId" Core..= regexMatchSetId)])

instance Core.AWSRequest GetRegexMatchSet where
        type Rs GetRegexMatchSet = GetRegexMatchSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRegexMatchSetResponse' Core.<$>
                   (x Core..:? "RegexMatchSet") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetRegexMatchSetResponse' smart constructor.
data GetRegexMatchSetResponse = GetRegexMatchSetResponse'
  { regexMatchSet :: Core.Maybe Types.RegexMatchSet
    -- ^ Information about the 'RegexMatchSet' that you specified in the @GetRegexMatchSet@ request. For more information, see 'RegexMatchTuple' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRegexMatchSetResponse' value with any optional fields omitted.
mkGetRegexMatchSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRegexMatchSetResponse
mkGetRegexMatchSetResponse responseStatus
  = GetRegexMatchSetResponse'{regexMatchSet = Core.Nothing,
                              responseStatus}

-- | Information about the 'RegexMatchSet' that you specified in the @GetRegexMatchSet@ request. For more information, see 'RegexMatchTuple' .
--
-- /Note:/ Consider using 'regexMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grmsrrsRegexMatchSet :: Lens.Lens' GetRegexMatchSetResponse (Core.Maybe Types.RegexMatchSet)
grmsrrsRegexMatchSet = Lens.field @"regexMatchSet"
{-# INLINEABLE grmsrrsRegexMatchSet #-}
{-# DEPRECATED regexMatchSet "Use generic-lens or generic-optics with 'regexMatchSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grmsrrsResponseStatus :: Lens.Lens' GetRegexMatchSetResponse Core.Int
grmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
