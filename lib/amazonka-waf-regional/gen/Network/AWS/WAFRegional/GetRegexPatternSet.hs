{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetRegexPatternSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'RegexPatternSet' specified by @RegexPatternSetId@ .
module Network.AWS.WAFRegional.GetRegexPatternSet
    (
    -- * Creating a request
      GetRegexPatternSet (..)
    , mkGetRegexPatternSet
    -- ** Request lenses
    , grpsRegexPatternSetId

    -- * Destructuring the response
    , GetRegexPatternSetResponse (..)
    , mkGetRegexPatternSetResponse
    -- ** Response lenses
    , grpsrrsRegexPatternSet
    , grpsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkGetRegexPatternSet' smart constructor.
newtype GetRegexPatternSet = GetRegexPatternSet'
  { regexPatternSetId :: Types.ResourceId
    -- ^ The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to get. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRegexPatternSet' value with any optional fields omitted.
mkGetRegexPatternSet
    :: Types.ResourceId -- ^ 'regexPatternSetId'
    -> GetRegexPatternSet
mkGetRegexPatternSet regexPatternSetId
  = GetRegexPatternSet'{regexPatternSetId}

-- | The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to get. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- /Note:/ Consider using 'regexPatternSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpsRegexPatternSetId :: Lens.Lens' GetRegexPatternSet Types.ResourceId
grpsRegexPatternSetId = Lens.field @"regexPatternSetId"
{-# INLINEABLE grpsRegexPatternSetId #-}
{-# DEPRECATED regexPatternSetId "Use generic-lens or generic-optics with 'regexPatternSetId' instead"  #-}

instance Core.ToQuery GetRegexPatternSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRegexPatternSet where
        toHeaders GetRegexPatternSet{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.GetRegexPatternSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetRegexPatternSet where
        toJSON GetRegexPatternSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RegexPatternSetId" Core..= regexPatternSetId)])

instance Core.AWSRequest GetRegexPatternSet where
        type Rs GetRegexPatternSet = GetRegexPatternSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRegexPatternSetResponse' Core.<$>
                   (x Core..:? "RegexPatternSet") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetRegexPatternSetResponse' smart constructor.
data GetRegexPatternSetResponse = GetRegexPatternSetResponse'
  { regexPatternSet :: Core.Maybe Types.RegexPatternSet
    -- ^ Information about the 'RegexPatternSet' that you specified in the @GetRegexPatternSet@ request, including the identifier of the pattern set and the regular expression patterns you want AWS WAF to search for. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRegexPatternSetResponse' value with any optional fields omitted.
mkGetRegexPatternSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRegexPatternSetResponse
mkGetRegexPatternSetResponse responseStatus
  = GetRegexPatternSetResponse'{regexPatternSet = Core.Nothing,
                                responseStatus}

-- | Information about the 'RegexPatternSet' that you specified in the @GetRegexPatternSet@ request, including the identifier of the pattern set and the regular expression patterns you want AWS WAF to search for. 
--
-- /Note:/ Consider using 'regexPatternSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpsrrsRegexPatternSet :: Lens.Lens' GetRegexPatternSetResponse (Core.Maybe Types.RegexPatternSet)
grpsrrsRegexPatternSet = Lens.field @"regexPatternSet"
{-# INLINEABLE grpsrrsRegexPatternSet #-}
{-# DEPRECATED regexPatternSet "Use generic-lens or generic-optics with 'regexPatternSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpsrrsResponseStatus :: Lens.Lens' GetRegexPatternSetResponse Core.Int
grpsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grpsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
