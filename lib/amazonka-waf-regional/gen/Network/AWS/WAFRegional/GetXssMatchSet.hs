{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetXssMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'XssMatchSet' that is specified by @XssMatchSetId@ .
module Network.AWS.WAFRegional.GetXssMatchSet
    (
    -- * Creating a request
      GetXssMatchSet (..)
    , mkGetXssMatchSet
    -- ** Request lenses
    , gxmsXssMatchSetId

    -- * Destructuring the response
    , GetXssMatchSetResponse (..)
    , mkGetXssMatchSetResponse
    -- ** Response lenses
    , gxmsrrsXssMatchSet
    , gxmsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | A request to get an 'XssMatchSet' .
--
-- /See:/ 'mkGetXssMatchSet' smart constructor.
newtype GetXssMatchSet = GetXssMatchSet'
  { xssMatchSetId :: Types.ResourceId
    -- ^ The @XssMatchSetId@ of the 'XssMatchSet' that you want to get. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetXssMatchSet' value with any optional fields omitted.
mkGetXssMatchSet
    :: Types.ResourceId -- ^ 'xssMatchSetId'
    -> GetXssMatchSet
mkGetXssMatchSet xssMatchSetId = GetXssMatchSet'{xssMatchSetId}

-- | The @XssMatchSetId@ of the 'XssMatchSet' that you want to get. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
--
-- /Note:/ Consider using 'xssMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gxmsXssMatchSetId :: Lens.Lens' GetXssMatchSet Types.ResourceId
gxmsXssMatchSetId = Lens.field @"xssMatchSetId"
{-# INLINEABLE gxmsXssMatchSetId #-}
{-# DEPRECATED xssMatchSetId "Use generic-lens or generic-optics with 'xssMatchSetId' instead"  #-}

instance Core.ToQuery GetXssMatchSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetXssMatchSet where
        toHeaders GetXssMatchSet{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.GetXssMatchSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetXssMatchSet where
        toJSON GetXssMatchSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("XssMatchSetId" Core..= xssMatchSetId)])

instance Core.AWSRequest GetXssMatchSet where
        type Rs GetXssMatchSet = GetXssMatchSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetXssMatchSetResponse' Core.<$>
                   (x Core..:? "XssMatchSet") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The response to a 'GetXssMatchSet' request.
--
-- /See:/ 'mkGetXssMatchSetResponse' smart constructor.
data GetXssMatchSetResponse = GetXssMatchSetResponse'
  { xssMatchSet :: Core.Maybe Types.XssMatchSet
    -- ^ Information about the 'XssMatchSet' that you specified in the @GetXssMatchSet@ request. For more information, see the following topics:
--
--
--     * 'XssMatchSet' : Contains @Name@ , @XssMatchSetId@ , and an array of @XssMatchTuple@ objects
--
--
--     * 'XssMatchTuple' : Each @XssMatchTuple@ object contains @FieldToMatch@ and @TextTransformation@ 
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@ 
--
--
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetXssMatchSetResponse' value with any optional fields omitted.
mkGetXssMatchSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetXssMatchSetResponse
mkGetXssMatchSetResponse responseStatus
  = GetXssMatchSetResponse'{xssMatchSet = Core.Nothing,
                            responseStatus}

-- | Information about the 'XssMatchSet' that you specified in the @GetXssMatchSet@ request. For more information, see the following topics:
--
--
--     * 'XssMatchSet' : Contains @Name@ , @XssMatchSetId@ , and an array of @XssMatchTuple@ objects
--
--
--     * 'XssMatchTuple' : Each @XssMatchTuple@ object contains @FieldToMatch@ and @TextTransformation@ 
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@ 
--
--
--
-- /Note:/ Consider using 'xssMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gxmsrrsXssMatchSet :: Lens.Lens' GetXssMatchSetResponse (Core.Maybe Types.XssMatchSet)
gxmsrrsXssMatchSet = Lens.field @"xssMatchSet"
{-# INLINEABLE gxmsrrsXssMatchSet #-}
{-# DEPRECATED xssMatchSet "Use generic-lens or generic-optics with 'xssMatchSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gxmsrrsResponseStatus :: Lens.Lens' GetXssMatchSetResponse Core.Int
gxmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gxmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
