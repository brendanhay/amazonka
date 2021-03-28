{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetWebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'WebACL' that is specified by @WebACLId@ .
module Network.AWS.WAF.GetWebACL
    (
    -- * Creating a request
      GetWebACL (..)
    , mkGetWebACL
    -- ** Request lenses
    , gwaclWebACLId

    -- * Destructuring the response
    , GetWebACLResponse (..)
    , mkGetWebACLResponse
    -- ** Response lenses
    , gwaclrrsWebACL
    , gwaclrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkGetWebACL' smart constructor.
newtype GetWebACL = GetWebACL'
  { webACLId :: Types.ResourceId
    -- ^ The @WebACLId@ of the 'WebACL' that you want to get. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetWebACL' value with any optional fields omitted.
mkGetWebACL
    :: Types.ResourceId -- ^ 'webACLId'
    -> GetWebACL
mkGetWebACL webACLId = GetWebACL'{webACLId}

-- | The @WebACLId@ of the 'WebACL' that you want to get. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwaclWebACLId :: Lens.Lens' GetWebACL Types.ResourceId
gwaclWebACLId = Lens.field @"webACLId"
{-# INLINEABLE gwaclWebACLId #-}
{-# DEPRECATED webACLId "Use generic-lens or generic-optics with 'webACLId' instead"  #-}

instance Core.ToQuery GetWebACL where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetWebACL where
        toHeaders GetWebACL{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.GetWebACL") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetWebACL where
        toJSON GetWebACL{..}
          = Core.object
              (Core.catMaybes [Core.Just ("WebACLId" Core..= webACLId)])

instance Core.AWSRequest GetWebACL where
        type Rs GetWebACL = GetWebACLResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetWebACLResponse' Core.<$>
                   (x Core..:? "WebACL") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetWebACLResponse' smart constructor.
data GetWebACLResponse = GetWebACLResponse'
  { webACL :: Core.Maybe Types.WebACL
    -- ^ Information about the 'WebACL' that you specified in the @GetWebACL@ request. For more information, see the following topics:
--
--
--     * 'WebACL' : Contains @DefaultAction@ , @MetricName@ , @Name@ , an array of @Rule@ objects, and @WebACLId@ 
--
--
--     * @DefaultAction@ (Data type is 'WafAction' ): Contains @Type@ 
--
--
--     * @Rules@ : Contains an array of @ActivatedRule@ objects, which contain @Action@ , @Priority@ , and @RuleId@ 
--
--
--     * @Action@ : Contains @Type@ 
--
--
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetWebACLResponse' value with any optional fields omitted.
mkGetWebACLResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetWebACLResponse
mkGetWebACLResponse responseStatus
  = GetWebACLResponse'{webACL = Core.Nothing, responseStatus}

-- | Information about the 'WebACL' that you specified in the @GetWebACL@ request. For more information, see the following topics:
--
--
--     * 'WebACL' : Contains @DefaultAction@ , @MetricName@ , @Name@ , an array of @Rule@ objects, and @WebACLId@ 
--
--
--     * @DefaultAction@ (Data type is 'WafAction' ): Contains @Type@ 
--
--
--     * @Rules@ : Contains an array of @ActivatedRule@ objects, which contain @Action@ , @Priority@ , and @RuleId@ 
--
--
--     * @Action@ : Contains @Type@ 
--
--
--
-- /Note:/ Consider using 'webACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwaclrrsWebACL :: Lens.Lens' GetWebACLResponse (Core.Maybe Types.WebACL)
gwaclrrsWebACL = Lens.field @"webACL"
{-# INLINEABLE gwaclrrsWebACL #-}
{-# DEPRECATED webACL "Use generic-lens or generic-optics with 'webACL' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwaclrrsResponseStatus :: Lens.Lens' GetWebACLResponse Core.Int
gwaclrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gwaclrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
