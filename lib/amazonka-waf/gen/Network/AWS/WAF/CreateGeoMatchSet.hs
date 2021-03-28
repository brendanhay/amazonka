{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateGeoMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an 'GeoMatchSet' , which you use to specify which web requests you want to allow or block based on the country that the requests originate from. For example, if you're receiving a lot of requests from one or more countries and you want to block the requests, you can create an @GeoMatchSet@ that contains those countries and then configure AWS WAF to block the requests. 
--
-- To create and configure a @GeoMatchSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateGeoMatchSet@ request.
--
--
--     * Submit a @CreateGeoMatchSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateGeoMatchSet' request.
--
--
--     * Submit an @UpdateGeoMatchSetSet@ request to specify the countries that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateGeoMatchSet
    (
    -- * Creating a request
      CreateGeoMatchSet (..)
    , mkCreateGeoMatchSet
    -- ** Request lenses
    , cgmsName
    , cgmsChangeToken

    -- * Destructuring the response
    , CreateGeoMatchSetResponse (..)
    , mkCreateGeoMatchSetResponse
    -- ** Response lenses
    , cgmsrrsChangeToken
    , cgmsrrsGeoMatchSet
    , cgmsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkCreateGeoMatchSet' smart constructor.
data CreateGeoMatchSet = CreateGeoMatchSet'
  { name :: Types.ResourceName
    -- ^ A friendly name or description of the 'GeoMatchSet' . You can't change @Name@ after you create the @GeoMatchSet@ .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGeoMatchSet' value with any optional fields omitted.
mkCreateGeoMatchSet
    :: Types.ResourceName -- ^ 'name'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> CreateGeoMatchSet
mkCreateGeoMatchSet name changeToken
  = CreateGeoMatchSet'{name, changeToken}

-- | A friendly name or description of the 'GeoMatchSet' . You can't change @Name@ after you create the @GeoMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgmsName :: Lens.Lens' CreateGeoMatchSet Types.ResourceName
cgmsName = Lens.field @"name"
{-# INLINEABLE cgmsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgmsChangeToken :: Lens.Lens' CreateGeoMatchSet Types.ChangeToken
cgmsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE cgmsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery CreateGeoMatchSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateGeoMatchSet where
        toHeaders CreateGeoMatchSet{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.CreateGeoMatchSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateGeoMatchSet where
        toJSON CreateGeoMatchSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest CreateGeoMatchSet where
        type Rs CreateGeoMatchSet = CreateGeoMatchSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateGeoMatchSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> x Core..:? "GeoMatchSet"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateGeoMatchSetResponse' smart constructor.
data CreateGeoMatchSetResponse = CreateGeoMatchSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @CreateGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , geoMatchSet :: Core.Maybe Types.GeoMatchSet
    -- ^ The 'GeoMatchSet' returned in the @CreateGeoMatchSet@ response. The @GeoMatchSet@ contains no @GeoMatchConstraints@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGeoMatchSetResponse' value with any optional fields omitted.
mkCreateGeoMatchSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateGeoMatchSetResponse
mkCreateGeoMatchSetResponse responseStatus
  = CreateGeoMatchSetResponse'{changeToken = Core.Nothing,
                               geoMatchSet = Core.Nothing, responseStatus}

-- | The @ChangeToken@ that you used to submit the @CreateGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgmsrrsChangeToken :: Lens.Lens' CreateGeoMatchSetResponse (Core.Maybe Types.ChangeToken)
cgmsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE cgmsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The 'GeoMatchSet' returned in the @CreateGeoMatchSet@ response. The @GeoMatchSet@ contains no @GeoMatchConstraints@ .
--
-- /Note:/ Consider using 'geoMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgmsrrsGeoMatchSet :: Lens.Lens' CreateGeoMatchSetResponse (Core.Maybe Types.GeoMatchSet)
cgmsrrsGeoMatchSet = Lens.field @"geoMatchSet"
{-# INLINEABLE cgmsrrsGeoMatchSet #-}
{-# DEPRECATED geoMatchSet "Use generic-lens or generic-optics with 'geoMatchSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgmsrrsResponseStatus :: Lens.Lens' CreateGeoMatchSetResponse Core.Int
cgmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cgmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
