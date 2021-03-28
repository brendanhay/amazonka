{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateXssMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an 'XssMatchSet' , which you use to allow, block, or count requests that contain cross-site scripting attacks in the specified part of web requests. AWS WAF searches for character sequences that are likely to be malicious strings.
--
-- To create and configure an @XssMatchSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateXssMatchSet@ request.
--
--
--     * Submit a @CreateXssMatchSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateXssMatchSet' request.
--
--
--     * Submit an 'UpdateXssMatchSet' request to specify the parts of web requests in which you want to allow, block, or count cross-site scripting attacks.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateXssMatchSet
    (
    -- * Creating a request
      CreateXssMatchSet (..)
    , mkCreateXssMatchSet
    -- ** Request lenses
    , cxmsName
    , cxmsChangeToken

    -- * Destructuring the response
    , CreateXssMatchSetResponse (..)
    , mkCreateXssMatchSetResponse
    -- ** Response lenses
    , cxmsrrsChangeToken
    , cxmsrrsXssMatchSet
    , cxmsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | A request to create an 'XssMatchSet' .
--
-- /See:/ 'mkCreateXssMatchSet' smart constructor.
data CreateXssMatchSet = CreateXssMatchSet'
  { name :: Types.ResourceName
    -- ^ A friendly name or description for the 'XssMatchSet' that you're creating. You can't change @Name@ after you create the @XssMatchSet@ .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateXssMatchSet' value with any optional fields omitted.
mkCreateXssMatchSet
    :: Types.ResourceName -- ^ 'name'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> CreateXssMatchSet
mkCreateXssMatchSet name changeToken
  = CreateXssMatchSet'{name, changeToken}

-- | A friendly name or description for the 'XssMatchSet' that you're creating. You can't change @Name@ after you create the @XssMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxmsName :: Lens.Lens' CreateXssMatchSet Types.ResourceName
cxmsName = Lens.field @"name"
{-# INLINEABLE cxmsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxmsChangeToken :: Lens.Lens' CreateXssMatchSet Types.ChangeToken
cxmsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE cxmsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery CreateXssMatchSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateXssMatchSet where
        toHeaders CreateXssMatchSet{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.CreateXssMatchSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateXssMatchSet where
        toJSON CreateXssMatchSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest CreateXssMatchSet where
        type Rs CreateXssMatchSet = CreateXssMatchSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateXssMatchSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> x Core..:? "XssMatchSet"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The response to a @CreateXssMatchSet@ request.
--
-- /See:/ 'mkCreateXssMatchSetResponse' smart constructor.
data CreateXssMatchSetResponse = CreateXssMatchSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @CreateXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , xssMatchSet :: Core.Maybe Types.XssMatchSet
    -- ^ An 'XssMatchSet' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateXssMatchSetResponse' value with any optional fields omitted.
mkCreateXssMatchSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateXssMatchSetResponse
mkCreateXssMatchSetResponse responseStatus
  = CreateXssMatchSetResponse'{changeToken = Core.Nothing,
                               xssMatchSet = Core.Nothing, responseStatus}

-- | The @ChangeToken@ that you used to submit the @CreateXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxmsrrsChangeToken :: Lens.Lens' CreateXssMatchSetResponse (Core.Maybe Types.ChangeToken)
cxmsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE cxmsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | An 'XssMatchSet' .
--
-- /Note:/ Consider using 'xssMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxmsrrsXssMatchSet :: Lens.Lens' CreateXssMatchSetResponse (Core.Maybe Types.XssMatchSet)
cxmsrrsXssMatchSet = Lens.field @"xssMatchSet"
{-# INLINEABLE cxmsrrsXssMatchSet #-}
{-# DEPRECATED xssMatchSet "Use generic-lens or generic-optics with 'xssMatchSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxmsrrsResponseStatus :: Lens.Lens' CreateXssMatchSetResponse Core.Int
cxmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cxmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
