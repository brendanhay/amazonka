{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.CreateRegexPatternSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @RegexPatternSet@ . You then use 'UpdateRegexPatternSet' to specify the regular expression (regex) pattern that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ . You can then configure AWS WAF to reject those requests.
--
-- To create and configure a @RegexPatternSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateRegexPatternSet@ request.
--
--
--     * Submit a @CreateRegexPatternSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateRegexPatternSet@ request.
--
--
--     * Submit an 'UpdateRegexPatternSet' request to specify the string that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.CreateRegexPatternSet
    (
    -- * Creating a request
      CreateRegexPatternSet (..)
    , mkCreateRegexPatternSet
    -- ** Request lenses
    , crpsName
    , crpsChangeToken

    -- * Destructuring the response
    , CreateRegexPatternSetResponse (..)
    , mkCreateRegexPatternSetResponse
    -- ** Response lenses
    , crpsrrsChangeToken
    , crpsrrsRegexPatternSet
    , crpsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkCreateRegexPatternSet' smart constructor.
data CreateRegexPatternSet = CreateRegexPatternSet'
  { name :: Types.ResourceName
    -- ^ A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRegexPatternSet' value with any optional fields omitted.
mkCreateRegexPatternSet
    :: Types.ResourceName -- ^ 'name'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> CreateRegexPatternSet
mkCreateRegexPatternSet name changeToken
  = CreateRegexPatternSet'{name, changeToken}

-- | A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpsName :: Lens.Lens' CreateRegexPatternSet Types.ResourceName
crpsName = Lens.field @"name"
{-# INLINEABLE crpsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpsChangeToken :: Lens.Lens' CreateRegexPatternSet Types.ChangeToken
crpsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE crpsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery CreateRegexPatternSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateRegexPatternSet where
        toHeaders CreateRegexPatternSet{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.CreateRegexPatternSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateRegexPatternSet where
        toJSON CreateRegexPatternSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest CreateRegexPatternSet where
        type Rs CreateRegexPatternSet = CreateRegexPatternSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateRegexPatternSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> x Core..:? "RegexPatternSet"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateRegexPatternSetResponse' smart constructor.
data CreateRegexPatternSetResponse = CreateRegexPatternSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @CreateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , regexPatternSet :: Core.Maybe Types.RegexPatternSet
    -- ^ A 'RegexPatternSet' that contains no objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRegexPatternSetResponse' value with any optional fields omitted.
mkCreateRegexPatternSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateRegexPatternSetResponse
mkCreateRegexPatternSetResponse responseStatus
  = CreateRegexPatternSetResponse'{changeToken = Core.Nothing,
                                   regexPatternSet = Core.Nothing, responseStatus}

-- | The @ChangeToken@ that you used to submit the @CreateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpsrrsChangeToken :: Lens.Lens' CreateRegexPatternSetResponse (Core.Maybe Types.ChangeToken)
crpsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE crpsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | A 'RegexPatternSet' that contains no objects.
--
-- /Note:/ Consider using 'regexPatternSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpsrrsRegexPatternSet :: Lens.Lens' CreateRegexPatternSetResponse (Core.Maybe Types.RegexPatternSet)
crpsrrsRegexPatternSet = Lens.field @"regexPatternSet"
{-# INLINEABLE crpsrrsRegexPatternSet #-}
{-# DEPRECATED regexPatternSet "Use generic-lens or generic-optics with 'regexPatternSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpsrrsResponseStatus :: Lens.Lens' CreateRegexPatternSetResponse Core.Int
crpsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crpsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
