{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateSizeConstraintSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @SizeConstraintSet@ . You then use 'UpdateSizeConstraintSet' to identify the part of a web request that you want AWS WAF to check for length, such as the length of the @User-Agent@ header or the length of the query string. For example, you can create a @SizeConstraintSet@ that matches any requests that have a query string that is longer than 100 bytes. You can then configure AWS WAF to reject those requests.
--
-- To create and configure a @SizeConstraintSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateSizeConstraintSet@ request.
--
--
--     * Submit a @CreateSizeConstraintSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateSizeConstraintSet@ request.
--
--
--     * Submit an 'UpdateSizeConstraintSet' request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateSizeConstraintSet
    (
    -- * Creating a request
      CreateSizeConstraintSet (..)
    , mkCreateSizeConstraintSet
    -- ** Request lenses
    , cscsName
    , cscsChangeToken

    -- * Destructuring the response
    , CreateSizeConstraintSetResponse (..)
    , mkCreateSizeConstraintSetResponse
    -- ** Response lenses
    , cscsrrsChangeToken
    , cscsrrsSizeConstraintSet
    , cscsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkCreateSizeConstraintSet' smart constructor.
data CreateSizeConstraintSet = CreateSizeConstraintSet'
  { name :: Types.ResourceName
    -- ^ A friendly name or description of the 'SizeConstraintSet' . You can't change @Name@ after you create a @SizeConstraintSet@ .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSizeConstraintSet' value with any optional fields omitted.
mkCreateSizeConstraintSet
    :: Types.ResourceName -- ^ 'name'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> CreateSizeConstraintSet
mkCreateSizeConstraintSet name changeToken
  = CreateSizeConstraintSet'{name, changeToken}

-- | A friendly name or description of the 'SizeConstraintSet' . You can't change @Name@ after you create a @SizeConstraintSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsName :: Lens.Lens' CreateSizeConstraintSet Types.ResourceName
cscsName = Lens.field @"name"
{-# INLINEABLE cscsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsChangeToken :: Lens.Lens' CreateSizeConstraintSet Types.ChangeToken
cscsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE cscsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery CreateSizeConstraintSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateSizeConstraintSet where
        toHeaders CreateSizeConstraintSet{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_20150824.CreateSizeConstraintSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateSizeConstraintSet where
        toJSON CreateSizeConstraintSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest CreateSizeConstraintSet where
        type Rs CreateSizeConstraintSet = CreateSizeConstraintSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateSizeConstraintSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> x Core..:? "SizeConstraintSet"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateSizeConstraintSetResponse' smart constructor.
data CreateSizeConstraintSetResponse = CreateSizeConstraintSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @CreateSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , sizeConstraintSet :: Core.Maybe Types.SizeConstraintSet
    -- ^ A 'SizeConstraintSet' that contains no @SizeConstraint@ objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSizeConstraintSetResponse' value with any optional fields omitted.
mkCreateSizeConstraintSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSizeConstraintSetResponse
mkCreateSizeConstraintSetResponse responseStatus
  = CreateSizeConstraintSetResponse'{changeToken = Core.Nothing,
                                     sizeConstraintSet = Core.Nothing, responseStatus}

-- | The @ChangeToken@ that you used to submit the @CreateSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsrrsChangeToken :: Lens.Lens' CreateSizeConstraintSetResponse (Core.Maybe Types.ChangeToken)
cscsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE cscsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | A 'SizeConstraintSet' that contains no @SizeConstraint@ objects.
--
-- /Note:/ Consider using 'sizeConstraintSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsrrsSizeConstraintSet :: Lens.Lens' CreateSizeConstraintSetResponse (Core.Maybe Types.SizeConstraintSet)
cscsrrsSizeConstraintSet = Lens.field @"sizeConstraintSet"
{-# INLINEABLE cscsrrsSizeConstraintSet #-}
{-# DEPRECATED sizeConstraintSet "Use generic-lens or generic-optics with 'sizeConstraintSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsrrsResponseStatus :: Lens.Lens' CreateSizeConstraintSetResponse Core.Int
cscsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cscsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
