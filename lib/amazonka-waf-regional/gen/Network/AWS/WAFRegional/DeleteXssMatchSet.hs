{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.DeleteXssMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an 'XssMatchSet' . You can't delete an @XssMatchSet@ if it's still used in any @Rules@ or if it still contains any 'XssMatchTuple' objects.
--
-- If you just want to remove an @XssMatchSet@ from a @Rule@ , use 'UpdateRule' .
-- To permanently delete an @XssMatchSet@ from AWS WAF, perform the following steps:
--
--     * Update the @XssMatchSet@ to remove filters, if any. For more information, see 'UpdateXssMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteXssMatchSet@ request.
--
--
--     * Submit a @DeleteXssMatchSet@ request.
--
--
module Network.AWS.WAFRegional.DeleteXssMatchSet
    (
    -- * Creating a request
      DeleteXssMatchSet (..)
    , mkDeleteXssMatchSet
    -- ** Request lenses
    , dxmsXssMatchSetId
    , dxmsChangeToken

    -- * Destructuring the response
    , DeleteXssMatchSetResponse (..)
    , mkDeleteXssMatchSetResponse
    -- ** Response lenses
    , dxmsrrsChangeToken
    , dxmsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | A request to delete an 'XssMatchSet' from AWS WAF.
--
-- /See:/ 'mkDeleteXssMatchSet' smart constructor.
data DeleteXssMatchSet = DeleteXssMatchSet'
  { xssMatchSetId :: Types.ResourceId
    -- ^ The @XssMatchSetId@ of the 'XssMatchSet' that you want to delete. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteXssMatchSet' value with any optional fields omitted.
mkDeleteXssMatchSet
    :: Types.ResourceId -- ^ 'xssMatchSetId'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> DeleteXssMatchSet
mkDeleteXssMatchSet xssMatchSetId changeToken
  = DeleteXssMatchSet'{xssMatchSetId, changeToken}

-- | The @XssMatchSetId@ of the 'XssMatchSet' that you want to delete. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
--
-- /Note:/ Consider using 'xssMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dxmsXssMatchSetId :: Lens.Lens' DeleteXssMatchSet Types.ResourceId
dxmsXssMatchSetId = Lens.field @"xssMatchSetId"
{-# INLINEABLE dxmsXssMatchSetId #-}
{-# DEPRECATED xssMatchSetId "Use generic-lens or generic-optics with 'xssMatchSetId' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dxmsChangeToken :: Lens.Lens' DeleteXssMatchSet Types.ChangeToken
dxmsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE dxmsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery DeleteXssMatchSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteXssMatchSet where
        toHeaders DeleteXssMatchSet{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.DeleteXssMatchSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteXssMatchSet where
        toJSON DeleteXssMatchSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("XssMatchSetId" Core..= xssMatchSetId),
                  Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest DeleteXssMatchSet where
        type Rs DeleteXssMatchSet = DeleteXssMatchSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteXssMatchSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The response to a request to delete an 'XssMatchSet' from AWS WAF.
--
-- /See:/ 'mkDeleteXssMatchSetResponse' smart constructor.
data DeleteXssMatchSetResponse = DeleteXssMatchSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @DeleteXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteXssMatchSetResponse' value with any optional fields omitted.
mkDeleteXssMatchSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteXssMatchSetResponse
mkDeleteXssMatchSetResponse responseStatus
  = DeleteXssMatchSetResponse'{changeToken = Core.Nothing,
                               responseStatus}

-- | The @ChangeToken@ that you used to submit the @DeleteXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dxmsrrsChangeToken :: Lens.Lens' DeleteXssMatchSetResponse (Core.Maybe Types.ChangeToken)
dxmsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE dxmsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dxmsrrsResponseStatus :: Lens.Lens' DeleteXssMatchSetResponse Core.Int
dxmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dxmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
