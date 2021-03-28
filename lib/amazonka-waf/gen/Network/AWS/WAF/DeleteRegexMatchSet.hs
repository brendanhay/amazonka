{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.DeleteRegexMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'RegexMatchSet' . You can't delete a @RegexMatchSet@ if it's still used in any @Rules@ or if it still includes any @RegexMatchTuples@ objects (any filters).
--
-- If you just want to remove a @RegexMatchSet@ from a @Rule@ , use 'UpdateRule' .
-- To permanently delete a @RegexMatchSet@ , perform the following steps:
--
--     * Update the @RegexMatchSet@ to remove filters, if any. For more information, see 'UpdateRegexMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteRegexMatchSet@ request.
--
--
--     * Submit a @DeleteRegexMatchSet@ request.
--
--
module Network.AWS.WAF.DeleteRegexMatchSet
    (
    -- * Creating a request
      DeleteRegexMatchSet (..)
    , mkDeleteRegexMatchSet
    -- ** Request lenses
    , drmsRegexMatchSetId
    , drmsChangeToken

    -- * Destructuring the response
    , DeleteRegexMatchSetResponse (..)
    , mkDeleteRegexMatchSetResponse
    -- ** Response lenses
    , drmsrrsChangeToken
    , drmsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkDeleteRegexMatchSet' smart constructor.
data DeleteRegexMatchSet = DeleteRegexMatchSet'
  { regexMatchSetId :: Types.RegexMatchSetId
    -- ^ The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to delete. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRegexMatchSet' value with any optional fields omitted.
mkDeleteRegexMatchSet
    :: Types.RegexMatchSetId -- ^ 'regexMatchSetId'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> DeleteRegexMatchSet
mkDeleteRegexMatchSet regexMatchSetId changeToken
  = DeleteRegexMatchSet'{regexMatchSetId, changeToken}

-- | The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to delete. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
--
-- /Note:/ Consider using 'regexMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drmsRegexMatchSetId :: Lens.Lens' DeleteRegexMatchSet Types.RegexMatchSetId
drmsRegexMatchSetId = Lens.field @"regexMatchSetId"
{-# INLINEABLE drmsRegexMatchSetId #-}
{-# DEPRECATED regexMatchSetId "Use generic-lens or generic-optics with 'regexMatchSetId' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drmsChangeToken :: Lens.Lens' DeleteRegexMatchSet Types.ChangeToken
drmsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE drmsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery DeleteRegexMatchSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteRegexMatchSet where
        toHeaders DeleteRegexMatchSet{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.DeleteRegexMatchSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteRegexMatchSet where
        toJSON DeleteRegexMatchSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RegexMatchSetId" Core..= regexMatchSetId),
                  Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest DeleteRegexMatchSet where
        type Rs DeleteRegexMatchSet = DeleteRegexMatchSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteRegexMatchSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRegexMatchSetResponse' smart constructor.
data DeleteRegexMatchSetResponse = DeleteRegexMatchSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @DeleteRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRegexMatchSetResponse' value with any optional fields omitted.
mkDeleteRegexMatchSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteRegexMatchSetResponse
mkDeleteRegexMatchSetResponse responseStatus
  = DeleteRegexMatchSetResponse'{changeToken = Core.Nothing,
                                 responseStatus}

-- | The @ChangeToken@ that you used to submit the @DeleteRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drmsrrsChangeToken :: Lens.Lens' DeleteRegexMatchSetResponse (Core.Maybe Types.ChangeToken)
drmsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE drmsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drmsrrsResponseStatus :: Lens.Lens' DeleteRegexMatchSetResponse Core.Int
drmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
