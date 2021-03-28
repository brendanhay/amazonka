{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.DeleteRegexPatternSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'RegexPatternSet' . You can't delete a @RegexPatternSet@ if it's still used in any @RegexMatchSet@ or if the @RegexPatternSet@ is not empty. 
module Network.AWS.WAFRegional.DeleteRegexPatternSet
    (
    -- * Creating a request
      DeleteRegexPatternSet (..)
    , mkDeleteRegexPatternSet
    -- ** Request lenses
    , drpsRegexPatternSetId
    , drpsChangeToken

    -- * Destructuring the response
    , DeleteRegexPatternSetResponse (..)
    , mkDeleteRegexPatternSetResponse
    -- ** Response lenses
    , drpsrrsChangeToken
    , drpsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkDeleteRegexPatternSet' smart constructor.
data DeleteRegexPatternSet = DeleteRegexPatternSet'
  { regexPatternSetId :: Types.ResourceId
    -- ^ The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to delete. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRegexPatternSet' value with any optional fields omitted.
mkDeleteRegexPatternSet
    :: Types.ResourceId -- ^ 'regexPatternSetId'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> DeleteRegexPatternSet
mkDeleteRegexPatternSet regexPatternSetId changeToken
  = DeleteRegexPatternSet'{regexPatternSetId, changeToken}

-- | The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to delete. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- /Note:/ Consider using 'regexPatternSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpsRegexPatternSetId :: Lens.Lens' DeleteRegexPatternSet Types.ResourceId
drpsRegexPatternSetId = Lens.field @"regexPatternSetId"
{-# INLINEABLE drpsRegexPatternSetId #-}
{-# DEPRECATED regexPatternSetId "Use generic-lens or generic-optics with 'regexPatternSetId' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpsChangeToken :: Lens.Lens' DeleteRegexPatternSet Types.ChangeToken
drpsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE drpsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery DeleteRegexPatternSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteRegexPatternSet where
        toHeaders DeleteRegexPatternSet{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.DeleteRegexPatternSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteRegexPatternSet where
        toJSON DeleteRegexPatternSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RegexPatternSetId" Core..= regexPatternSetId),
                  Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest DeleteRegexPatternSet where
        type Rs DeleteRegexPatternSet = DeleteRegexPatternSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteRegexPatternSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRegexPatternSetResponse' smart constructor.
data DeleteRegexPatternSetResponse = DeleteRegexPatternSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @DeleteRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRegexPatternSetResponse' value with any optional fields omitted.
mkDeleteRegexPatternSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteRegexPatternSetResponse
mkDeleteRegexPatternSetResponse responseStatus
  = DeleteRegexPatternSetResponse'{changeToken = Core.Nothing,
                                   responseStatus}

-- | The @ChangeToken@ that you used to submit the @DeleteRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpsrrsChangeToken :: Lens.Lens' DeleteRegexPatternSetResponse (Core.Maybe Types.ChangeToken)
drpsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE drpsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpsrrsResponseStatus :: Lens.Lens' DeleteRegexPatternSetResponse Core.Int
drpsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drpsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
