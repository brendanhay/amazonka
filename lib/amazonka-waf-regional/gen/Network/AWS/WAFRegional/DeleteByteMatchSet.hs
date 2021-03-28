{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.DeleteByteMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'ByteMatchSet' . You can't delete a @ByteMatchSet@ if it's still used in any @Rules@ or if it still includes any 'ByteMatchTuple' objects (any filters).
--
-- If you just want to remove a @ByteMatchSet@ from a @Rule@ , use 'UpdateRule' .
-- To permanently delete a @ByteMatchSet@ , perform the following steps:
--
--     * Update the @ByteMatchSet@ to remove filters, if any. For more information, see 'UpdateByteMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteByteMatchSet@ request.
--
--
--     * Submit a @DeleteByteMatchSet@ request.
--
--
module Network.AWS.WAFRegional.DeleteByteMatchSet
    (
    -- * Creating a request
      DeleteByteMatchSet (..)
    , mkDeleteByteMatchSet
    -- ** Request lenses
    , dbmsByteMatchSetId
    , dbmsChangeToken

    -- * Destructuring the response
    , DeleteByteMatchSetResponse (..)
    , mkDeleteByteMatchSetResponse
    -- ** Response lenses
    , dbmsrrsChangeToken
    , dbmsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkDeleteByteMatchSet' smart constructor.
data DeleteByteMatchSet = DeleteByteMatchSet'
  { byteMatchSetId :: Types.ResourceId
    -- ^ The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to delete. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteByteMatchSet' value with any optional fields omitted.
mkDeleteByteMatchSet
    :: Types.ResourceId -- ^ 'byteMatchSetId'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> DeleteByteMatchSet
mkDeleteByteMatchSet byteMatchSetId changeToken
  = DeleteByteMatchSet'{byteMatchSetId, changeToken}

-- | The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to delete. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- /Note:/ Consider using 'byteMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmsByteMatchSetId :: Lens.Lens' DeleteByteMatchSet Types.ResourceId
dbmsByteMatchSetId = Lens.field @"byteMatchSetId"
{-# INLINEABLE dbmsByteMatchSetId #-}
{-# DEPRECATED byteMatchSetId "Use generic-lens or generic-optics with 'byteMatchSetId' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmsChangeToken :: Lens.Lens' DeleteByteMatchSet Types.ChangeToken
dbmsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE dbmsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery DeleteByteMatchSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteByteMatchSet where
        toHeaders DeleteByteMatchSet{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.DeleteByteMatchSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteByteMatchSet where
        toJSON DeleteByteMatchSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ByteMatchSetId" Core..= byteMatchSetId),
                  Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest DeleteByteMatchSet where
        type Rs DeleteByteMatchSet = DeleteByteMatchSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteByteMatchSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteByteMatchSetResponse' smart constructor.
data DeleteByteMatchSetResponse = DeleteByteMatchSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @DeleteByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteByteMatchSetResponse' value with any optional fields omitted.
mkDeleteByteMatchSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteByteMatchSetResponse
mkDeleteByteMatchSetResponse responseStatus
  = DeleteByteMatchSetResponse'{changeToken = Core.Nothing,
                                responseStatus}

-- | The @ChangeToken@ that you used to submit the @DeleteByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmsrrsChangeToken :: Lens.Lens' DeleteByteMatchSetResponse (Core.Maybe Types.ChangeToken)
dbmsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE dbmsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmsrrsResponseStatus :: Lens.Lens' DeleteByteMatchSetResponse Core.Int
dbmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
