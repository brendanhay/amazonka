{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.DeleteByteMatchSet
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
module Network.AWS.WAF.DeleteByteMatchSet
  ( -- * Creating a request
    DeleteByteMatchSet (..),
    mkDeleteByteMatchSet,

    -- ** Request lenses
    dbmsByteMatchSetId,
    dbmsChangeToken,

    -- * Destructuring the response
    DeleteByteMatchSetResponse (..),
    mkDeleteByteMatchSetResponse,

    -- ** Response lenses
    dbmsrrsChangeToken,
    dbmsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkDeleteByteMatchSet' smart constructor.
data DeleteByteMatchSet = DeleteByteMatchSet'
  { -- | The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to delete. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
    byteMatchSetId :: Types.ResourceId,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteByteMatchSet' value with any optional fields omitted.
mkDeleteByteMatchSet ::
  -- | 'byteMatchSetId'
  Types.ResourceId ->
  -- | 'changeToken'
  Types.ChangeToken ->
  DeleteByteMatchSet
mkDeleteByteMatchSet byteMatchSetId changeToken =
  DeleteByteMatchSet' {byteMatchSetId, changeToken}

-- | The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to delete. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- /Note:/ Consider using 'byteMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmsByteMatchSetId :: Lens.Lens' DeleteByteMatchSet Types.ResourceId
dbmsByteMatchSetId = Lens.field @"byteMatchSetId"
{-# DEPRECATED dbmsByteMatchSetId "Use generic-lens or generic-optics with 'byteMatchSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmsChangeToken :: Lens.Lens' DeleteByteMatchSet Types.ChangeToken
dbmsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED dbmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Core.FromJSON DeleteByteMatchSet where
  toJSON DeleteByteMatchSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ByteMatchSetId" Core..= byteMatchSetId),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.AWSRequest DeleteByteMatchSet where
  type Rs DeleteByteMatchSet = DeleteByteMatchSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_20150824.DeleteByteMatchSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteByteMatchSetResponse'
            Core.<$> (x Core..:? "ChangeToken") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteByteMatchSetResponse' smart constructor.
data DeleteByteMatchSetResponse = DeleteByteMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteByteMatchSetResponse' value with any optional fields omitted.
mkDeleteByteMatchSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteByteMatchSetResponse
mkDeleteByteMatchSetResponse responseStatus =
  DeleteByteMatchSetResponse'
    { changeToken = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @DeleteByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmsrrsChangeToken :: Lens.Lens' DeleteByteMatchSetResponse (Core.Maybe Types.ChangeToken)
dbmsrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED dbmsrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmsrrsResponseStatus :: Lens.Lens' DeleteByteMatchSetResponse Core.Int
dbmsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbmsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
