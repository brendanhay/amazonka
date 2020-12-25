{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.DeleteRegexMatchSet
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
module Network.AWS.WAFRegional.DeleteRegexMatchSet
  ( -- * Creating a request
    DeleteRegexMatchSet (..),
    mkDeleteRegexMatchSet,

    -- ** Request lenses
    drmsRegexMatchSetId,
    drmsChangeToken,

    -- * Destructuring the response
    DeleteRegexMatchSetResponse (..),
    mkDeleteRegexMatchSetResponse,

    -- ** Response lenses
    drmsrrsChangeToken,
    drmsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkDeleteRegexMatchSet' smart constructor.
data DeleteRegexMatchSet = DeleteRegexMatchSet'
  { -- | The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to delete. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
    regexMatchSetId :: Types.RegexMatchSetId,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRegexMatchSet' value with any optional fields omitted.
mkDeleteRegexMatchSet ::
  -- | 'regexMatchSetId'
  Types.RegexMatchSetId ->
  -- | 'changeToken'
  Types.ChangeToken ->
  DeleteRegexMatchSet
mkDeleteRegexMatchSet regexMatchSetId changeToken =
  DeleteRegexMatchSet' {regexMatchSetId, changeToken}

-- | The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to delete. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
--
-- /Note:/ Consider using 'regexMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drmsRegexMatchSetId :: Lens.Lens' DeleteRegexMatchSet Types.RegexMatchSetId
drmsRegexMatchSetId = Lens.field @"regexMatchSetId"
{-# DEPRECATED drmsRegexMatchSetId "Use generic-lens or generic-optics with 'regexMatchSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drmsChangeToken :: Lens.Lens' DeleteRegexMatchSet Types.ChangeToken
drmsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED drmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Core.FromJSON DeleteRegexMatchSet where
  toJSON DeleteRegexMatchSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RegexMatchSetId" Core..= regexMatchSetId),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.AWSRequest DeleteRegexMatchSet where
  type Rs DeleteRegexMatchSet = DeleteRegexMatchSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSWAF_Regional_20161128.DeleteRegexMatchSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRegexMatchSetResponse'
            Core.<$> (x Core..:? "ChangeToken") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteRegexMatchSetResponse' smart constructor.
data DeleteRegexMatchSetResponse = DeleteRegexMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRegexMatchSetResponse' value with any optional fields omitted.
mkDeleteRegexMatchSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteRegexMatchSetResponse
mkDeleteRegexMatchSetResponse responseStatus =
  DeleteRegexMatchSetResponse'
    { changeToken = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @DeleteRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drmsrrsChangeToken :: Lens.Lens' DeleteRegexMatchSetResponse (Core.Maybe Types.ChangeToken)
drmsrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED drmsrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drmsrrsResponseStatus :: Lens.Lens' DeleteRegexMatchSetResponse Core.Int
drmsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drmsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
