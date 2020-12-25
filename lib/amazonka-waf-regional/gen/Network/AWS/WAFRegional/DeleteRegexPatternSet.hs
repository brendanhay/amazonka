{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteRegexPatternSet (..),
    mkDeleteRegexPatternSet,

    -- ** Request lenses
    drpsRegexPatternSetId,
    drpsChangeToken,

    -- * Destructuring the response
    DeleteRegexPatternSetResponse (..),
    mkDeleteRegexPatternSetResponse,

    -- ** Response lenses
    drpsrrsChangeToken,
    drpsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkDeleteRegexPatternSet' smart constructor.
data DeleteRegexPatternSet = DeleteRegexPatternSet'
  { -- | The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to delete. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
    regexPatternSetId :: Types.ResourceId,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRegexPatternSet' value with any optional fields omitted.
mkDeleteRegexPatternSet ::
  -- | 'regexPatternSetId'
  Types.ResourceId ->
  -- | 'changeToken'
  Types.ChangeToken ->
  DeleteRegexPatternSet
mkDeleteRegexPatternSet regexPatternSetId changeToken =
  DeleteRegexPatternSet' {regexPatternSetId, changeToken}

-- | The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to delete. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- /Note:/ Consider using 'regexPatternSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpsRegexPatternSetId :: Lens.Lens' DeleteRegexPatternSet Types.ResourceId
drpsRegexPatternSetId = Lens.field @"regexPatternSetId"
{-# DEPRECATED drpsRegexPatternSetId "Use generic-lens or generic-optics with 'regexPatternSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpsChangeToken :: Lens.Lens' DeleteRegexPatternSet Types.ChangeToken
drpsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED drpsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Core.FromJSON DeleteRegexPatternSet where
  toJSON DeleteRegexPatternSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RegexPatternSetId" Core..= regexPatternSetId),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.AWSRequest DeleteRegexPatternSet where
  type Rs DeleteRegexPatternSet = DeleteRegexPatternSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSWAF_Regional_20161128.DeleteRegexPatternSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRegexPatternSetResponse'
            Core.<$> (x Core..:? "ChangeToken") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteRegexPatternSetResponse' smart constructor.
data DeleteRegexPatternSetResponse = DeleteRegexPatternSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRegexPatternSetResponse' value with any optional fields omitted.
mkDeleteRegexPatternSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteRegexPatternSetResponse
mkDeleteRegexPatternSetResponse responseStatus =
  DeleteRegexPatternSetResponse'
    { changeToken = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @DeleteRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpsrrsChangeToken :: Lens.Lens' DeleteRegexPatternSetResponse (Core.Maybe Types.ChangeToken)
drpsrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED drpsrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpsrrsResponseStatus :: Lens.Lens' DeleteRegexPatternSetResponse Core.Int
drpsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drpsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
