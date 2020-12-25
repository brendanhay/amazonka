{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.DeleteSizeConstraintSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'SizeConstraintSet' . You can't delete a @SizeConstraintSet@ if it's still used in any @Rules@ or if it still includes any 'SizeConstraint' objects (any filters).
--
-- If you just want to remove a @SizeConstraintSet@ from a @Rule@ , use 'UpdateRule' .
-- To permanently delete a @SizeConstraintSet@ , perform the following steps:
--
--     * Update the @SizeConstraintSet@ to remove filters, if any. For more information, see 'UpdateSizeConstraintSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteSizeConstraintSet@ request.
--
--
--     * Submit a @DeleteSizeConstraintSet@ request.
module Network.AWS.WAFRegional.DeleteSizeConstraintSet
  ( -- * Creating a request
    DeleteSizeConstraintSet (..),
    mkDeleteSizeConstraintSet,

    -- ** Request lenses
    dscsSizeConstraintSetId,
    dscsChangeToken,

    -- * Destructuring the response
    DeleteSizeConstraintSetResponse (..),
    mkDeleteSizeConstraintSetResponse,

    -- ** Response lenses
    dscsrrsChangeToken,
    dscsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkDeleteSizeConstraintSet' smart constructor.
data DeleteSizeConstraintSet = DeleteSizeConstraintSet'
  { -- | The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to delete. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
    sizeConstraintSetId :: Types.ResourceId,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSizeConstraintSet' value with any optional fields omitted.
mkDeleteSizeConstraintSet ::
  -- | 'sizeConstraintSetId'
  Types.ResourceId ->
  -- | 'changeToken'
  Types.ChangeToken ->
  DeleteSizeConstraintSet
mkDeleteSizeConstraintSet sizeConstraintSetId changeToken =
  DeleteSizeConstraintSet' {sizeConstraintSetId, changeToken}

-- | The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to delete. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
--
-- /Note:/ Consider using 'sizeConstraintSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsSizeConstraintSetId :: Lens.Lens' DeleteSizeConstraintSet Types.ResourceId
dscsSizeConstraintSetId = Lens.field @"sizeConstraintSetId"
{-# DEPRECATED dscsSizeConstraintSetId "Use generic-lens or generic-optics with 'sizeConstraintSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsChangeToken :: Lens.Lens' DeleteSizeConstraintSet Types.ChangeToken
dscsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED dscsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Core.FromJSON DeleteSizeConstraintSet where
  toJSON DeleteSizeConstraintSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SizeConstraintSetId" Core..= sizeConstraintSetId),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.AWSRequest DeleteSizeConstraintSet where
  type Rs DeleteSizeConstraintSet = DeleteSizeConstraintSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSWAF_Regional_20161128.DeleteSizeConstraintSet"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSizeConstraintSetResponse'
            Core.<$> (x Core..:? "ChangeToken") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteSizeConstraintSetResponse' smart constructor.
data DeleteSizeConstraintSetResponse = DeleteSizeConstraintSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSizeConstraintSetResponse' value with any optional fields omitted.
mkDeleteSizeConstraintSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteSizeConstraintSetResponse
mkDeleteSizeConstraintSetResponse responseStatus =
  DeleteSizeConstraintSetResponse'
    { changeToken = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @DeleteSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsrrsChangeToken :: Lens.Lens' DeleteSizeConstraintSetResponse (Core.Maybe Types.ChangeToken)
dscsrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED dscsrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsrrsResponseStatus :: Lens.Lens' DeleteSizeConstraintSetResponse Core.Int
dscsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dscsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
