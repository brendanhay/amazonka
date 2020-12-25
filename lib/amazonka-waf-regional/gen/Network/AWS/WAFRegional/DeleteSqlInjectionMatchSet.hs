{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.DeleteSqlInjectionMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'SqlInjectionMatchSet' . You can't delete a @SqlInjectionMatchSet@ if it's still used in any @Rules@ or if it still contains any 'SqlInjectionMatchTuple' objects.
--
-- If you just want to remove a @SqlInjectionMatchSet@ from a @Rule@ , use 'UpdateRule' .
-- To permanently delete a @SqlInjectionMatchSet@ from AWS WAF, perform the following steps:
--
--     * Update the @SqlInjectionMatchSet@ to remove filters, if any. For more information, see 'UpdateSqlInjectionMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteSqlInjectionMatchSet@ request.
--
--
--     * Submit a @DeleteSqlInjectionMatchSet@ request.
module Network.AWS.WAFRegional.DeleteSqlInjectionMatchSet
  ( -- * Creating a request
    DeleteSqlInjectionMatchSet (..),
    mkDeleteSqlInjectionMatchSet,

    -- ** Request lenses
    dsimsSqlInjectionMatchSetId,
    dsimsChangeToken,

    -- * Destructuring the response
    DeleteSqlInjectionMatchSetResponse (..),
    mkDeleteSqlInjectionMatchSetResponse,

    -- ** Response lenses
    dsimsrrsChangeToken,
    dsimsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | A request to delete a 'SqlInjectionMatchSet' from AWS WAF.
--
-- /See:/ 'mkDeleteSqlInjectionMatchSet' smart constructor.
data DeleteSqlInjectionMatchSet = DeleteSqlInjectionMatchSet'
  { -- | The @SqlInjectionMatchSetId@ of the 'SqlInjectionMatchSet' that you want to delete. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
    sqlInjectionMatchSetId :: Types.ResourceId,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSqlInjectionMatchSet' value with any optional fields omitted.
mkDeleteSqlInjectionMatchSet ::
  -- | 'sqlInjectionMatchSetId'
  Types.ResourceId ->
  -- | 'changeToken'
  Types.ChangeToken ->
  DeleteSqlInjectionMatchSet
mkDeleteSqlInjectionMatchSet sqlInjectionMatchSetId changeToken =
  DeleteSqlInjectionMatchSet' {sqlInjectionMatchSetId, changeToken}

-- | The @SqlInjectionMatchSetId@ of the 'SqlInjectionMatchSet' that you want to delete. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- /Note:/ Consider using 'sqlInjectionMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsimsSqlInjectionMatchSetId :: Lens.Lens' DeleteSqlInjectionMatchSet Types.ResourceId
dsimsSqlInjectionMatchSetId = Lens.field @"sqlInjectionMatchSetId"
{-# DEPRECATED dsimsSqlInjectionMatchSetId "Use generic-lens or generic-optics with 'sqlInjectionMatchSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsimsChangeToken :: Lens.Lens' DeleteSqlInjectionMatchSet Types.ChangeToken
dsimsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED dsimsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Core.FromJSON DeleteSqlInjectionMatchSet where
  toJSON DeleteSqlInjectionMatchSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("SqlInjectionMatchSetId" Core..= sqlInjectionMatchSetId),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.AWSRequest DeleteSqlInjectionMatchSet where
  type
    Rs DeleteSqlInjectionMatchSet =
      DeleteSqlInjectionMatchSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSWAF_Regional_20161128.DeleteSqlInjectionMatchSet"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSqlInjectionMatchSetResponse'
            Core.<$> (x Core..:? "ChangeToken") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The response to a request to delete a 'SqlInjectionMatchSet' from AWS WAF.
--
-- /See:/ 'mkDeleteSqlInjectionMatchSetResponse' smart constructor.
data DeleteSqlInjectionMatchSetResponse = DeleteSqlInjectionMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSqlInjectionMatchSetResponse' value with any optional fields omitted.
mkDeleteSqlInjectionMatchSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteSqlInjectionMatchSetResponse
mkDeleteSqlInjectionMatchSetResponse responseStatus =
  DeleteSqlInjectionMatchSetResponse'
    { changeToken = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @DeleteSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsimsrrsChangeToken :: Lens.Lens' DeleteSqlInjectionMatchSetResponse (Core.Maybe Types.ChangeToken)
dsimsrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED dsimsrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsimsrrsResponseStatus :: Lens.Lens' DeleteSqlInjectionMatchSetResponse Core.Int
dsimsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsimsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
