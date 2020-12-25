{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.UpdateSqlInjectionMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'SqlInjectionMatchTuple' objects (filters) in a 'SqlInjectionMatchSet' . For each @SqlInjectionMatchTuple@ object, you specify the following values:
--
--
--     * @Action@ : Whether to insert the object into or delete the object from the array. To change a @SqlInjectionMatchTuple@ , you delete the existing object and add a new one.
--
--
--     * @FieldToMatch@ : The part of web requests that you want AWS WAF to inspect and, if you want AWS WAF to inspect a header or custom query parameter, the name of the header or parameter.
--
--
--     * @TextTransformation@ : Which text transformation, if any, to perform on the web request before inspecting the request for snippets of malicious SQL code.
-- You can only specify a single type of TextTransformation.
--
--
-- You use @SqlInjectionMatchSet@ objects to specify which CloudFront requests that you want to allow, block, or count. For example, if you're receiving requests that contain snippets of SQL code in the query string and you want to block the requests, you can create a @SqlInjectionMatchSet@ with the applicable settings, and then configure AWS WAF to block the requests.
-- To create and configure a @SqlInjectionMatchSet@ , perform the following steps:
--
--     * Submit a 'CreateSqlInjectionMatchSet' request.
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateIPSet' request.
--
--
--     * Submit an @UpdateSqlInjectionMatchSet@ request to specify the parts of web requests that you want AWS WAF to inspect for snippets of SQL code.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.UpdateSqlInjectionMatchSet
  ( -- * Creating a request
    UpdateSqlInjectionMatchSet (..),
    mkUpdateSqlInjectionMatchSet,

    -- ** Request lenses
    usimsSqlInjectionMatchSetId,
    usimsChangeToken,
    usimsUpdates,

    -- * Destructuring the response
    UpdateSqlInjectionMatchSetResponse (..),
    mkUpdateSqlInjectionMatchSetResponse,

    -- ** Response lenses
    usimsrrsChangeToken,
    usimsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | A request to update a 'SqlInjectionMatchSet' .
--
-- /See:/ 'mkUpdateSqlInjectionMatchSet' smart constructor.
data UpdateSqlInjectionMatchSet = UpdateSqlInjectionMatchSet'
  { -- | The @SqlInjectionMatchSetId@ of the @SqlInjectionMatchSet@ that you want to update. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
    sqlInjectionMatchSetId :: Types.ResourceId,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken,
    -- | An array of @SqlInjectionMatchSetUpdate@ objects that you want to insert into or delete from a 'SqlInjectionMatchSet' . For more information, see the applicable data types:
    --
    --
    --     * 'SqlInjectionMatchSetUpdate' : Contains @Action@ and @SqlInjectionMatchTuple@
    --
    --
    --     * 'SqlInjectionMatchTuple' : Contains @FieldToMatch@ and @TextTransformation@
    --
    --
    --     * 'FieldToMatch' : Contains @Data@ and @Type@
    updates :: Core.NonEmpty Types.SqlInjectionMatchSetUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSqlInjectionMatchSet' value with any optional fields omitted.
mkUpdateSqlInjectionMatchSet ::
  -- | 'sqlInjectionMatchSetId'
  Types.ResourceId ->
  -- | 'changeToken'
  Types.ChangeToken ->
  -- | 'updates'
  Core.NonEmpty Types.SqlInjectionMatchSetUpdate ->
  UpdateSqlInjectionMatchSet
mkUpdateSqlInjectionMatchSet
  sqlInjectionMatchSetId
  changeToken
  updates =
    UpdateSqlInjectionMatchSet'
      { sqlInjectionMatchSetId,
        changeToken,
        updates
      }

-- | The @SqlInjectionMatchSetId@ of the @SqlInjectionMatchSet@ that you want to update. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- /Note:/ Consider using 'sqlInjectionMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usimsSqlInjectionMatchSetId :: Lens.Lens' UpdateSqlInjectionMatchSet Types.ResourceId
usimsSqlInjectionMatchSetId = Lens.field @"sqlInjectionMatchSetId"
{-# DEPRECATED usimsSqlInjectionMatchSetId "Use generic-lens or generic-optics with 'sqlInjectionMatchSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usimsChangeToken :: Lens.Lens' UpdateSqlInjectionMatchSet Types.ChangeToken
usimsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED usimsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | An array of @SqlInjectionMatchSetUpdate@ objects that you want to insert into or delete from a 'SqlInjectionMatchSet' . For more information, see the applicable data types:
--
--
--     * 'SqlInjectionMatchSetUpdate' : Contains @Action@ and @SqlInjectionMatchTuple@
--
--
--     * 'SqlInjectionMatchTuple' : Contains @FieldToMatch@ and @TextTransformation@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usimsUpdates :: Lens.Lens' UpdateSqlInjectionMatchSet (Core.NonEmpty Types.SqlInjectionMatchSetUpdate)
usimsUpdates = Lens.field @"updates"
{-# DEPRECATED usimsUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

instance Core.FromJSON UpdateSqlInjectionMatchSet where
  toJSON UpdateSqlInjectionMatchSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("SqlInjectionMatchSetId" Core..= sqlInjectionMatchSetId),
            Core.Just ("ChangeToken" Core..= changeToken),
            Core.Just ("Updates" Core..= updates)
          ]
      )

instance Core.AWSRequest UpdateSqlInjectionMatchSet where
  type
    Rs UpdateSqlInjectionMatchSet =
      UpdateSqlInjectionMatchSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSWAF_20150824.UpdateSqlInjectionMatchSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSqlInjectionMatchSetResponse'
            Core.<$> (x Core..:? "ChangeToken") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The response to an 'UpdateSqlInjectionMatchSets' request.
--
-- /See:/ 'mkUpdateSqlInjectionMatchSetResponse' smart constructor.
data UpdateSqlInjectionMatchSetResponse = UpdateSqlInjectionMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSqlInjectionMatchSetResponse' value with any optional fields omitted.
mkUpdateSqlInjectionMatchSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateSqlInjectionMatchSetResponse
mkUpdateSqlInjectionMatchSetResponse responseStatus =
  UpdateSqlInjectionMatchSetResponse'
    { changeToken = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @UpdateSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usimsrrsChangeToken :: Lens.Lens' UpdateSqlInjectionMatchSetResponse (Core.Maybe Types.ChangeToken)
usimsrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED usimsrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usimsrrsResponseStatus :: Lens.Lens' UpdateSqlInjectionMatchSetResponse Core.Int
usimsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usimsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
