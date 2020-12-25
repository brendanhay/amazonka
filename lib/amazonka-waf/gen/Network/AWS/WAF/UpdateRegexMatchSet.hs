{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.UpdateRegexMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'RegexMatchTuple' objects (filters) in a 'RegexMatchSet' . For each @RegexMatchSetUpdate@ object, you specify the following values:
--
--
--     * Whether to insert or delete the object from the array. If you want to change a @RegexMatchSetUpdate@ object, you delete the existing object and add a new one.
--
--
--     * The part of a web request that you want AWS WAF to inspectupdate, such as a query string or the value of the @User-Agent@ header.
--
--
--     * The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see 'RegexPatternSet' .
--
--
--     * Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.
--
--
-- For example, you can create a @RegexPatternSet@ that matches any requests with @User-Agent@ headers that contain the string @B[a@]dB[o0]t@ . You can then configure AWS WAF to reject those requests.
-- To create and configure a @RegexMatchSet@ , perform the following steps:
--
--     * Create a @RegexMatchSet.@ For more information, see 'CreateRegexMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateRegexMatchSet@ request.
--
--
--     * Submit an @UpdateRegexMatchSet@ request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the identifier of the @RegexPatternSet@ that contain the regular expression patters you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.UpdateRegexMatchSet
  ( -- * Creating a request
    UpdateRegexMatchSet (..),
    mkUpdateRegexMatchSet,

    -- ** Request lenses
    urmsRegexMatchSetId,
    urmsUpdates,
    urmsChangeToken,

    -- * Destructuring the response
    UpdateRegexMatchSetResponse (..),
    mkUpdateRegexMatchSetResponse,

    -- ** Response lenses
    urmsrrsChangeToken,
    urmsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkUpdateRegexMatchSet' smart constructor.
data UpdateRegexMatchSet = UpdateRegexMatchSet'
  { -- | The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to update. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
    regexMatchSetId :: Types.RegexMatchSetId,
    -- | An array of @RegexMatchSetUpdate@ objects that you want to insert into or delete from a 'RegexMatchSet' . For more information, see 'RegexMatchTuple' .
    updates :: Core.NonEmpty Types.RegexMatchSetUpdate,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRegexMatchSet' value with any optional fields omitted.
mkUpdateRegexMatchSet ::
  -- | 'regexMatchSetId'
  Types.RegexMatchSetId ->
  -- | 'updates'
  Core.NonEmpty Types.RegexMatchSetUpdate ->
  -- | 'changeToken'
  Types.ChangeToken ->
  UpdateRegexMatchSet
mkUpdateRegexMatchSet regexMatchSetId updates changeToken =
  UpdateRegexMatchSet' {regexMatchSetId, updates, changeToken}

-- | The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to update. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
--
-- /Note:/ Consider using 'regexMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urmsRegexMatchSetId :: Lens.Lens' UpdateRegexMatchSet Types.RegexMatchSetId
urmsRegexMatchSetId = Lens.field @"regexMatchSetId"
{-# DEPRECATED urmsRegexMatchSetId "Use generic-lens or generic-optics with 'regexMatchSetId' instead." #-}

-- | An array of @RegexMatchSetUpdate@ objects that you want to insert into or delete from a 'RegexMatchSet' . For more information, see 'RegexMatchTuple' .
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urmsUpdates :: Lens.Lens' UpdateRegexMatchSet (Core.NonEmpty Types.RegexMatchSetUpdate)
urmsUpdates = Lens.field @"updates"
{-# DEPRECATED urmsUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urmsChangeToken :: Lens.Lens' UpdateRegexMatchSet Types.ChangeToken
urmsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED urmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Core.FromJSON UpdateRegexMatchSet where
  toJSON UpdateRegexMatchSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RegexMatchSetId" Core..= regexMatchSetId),
            Core.Just ("Updates" Core..= updates),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.AWSRequest UpdateRegexMatchSet where
  type Rs UpdateRegexMatchSet = UpdateRegexMatchSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_20150824.UpdateRegexMatchSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRegexMatchSetResponse'
            Core.<$> (x Core..:? "ChangeToken") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateRegexMatchSetResponse' smart constructor.
data UpdateRegexMatchSetResponse = UpdateRegexMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRegexMatchSetResponse' value with any optional fields omitted.
mkUpdateRegexMatchSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateRegexMatchSetResponse
mkUpdateRegexMatchSetResponse responseStatus =
  UpdateRegexMatchSetResponse'
    { changeToken = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @UpdateRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urmsrrsChangeToken :: Lens.Lens' UpdateRegexMatchSetResponse (Core.Maybe Types.ChangeToken)
urmsrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED urmsrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urmsrrsResponseStatus :: Lens.Lens' UpdateRegexMatchSetResponse Core.Int
urmsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urmsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
