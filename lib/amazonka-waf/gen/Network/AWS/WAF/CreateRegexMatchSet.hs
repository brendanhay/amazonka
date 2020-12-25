{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateRegexMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'RegexMatchSet' . You then use 'UpdateRegexMatchSet' to identify the part of a web request that you want AWS WAF to inspect, such as the values of the @User-Agent@ header or the query string. For example, you can create a @RegexMatchSet@ that contains a @RegexMatchTuple@ that looks for any requests with @User-Agent@ headers that match a @RegexPatternSet@ with pattern @B[a@]dB[o0]t@ . You can then configure AWS WAF to reject those requests.
--
-- To create and configure a @RegexMatchSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateRegexMatchSet@ request.
--
--
--     * Submit a @CreateRegexMatchSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateRegexMatchSet@ request.
--
--
--     * Submit an 'UpdateRegexMatchSet' request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value, using a @RegexPatternSet@ , that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateRegexMatchSet
  ( -- * Creating a request
    CreateRegexMatchSet (..),
    mkCreateRegexMatchSet,

    -- ** Request lenses
    crmsName,
    crmsChangeToken,

    -- * Destructuring the response
    CreateRegexMatchSetResponse (..),
    mkCreateRegexMatchSetResponse,

    -- ** Response lenses
    crmsrrsChangeToken,
    crmsrrsRegexMatchSet,
    crmsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkCreateRegexMatchSet' smart constructor.
data CreateRegexMatchSet = CreateRegexMatchSet'
  { -- | A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
    name :: Types.ResourceName,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRegexMatchSet' value with any optional fields omitted.
mkCreateRegexMatchSet ::
  -- | 'name'
  Types.ResourceName ->
  -- | 'changeToken'
  Types.ChangeToken ->
  CreateRegexMatchSet
mkCreateRegexMatchSet name changeToken =
  CreateRegexMatchSet' {name, changeToken}

-- | A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crmsName :: Lens.Lens' CreateRegexMatchSet Types.ResourceName
crmsName = Lens.field @"name"
{-# DEPRECATED crmsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crmsChangeToken :: Lens.Lens' CreateRegexMatchSet Types.ChangeToken
crmsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED crmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Core.FromJSON CreateRegexMatchSet where
  toJSON CreateRegexMatchSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.AWSRequest CreateRegexMatchSet where
  type Rs CreateRegexMatchSet = CreateRegexMatchSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_20150824.CreateRegexMatchSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRegexMatchSetResponse'
            Core.<$> (x Core..:? "ChangeToken")
            Core.<*> (x Core..:? "RegexMatchSet")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateRegexMatchSetResponse' smart constructor.
data CreateRegexMatchSetResponse = CreateRegexMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @CreateRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | A 'RegexMatchSet' that contains no @RegexMatchTuple@ objects.
    regexMatchSet :: Core.Maybe Types.RegexMatchSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRegexMatchSetResponse' value with any optional fields omitted.
mkCreateRegexMatchSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateRegexMatchSetResponse
mkCreateRegexMatchSetResponse responseStatus =
  CreateRegexMatchSetResponse'
    { changeToken = Core.Nothing,
      regexMatchSet = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @CreateRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crmsrrsChangeToken :: Lens.Lens' CreateRegexMatchSetResponse (Core.Maybe Types.ChangeToken)
crmsrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED crmsrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | A 'RegexMatchSet' that contains no @RegexMatchTuple@ objects.
--
-- /Note:/ Consider using 'regexMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crmsrrsRegexMatchSet :: Lens.Lens' CreateRegexMatchSetResponse (Core.Maybe Types.RegexMatchSet)
crmsrrsRegexMatchSet = Lens.field @"regexMatchSet"
{-# DEPRECATED crmsrrsRegexMatchSet "Use generic-lens or generic-optics with 'regexMatchSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crmsrrsResponseStatus :: Lens.Lens' CreateRegexMatchSetResponse Core.Int
crmsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crmsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
