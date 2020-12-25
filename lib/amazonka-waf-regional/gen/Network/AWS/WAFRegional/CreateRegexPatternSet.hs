{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.CreateRegexPatternSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @RegexPatternSet@ . You then use 'UpdateRegexPatternSet' to specify the regular expression (regex) pattern that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ . You can then configure AWS WAF to reject those requests.
--
-- To create and configure a @RegexPatternSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateRegexPatternSet@ request.
--
--
--     * Submit a @CreateRegexPatternSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateRegexPatternSet@ request.
--
--
--     * Submit an 'UpdateRegexPatternSet' request to specify the string that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.CreateRegexPatternSet
  ( -- * Creating a request
    CreateRegexPatternSet (..),
    mkCreateRegexPatternSet,

    -- ** Request lenses
    crpsName,
    crpsChangeToken,

    -- * Destructuring the response
    CreateRegexPatternSetResponse (..),
    mkCreateRegexPatternSetResponse,

    -- ** Response lenses
    crpsrrsChangeToken,
    crpsrrsRegexPatternSet,
    crpsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkCreateRegexPatternSet' smart constructor.
data CreateRegexPatternSet = CreateRegexPatternSet'
  { -- | A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
    name :: Types.ResourceName,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRegexPatternSet' value with any optional fields omitted.
mkCreateRegexPatternSet ::
  -- | 'name'
  Types.ResourceName ->
  -- | 'changeToken'
  Types.ChangeToken ->
  CreateRegexPatternSet
mkCreateRegexPatternSet name changeToken =
  CreateRegexPatternSet' {name, changeToken}

-- | A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpsName :: Lens.Lens' CreateRegexPatternSet Types.ResourceName
crpsName = Lens.field @"name"
{-# DEPRECATED crpsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpsChangeToken :: Lens.Lens' CreateRegexPatternSet Types.ChangeToken
crpsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED crpsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Core.FromJSON CreateRegexPatternSet where
  toJSON CreateRegexPatternSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.AWSRequest CreateRegexPatternSet where
  type Rs CreateRegexPatternSet = CreateRegexPatternSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSWAF_Regional_20161128.CreateRegexPatternSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRegexPatternSetResponse'
            Core.<$> (x Core..:? "ChangeToken")
            Core.<*> (x Core..:? "RegexPatternSet")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateRegexPatternSetResponse' smart constructor.
data CreateRegexPatternSetResponse = CreateRegexPatternSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @CreateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | A 'RegexPatternSet' that contains no objects.
    regexPatternSet :: Core.Maybe Types.RegexPatternSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRegexPatternSetResponse' value with any optional fields omitted.
mkCreateRegexPatternSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateRegexPatternSetResponse
mkCreateRegexPatternSetResponse responseStatus =
  CreateRegexPatternSetResponse'
    { changeToken = Core.Nothing,
      regexPatternSet = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @CreateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpsrrsChangeToken :: Lens.Lens' CreateRegexPatternSetResponse (Core.Maybe Types.ChangeToken)
crpsrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED crpsrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | A 'RegexPatternSet' that contains no objects.
--
-- /Note:/ Consider using 'regexPatternSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpsrrsRegexPatternSet :: Lens.Lens' CreateRegexPatternSetResponse (Core.Maybe Types.RegexPatternSet)
crpsrrsRegexPatternSet = Lens.field @"regexPatternSet"
{-# DEPRECATED crpsrrsRegexPatternSet "Use generic-lens or generic-optics with 'regexPatternSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpsrrsResponseStatus :: Lens.Lens' CreateRegexPatternSetResponse Core.Int
crpsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crpsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
