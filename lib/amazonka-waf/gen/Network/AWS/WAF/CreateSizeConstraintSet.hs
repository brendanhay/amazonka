{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateSizeConstraintSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @SizeConstraintSet@ . You then use 'UpdateSizeConstraintSet' to identify the part of a web request that you want AWS WAF to check for length, such as the length of the @User-Agent@ header or the length of the query string. For example, you can create a @SizeConstraintSet@ that matches any requests that have a query string that is longer than 100 bytes. You can then configure AWS WAF to reject those requests.
--
-- To create and configure a @SizeConstraintSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateSizeConstraintSet@ request.
--
--
--     * Submit a @CreateSizeConstraintSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateSizeConstraintSet@ request.
--
--
--     * Submit an 'UpdateSizeConstraintSet' request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateSizeConstraintSet
  ( -- * Creating a request
    CreateSizeConstraintSet (..),
    mkCreateSizeConstraintSet,

    -- ** Request lenses
    cscsName,
    cscsChangeToken,

    -- * Destructuring the response
    CreateSizeConstraintSetResponse (..),
    mkCreateSizeConstraintSetResponse,

    -- ** Response lenses
    cscsrrsChangeToken,
    cscsrrsSizeConstraintSet,
    cscsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkCreateSizeConstraintSet' smart constructor.
data CreateSizeConstraintSet = CreateSizeConstraintSet'
  { -- | A friendly name or description of the 'SizeConstraintSet' . You can't change @Name@ after you create a @SizeConstraintSet@ .
    name :: Types.ResourceName,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSizeConstraintSet' value with any optional fields omitted.
mkCreateSizeConstraintSet ::
  -- | 'name'
  Types.ResourceName ->
  -- | 'changeToken'
  Types.ChangeToken ->
  CreateSizeConstraintSet
mkCreateSizeConstraintSet name changeToken =
  CreateSizeConstraintSet' {name, changeToken}

-- | A friendly name or description of the 'SizeConstraintSet' . You can't change @Name@ after you create a @SizeConstraintSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsName :: Lens.Lens' CreateSizeConstraintSet Types.ResourceName
cscsName = Lens.field @"name"
{-# DEPRECATED cscsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsChangeToken :: Lens.Lens' CreateSizeConstraintSet Types.ChangeToken
cscsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED cscsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Core.FromJSON CreateSizeConstraintSet where
  toJSON CreateSizeConstraintSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.AWSRequest CreateSizeConstraintSet where
  type Rs CreateSizeConstraintSet = CreateSizeConstraintSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSWAF_20150824.CreateSizeConstraintSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSizeConstraintSetResponse'
            Core.<$> (x Core..:? "ChangeToken")
            Core.<*> (x Core..:? "SizeConstraintSet")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSizeConstraintSetResponse' smart constructor.
data CreateSizeConstraintSetResponse = CreateSizeConstraintSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @CreateSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | A 'SizeConstraintSet' that contains no @SizeConstraint@ objects.
    sizeConstraintSet :: Core.Maybe Types.SizeConstraintSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSizeConstraintSetResponse' value with any optional fields omitted.
mkCreateSizeConstraintSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSizeConstraintSetResponse
mkCreateSizeConstraintSetResponse responseStatus =
  CreateSizeConstraintSetResponse'
    { changeToken = Core.Nothing,
      sizeConstraintSet = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @CreateSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsrrsChangeToken :: Lens.Lens' CreateSizeConstraintSetResponse (Core.Maybe Types.ChangeToken)
cscsrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED cscsrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | A 'SizeConstraintSet' that contains no @SizeConstraint@ objects.
--
-- /Note:/ Consider using 'sizeConstraintSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsrrsSizeConstraintSet :: Lens.Lens' CreateSizeConstraintSetResponse (Core.Maybe Types.SizeConstraintSet)
cscsrrsSizeConstraintSet = Lens.field @"sizeConstraintSet"
{-# DEPRECATED cscsrrsSizeConstraintSet "Use generic-lens or generic-optics with 'sizeConstraintSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsrrsResponseStatus :: Lens.Lens' CreateSizeConstraintSetResponse Core.Int
cscsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cscsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
