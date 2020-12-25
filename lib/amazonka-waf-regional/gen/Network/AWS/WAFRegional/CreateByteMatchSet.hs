{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.CreateByteMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @ByteMatchSet@ . You then use 'UpdateByteMatchSet' to identify the part of a web request that you want AWS WAF to inspect, such as the values of the @User-Agent@ header or the query string. For example, you can create a @ByteMatchSet@ that matches any requests with @User-Agent@ headers that contain the string @BadBot@ . You can then configure AWS WAF to reject those requests.
--
-- To create and configure a @ByteMatchSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateByteMatchSet@ request.
--
--
--     * Submit a @CreateByteMatchSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateByteMatchSet@ request.
--
--
--     * Submit an 'UpdateByteMatchSet' request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.CreateByteMatchSet
  ( -- * Creating a request
    CreateByteMatchSet (..),
    mkCreateByteMatchSet,

    -- ** Request lenses
    cbmsName,
    cbmsChangeToken,

    -- * Destructuring the response
    CreateByteMatchSetResponse (..),
    mkCreateByteMatchSetResponse,

    -- ** Response lenses
    cbmsrrsByteMatchSet,
    cbmsrrsChangeToken,
    cbmsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkCreateByteMatchSet' smart constructor.
data CreateByteMatchSet = CreateByteMatchSet'
  { -- | A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
    name :: Types.ResourceName,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateByteMatchSet' value with any optional fields omitted.
mkCreateByteMatchSet ::
  -- | 'name'
  Types.ResourceName ->
  -- | 'changeToken'
  Types.ChangeToken ->
  CreateByteMatchSet
mkCreateByteMatchSet name changeToken =
  CreateByteMatchSet' {name, changeToken}

-- | A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbmsName :: Lens.Lens' CreateByteMatchSet Types.ResourceName
cbmsName = Lens.field @"name"
{-# DEPRECATED cbmsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbmsChangeToken :: Lens.Lens' CreateByteMatchSet Types.ChangeToken
cbmsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED cbmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Core.FromJSON CreateByteMatchSet where
  toJSON CreateByteMatchSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.AWSRequest CreateByteMatchSet where
  type Rs CreateByteMatchSet = CreateByteMatchSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSWAF_Regional_20161128.CreateByteMatchSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateByteMatchSetResponse'
            Core.<$> (x Core..:? "ByteMatchSet")
            Core.<*> (x Core..:? "ChangeToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateByteMatchSetResponse' smart constructor.
data CreateByteMatchSetResponse = CreateByteMatchSetResponse'
  { -- | A 'ByteMatchSet' that contains no @ByteMatchTuple@ objects.
    byteMatchSet :: Core.Maybe Types.ByteMatchSet,
    -- | The @ChangeToken@ that you used to submit the @CreateByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateByteMatchSetResponse' value with any optional fields omitted.
mkCreateByteMatchSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateByteMatchSetResponse
mkCreateByteMatchSetResponse responseStatus =
  CreateByteMatchSetResponse'
    { byteMatchSet = Core.Nothing,
      changeToken = Core.Nothing,
      responseStatus
    }

-- | A 'ByteMatchSet' that contains no @ByteMatchTuple@ objects.
--
-- /Note:/ Consider using 'byteMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbmsrrsByteMatchSet :: Lens.Lens' CreateByteMatchSetResponse (Core.Maybe Types.ByteMatchSet)
cbmsrrsByteMatchSet = Lens.field @"byteMatchSet"
{-# DEPRECATED cbmsrrsByteMatchSet "Use generic-lens or generic-optics with 'byteMatchSet' instead." #-}

-- | The @ChangeToken@ that you used to submit the @CreateByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbmsrrsChangeToken :: Lens.Lens' CreateByteMatchSetResponse (Core.Maybe Types.ChangeToken)
cbmsrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED cbmsrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbmsrrsResponseStatus :: Lens.Lens' CreateByteMatchSetResponse Core.Int
cbmsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cbmsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
