{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetChangeTokenStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of a @ChangeToken@ that you got by calling 'GetChangeToken' . @ChangeTokenStatus@ is one of the following values:
--
--
--     * @PROVISIONED@ : You requested the change token by calling @GetChangeToken@ , but you haven't used it yet in a call to create, update, or delete an AWS WAF object.
--
--
--     * @PENDING@ : AWS WAF is propagating the create, update, or delete request to all AWS WAF servers.
--
--
--     * @INSYNC@ : Propagation is complete.
module Network.AWS.WAFRegional.GetChangeTokenStatus
  ( -- * Creating a request
    GetChangeTokenStatus (..),
    mkGetChangeTokenStatus,

    -- ** Request lenses
    gctsChangeToken,

    -- * Destructuring the response
    GetChangeTokenStatusResponse (..),
    mkGetChangeTokenStatusResponse,

    -- ** Response lenses
    gctsrrsChangeTokenStatus,
    gctsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkGetChangeTokenStatus' smart constructor.
newtype GetChangeTokenStatus = GetChangeTokenStatus'
  { -- | The change token for which you want to get the status. This change token was previously returned in the @GetChangeToken@ response.
    changeToken :: Types.ChangeToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetChangeTokenStatus' value with any optional fields omitted.
mkGetChangeTokenStatus ::
  -- | 'changeToken'
  Types.ChangeToken ->
  GetChangeTokenStatus
mkGetChangeTokenStatus changeToken =
  GetChangeTokenStatus' {changeToken}

-- | The change token for which you want to get the status. This change token was previously returned in the @GetChangeToken@ response.
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gctsChangeToken :: Lens.Lens' GetChangeTokenStatus Types.ChangeToken
gctsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED gctsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Core.FromJSON GetChangeTokenStatus where
  toJSON GetChangeTokenStatus {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest GetChangeTokenStatus where
  type Rs GetChangeTokenStatus = GetChangeTokenStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSWAF_Regional_20161128.GetChangeTokenStatus")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChangeTokenStatusResponse'
            Core.<$> (x Core..:? "ChangeTokenStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetChangeTokenStatusResponse' smart constructor.
data GetChangeTokenStatusResponse = GetChangeTokenStatusResponse'
  { -- | The status of the change token.
    changeTokenStatus :: Core.Maybe Types.ChangeTokenStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetChangeTokenStatusResponse' value with any optional fields omitted.
mkGetChangeTokenStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetChangeTokenStatusResponse
mkGetChangeTokenStatusResponse responseStatus =
  GetChangeTokenStatusResponse'
    { changeTokenStatus = Core.Nothing,
      responseStatus
    }

-- | The status of the change token.
--
-- /Note:/ Consider using 'changeTokenStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gctsrrsChangeTokenStatus :: Lens.Lens' GetChangeTokenStatusResponse (Core.Maybe Types.ChangeTokenStatus)
gctsrrsChangeTokenStatus = Lens.field @"changeTokenStatus"
{-# DEPRECATED gctsrrsChangeTokenStatus "Use generic-lens or generic-optics with 'changeTokenStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gctsrrsResponseStatus :: Lens.Lens' GetChangeTokenStatusResponse Core.Int
gctsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gctsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
