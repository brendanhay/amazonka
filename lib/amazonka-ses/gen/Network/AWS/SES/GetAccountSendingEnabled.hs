{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetAccountSendingEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the email sending status of the Amazon SES account for the current region.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.GetAccountSendingEnabled
  ( -- * Creating a request
    GetAccountSendingEnabled (..),
    mkGetAccountSendingEnabled,

    -- * Destructuring the response
    GetAccountSendingEnabledResponse (..),
    mkGetAccountSendingEnabledResponse,

    -- ** Response lenses
    gaserrsEnabled,
    gaserrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | /See:/ 'mkGetAccountSendingEnabled' smart constructor.
data GetAccountSendingEnabled = GetAccountSendingEnabled'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccountSendingEnabled' value with any optional fields omitted.
mkGetAccountSendingEnabled ::
  GetAccountSendingEnabled
mkGetAccountSendingEnabled = GetAccountSendingEnabled'

instance Core.AWSRequest GetAccountSendingEnabled where
  type Rs GetAccountSendingEnabled = GetAccountSendingEnabledResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GetAccountSendingEnabled")
                Core.<> (Core.pure ("Version", "2010-12-01"))
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetAccountSendingEnabledResult"
      ( \s h x ->
          GetAccountSendingEnabledResponse'
            Core.<$> (x Core..@? "Enabled") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents a request to return the email sending status for your Amazon SES account in the current AWS Region.
--
-- /See:/ 'mkGetAccountSendingEnabledResponse' smart constructor.
data GetAccountSendingEnabledResponse = GetAccountSendingEnabledResponse'
  { -- | Describes whether email sending is enabled or disabled for your Amazon SES account in the current AWS Region.
    enabled :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccountSendingEnabledResponse' value with any optional fields omitted.
mkGetAccountSendingEnabledResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAccountSendingEnabledResponse
mkGetAccountSendingEnabledResponse responseStatus =
  GetAccountSendingEnabledResponse'
    { enabled = Core.Nothing,
      responseStatus
    }

-- | Describes whether email sending is enabled or disabled for your Amazon SES account in the current AWS Region.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaserrsEnabled :: Lens.Lens' GetAccountSendingEnabledResponse (Core.Maybe Core.Bool)
gaserrsEnabled = Lens.field @"enabled"
{-# DEPRECATED gaserrsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaserrsResponseStatus :: Lens.Lens' GetAccountSendingEnabledResponse Core.Int
gaserrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gaserrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
