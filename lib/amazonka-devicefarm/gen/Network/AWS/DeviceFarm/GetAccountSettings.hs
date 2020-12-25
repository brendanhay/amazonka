{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetAccountSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of unmetered iOS or unmetered Android devices that have been purchased by the account.
module Network.AWS.DeviceFarm.GetAccountSettings
  ( -- * Creating a request
    GetAccountSettings (..),
    mkGetAccountSettings,

    -- * Destructuring the response
    GetAccountSettingsResponse (..),
    mkGetAccountSettingsResponse,

    -- ** Response lenses
    gasrrsAccountSettings,
    gasrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request sent to retrieve the account settings.
--
-- /See:/ 'mkGetAccountSettings' smart constructor.
data GetAccountSettings = GetAccountSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccountSettings' value with any optional fields omitted.
mkGetAccountSettings ::
  GetAccountSettings
mkGetAccountSettings = GetAccountSettings'

instance Core.FromJSON GetAccountSettings where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetAccountSettings where
  type Rs GetAccountSettings = GetAccountSettingsResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.GetAccountSettings")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountSettingsResponse'
            Core.<$> (x Core..:? "accountSettings")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the account settings return values from the @GetAccountSettings@ request.
--
-- /See:/ 'mkGetAccountSettingsResponse' smart constructor.
data GetAccountSettingsResponse = GetAccountSettingsResponse'
  { -- | The account settings.
    accountSettings :: Core.Maybe Types.AccountSettings,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccountSettingsResponse' value with any optional fields omitted.
mkGetAccountSettingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAccountSettingsResponse
mkGetAccountSettingsResponse responseStatus =
  GetAccountSettingsResponse'
    { accountSettings = Core.Nothing,
      responseStatus
    }

-- | The account settings.
--
-- /Note:/ Consider using 'accountSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsAccountSettings :: Lens.Lens' GetAccountSettingsResponse (Core.Maybe Types.AccountSettings)
gasrrsAccountSettings = Lens.field @"accountSettings"
{-# DEPRECATED gasrrsAccountSettings "Use generic-lens or generic-optics with 'accountSettings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsResponseStatus :: Lens.Lens' GetAccountSettingsResponse Core.Int
gasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
