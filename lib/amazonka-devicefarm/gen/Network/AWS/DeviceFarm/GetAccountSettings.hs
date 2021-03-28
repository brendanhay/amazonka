{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetAccountSettings (..)
    , mkGetAccountSettings

    -- * Destructuring the response
    , GetAccountSettingsResponse (..)
    , mkGetAccountSettingsResponse
    -- ** Response lenses
    , gasrrsAccountSettings
    , gasrrsResponseStatus
    ) where

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
mkGetAccountSettings
    :: GetAccountSettings
mkGetAccountSettings = GetAccountSettings'

instance Core.ToQuery GetAccountSettings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAccountSettings where
        toHeaders GetAccountSettings{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.GetAccountSettings")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAccountSettings where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetAccountSettings where
        type Rs GetAccountSettings = GetAccountSettingsResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAccountSettingsResponse' Core.<$>
                   (x Core..:? "accountSettings") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the account settings return values from the @GetAccountSettings@ request.
--
-- /See:/ 'mkGetAccountSettingsResponse' smart constructor.
data GetAccountSettingsResponse = GetAccountSettingsResponse'
  { accountSettings :: Core.Maybe Types.AccountSettings
    -- ^ The account settings.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccountSettingsResponse' value with any optional fields omitted.
mkGetAccountSettingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAccountSettingsResponse
mkGetAccountSettingsResponse responseStatus
  = GetAccountSettingsResponse'{accountSettings = Core.Nothing,
                                responseStatus}

-- | The account settings.
--
-- /Note:/ Consider using 'accountSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsAccountSettings :: Lens.Lens' GetAccountSettingsResponse (Core.Maybe Types.AccountSettings)
gasrrsAccountSettings = Lens.field @"accountSettings"
{-# INLINEABLE gasrrsAccountSettings #-}
{-# DEPRECATED accountSettings "Use generic-lens or generic-optics with 'accountSettings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsResponseStatus :: Lens.Lens' GetAccountSettingsResponse Core.Int
gasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
