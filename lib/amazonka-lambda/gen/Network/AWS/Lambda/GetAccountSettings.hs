{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetAccountSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about your account's <https://docs.aws.amazon.com/lambda/latest/dg/limits.html limits> and usage in an AWS Region.
module Network.AWS.Lambda.GetAccountSettings
    (
    -- * Creating a request
      GetAccountSettings (..)
    , mkGetAccountSettings

    -- * Destructuring the response
    , GetAccountSettingsResponse (..)
    , mkGetAccountSettingsResponse
    -- ** Response lenses
    , gasrrsAccountLimit
    , gasrrsAccountUsage
    , gasrrsResponseStatus
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAccountSettings' smart constructor.
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
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetAccountSettings where
        type Rs GetAccountSettings = GetAccountSettingsResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2016-08-19/account-settings/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAccountSettingsResponse' Core.<$>
                   (x Core..:? "AccountLimit") Core.<*> x Core..:? "AccountUsage"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAccountSettingsResponse' smart constructor.
data GetAccountSettingsResponse = GetAccountSettingsResponse'
  { accountLimit :: Core.Maybe Types.AccountLimit
    -- ^ Limits that are related to concurrency and code storage.
  , accountUsage :: Core.Maybe Types.AccountUsage
    -- ^ The number of functions and amount of storage in use.
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
  = GetAccountSettingsResponse'{accountLimit = Core.Nothing,
                                accountUsage = Core.Nothing, responseStatus}

-- | Limits that are related to concurrency and code storage.
--
-- /Note:/ Consider using 'accountLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsAccountLimit :: Lens.Lens' GetAccountSettingsResponse (Core.Maybe Types.AccountLimit)
gasrrsAccountLimit = Lens.field @"accountLimit"
{-# INLINEABLE gasrrsAccountLimit #-}
{-# DEPRECATED accountLimit "Use generic-lens or generic-optics with 'accountLimit' instead"  #-}

-- | The number of functions and amount of storage in use.
--
-- /Note:/ Consider using 'accountUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsAccountUsage :: Lens.Lens' GetAccountSettingsResponse (Core.Maybe Types.AccountUsage)
gasrrsAccountUsage = Lens.field @"accountUsage"
{-# INLINEABLE gasrrsAccountUsage #-}
{-# DEPRECATED accountUsage "Use generic-lens or generic-optics with 'accountUsage' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsResponseStatus :: Lens.Lens' GetAccountSettingsResponse Core.Int
gasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
