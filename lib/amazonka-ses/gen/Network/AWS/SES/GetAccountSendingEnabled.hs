{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetAccountSendingEnabled (..)
    , mkGetAccountSendingEnabled

    -- * Destructuring the response
    , GetAccountSendingEnabledResponse (..)
    , mkGetAccountSendingEnabledResponse
    -- ** Response lenses
    , gaserrsEnabled
    , gaserrsResponseStatus
    ) where

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
mkGetAccountSendingEnabled
    :: GetAccountSendingEnabled
mkGetAccountSendingEnabled = GetAccountSendingEnabled'

instance Core.ToQuery GetAccountSendingEnabled where
        toQuery GetAccountSendingEnabled{..}
          = Core.toQueryPair "Action"
              ("GetAccountSendingEnabled" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)

instance Core.ToHeaders GetAccountSendingEnabled where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetAccountSendingEnabled where
        type Rs GetAccountSendingEnabled = GetAccountSendingEnabledResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "GetAccountSendingEnabledResult"
              (\ s h x ->
                 GetAccountSendingEnabledResponse' Core.<$>
                   (x Core..@? "Enabled") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents a request to return the email sending status for your Amazon SES account in the current AWS Region.
--
-- /See:/ 'mkGetAccountSendingEnabledResponse' smart constructor.
data GetAccountSendingEnabledResponse = GetAccountSendingEnabledResponse'
  { enabled :: Core.Maybe Core.Bool
    -- ^ Describes whether email sending is enabled or disabled for your Amazon SES account in the current AWS Region.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccountSendingEnabledResponse' value with any optional fields omitted.
mkGetAccountSendingEnabledResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAccountSendingEnabledResponse
mkGetAccountSendingEnabledResponse responseStatus
  = GetAccountSendingEnabledResponse'{enabled = Core.Nothing,
                                      responseStatus}

-- | Describes whether email sending is enabled or disabled for your Amazon SES account in the current AWS Region.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaserrsEnabled :: Lens.Lens' GetAccountSendingEnabledResponse (Core.Maybe Core.Bool)
gaserrsEnabled = Lens.field @"enabled"
{-# INLINEABLE gaserrsEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaserrsResponseStatus :: Lens.Lens' GetAccountSendingEnabledResponse Core.Int
gaserrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gaserrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
