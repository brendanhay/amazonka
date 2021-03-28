{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.WAFRegional.GetChangeTokenStatus
    (
    -- * Creating a request
      GetChangeTokenStatus (..)
    , mkGetChangeTokenStatus
    -- ** Request lenses
    , gctsChangeToken

    -- * Destructuring the response
    , GetChangeTokenStatusResponse (..)
    , mkGetChangeTokenStatusResponse
    -- ** Response lenses
    , gctsrrsChangeTokenStatus
    , gctsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkGetChangeTokenStatus' smart constructor.
newtype GetChangeTokenStatus = GetChangeTokenStatus'
  { changeToken :: Types.ChangeToken
    -- ^ The change token for which you want to get the status. This change token was previously returned in the @GetChangeToken@ response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetChangeTokenStatus' value with any optional fields omitted.
mkGetChangeTokenStatus
    :: Types.ChangeToken -- ^ 'changeToken'
    -> GetChangeTokenStatus
mkGetChangeTokenStatus changeToken
  = GetChangeTokenStatus'{changeToken}

-- | The change token for which you want to get the status. This change token was previously returned in the @GetChangeToken@ response.
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gctsChangeToken :: Lens.Lens' GetChangeTokenStatus Types.ChangeToken
gctsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE gctsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery GetChangeTokenStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetChangeTokenStatus where
        toHeaders GetChangeTokenStatus{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.GetChangeTokenStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetChangeTokenStatus where
        toJSON GetChangeTokenStatus{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest GetChangeTokenStatus where
        type Rs GetChangeTokenStatus = GetChangeTokenStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetChangeTokenStatusResponse' Core.<$>
                   (x Core..:? "ChangeTokenStatus") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetChangeTokenStatusResponse' smart constructor.
data GetChangeTokenStatusResponse = GetChangeTokenStatusResponse'
  { changeTokenStatus :: Core.Maybe Types.ChangeTokenStatus
    -- ^ The status of the change token.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetChangeTokenStatusResponse' value with any optional fields omitted.
mkGetChangeTokenStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetChangeTokenStatusResponse
mkGetChangeTokenStatusResponse responseStatus
  = GetChangeTokenStatusResponse'{changeTokenStatus = Core.Nothing,
                                  responseStatus}

-- | The status of the change token.
--
-- /Note:/ Consider using 'changeTokenStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gctsrrsChangeTokenStatus :: Lens.Lens' GetChangeTokenStatusResponse (Core.Maybe Types.ChangeTokenStatus)
gctsrrsChangeTokenStatus = Lens.field @"changeTokenStatus"
{-# INLINEABLE gctsrrsChangeTokenStatus #-}
{-# DEPRECATED changeTokenStatus "Use generic-lens or generic-optics with 'changeTokenStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gctsrrsResponseStatus :: Lens.Lens' GetChangeTokenStatusResponse Core.Int
gctsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gctsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
