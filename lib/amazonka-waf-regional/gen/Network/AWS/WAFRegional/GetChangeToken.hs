{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetChangeToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When you want to create, update, or delete AWS WAF objects, get a change token and include the change token in the create, update, or delete request. Change tokens ensure that your application doesn't submit conflicting requests to AWS WAF.
--
-- Each create, update, or delete request must use a unique change token. If your application submits a @GetChangeToken@ request and then submits a second @GetChangeToken@ request before submitting a create, update, or delete request, the second @GetChangeToken@ request returns the same value as the first @GetChangeToken@ request.
-- When you use a change token in a create, update, or delete request, the status of the change token changes to @PENDING@ , which indicates that AWS WAF is propagating the change to all AWS WAF servers. Use @GetChangeTokenStatus@ to determine the status of your change token.
module Network.AWS.WAFRegional.GetChangeToken
    (
    -- * Creating a request
      GetChangeToken (..)
    , mkGetChangeToken

    -- * Destructuring the response
    , GetChangeTokenResponse (..)
    , mkGetChangeTokenResponse
    -- ** Response lenses
    , gctrrsChangeToken
    , gctrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkGetChangeToken' smart constructor.
data GetChangeToken = GetChangeToken'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetChangeToken' value with any optional fields omitted.
mkGetChangeToken
    :: GetChangeToken
mkGetChangeToken = GetChangeToken'

instance Core.ToQuery GetChangeToken where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetChangeToken where
        toHeaders GetChangeToken{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.GetChangeToken")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetChangeToken where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetChangeToken where
        type Rs GetChangeToken = GetChangeTokenResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetChangeTokenResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetChangeTokenResponse' smart constructor.
data GetChangeTokenResponse = GetChangeTokenResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used in the request. Use this value in a @GetChangeTokenStatus@ request to get the current status of the request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetChangeTokenResponse' value with any optional fields omitted.
mkGetChangeTokenResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetChangeTokenResponse
mkGetChangeTokenResponse responseStatus
  = GetChangeTokenResponse'{changeToken = Core.Nothing,
                            responseStatus}

-- | The @ChangeToken@ that you used in the request. Use this value in a @GetChangeTokenStatus@ request to get the current status of the request. 
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gctrrsChangeToken :: Lens.Lens' GetChangeTokenResponse (Core.Maybe Types.ChangeToken)
gctrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE gctrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gctrrsResponseStatus :: Lens.Lens' GetChangeTokenResponse Core.Int
gctrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gctrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
