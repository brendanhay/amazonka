{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.GetFederationToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a token for federation.
module Network.AWS.Connect.GetFederationToken
    (
    -- * Creating a request
      GetFederationToken (..)
    , mkGetFederationToken
    -- ** Request lenses
    , gftInstanceId

    -- * Destructuring the response
    , GetFederationTokenResponse (..)
    , mkGetFederationTokenResponse
    -- ** Response lenses
    , gftrrsCredentials
    , gftrrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFederationToken' smart constructor.
newtype GetFederationToken = GetFederationToken'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetFederationToken' value with any optional fields omitted.
mkGetFederationToken
    :: Types.InstanceId -- ^ 'instanceId'
    -> GetFederationToken
mkGetFederationToken instanceId = GetFederationToken'{instanceId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftInstanceId :: Lens.Lens' GetFederationToken Types.InstanceId
gftInstanceId = Lens.field @"instanceId"
{-# INLINEABLE gftInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery GetFederationToken where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetFederationToken where
        toHeaders GetFederationToken{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetFederationToken where
        type Rs GetFederationToken = GetFederationTokenResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/user/federate/" Core.<> Core.toText instanceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetFederationTokenResponse' Core.<$>
                   (x Core..:? "Credentials") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetFederationTokenResponse' smart constructor.
data GetFederationTokenResponse = GetFederationTokenResponse'
  { credentials :: Core.Maybe Types.Credentials
    -- ^ The credentials to use for federation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetFederationTokenResponse' value with any optional fields omitted.
mkGetFederationTokenResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetFederationTokenResponse
mkGetFederationTokenResponse responseStatus
  = GetFederationTokenResponse'{credentials = Core.Nothing,
                                responseStatus}

-- | The credentials to use for federation.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftrrsCredentials :: Lens.Lens' GetFederationTokenResponse (Core.Maybe Types.Credentials)
gftrrsCredentials = Lens.field @"credentials"
{-# INLINEABLE gftrrsCredentials #-}
{-# DEPRECATED credentials "Use generic-lens or generic-optics with 'credentials' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gftrrsResponseStatus :: Lens.Lens' GetFederationTokenResponse Core.Int
gftrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gftrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
