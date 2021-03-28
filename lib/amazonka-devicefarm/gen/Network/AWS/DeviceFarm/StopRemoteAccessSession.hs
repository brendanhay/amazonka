{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.StopRemoteAccessSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Ends a specified remote access session.
module Network.AWS.DeviceFarm.StopRemoteAccessSession
    (
    -- * Creating a request
      StopRemoteAccessSession (..)
    , mkStopRemoteAccessSession
    -- ** Request lenses
    , srasArn

    -- * Destructuring the response
    , StopRemoteAccessSessionResponse (..)
    , mkStopRemoteAccessSessionResponse
    -- ** Response lenses
    , srasrrsRemoteAccessSession
    , srasrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to stop the remote access session.
--
-- /See:/ 'mkStopRemoteAccessSession' smart constructor.
newtype StopRemoteAccessSession = StopRemoteAccessSession'
  { arn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the remote access session to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopRemoteAccessSession' value with any optional fields omitted.
mkStopRemoteAccessSession
    :: Types.Arn -- ^ 'arn'
    -> StopRemoteAccessSession
mkStopRemoteAccessSession arn = StopRemoteAccessSession'{arn}

-- | The Amazon Resource Name (ARN) of the remote access session to stop.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srasArn :: Lens.Lens' StopRemoteAccessSession Types.Arn
srasArn = Lens.field @"arn"
{-# INLINEABLE srasArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery StopRemoteAccessSession where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopRemoteAccessSession where
        toHeaders StopRemoteAccessSession{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.StopRemoteAccessSession")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopRemoteAccessSession where
        toJSON StopRemoteAccessSession{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest StopRemoteAccessSession where
        type Rs StopRemoteAccessSession = StopRemoteAccessSessionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopRemoteAccessSessionResponse' Core.<$>
                   (x Core..:? "remoteAccessSession") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the response from the server that describes the remote access session when AWS Device Farm stops the session.
--
-- /See:/ 'mkStopRemoteAccessSessionResponse' smart constructor.
data StopRemoteAccessSessionResponse = StopRemoteAccessSessionResponse'
  { remoteAccessSession :: Core.Maybe Types.RemoteAccessSession
    -- ^ A container that represents the metadata from the service about the remote access session you are stopping.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StopRemoteAccessSessionResponse' value with any optional fields omitted.
mkStopRemoteAccessSessionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopRemoteAccessSessionResponse
mkStopRemoteAccessSessionResponse responseStatus
  = StopRemoteAccessSessionResponse'{remoteAccessSession =
                                       Core.Nothing,
                                     responseStatus}

-- | A container that represents the metadata from the service about the remote access session you are stopping.
--
-- /Note:/ Consider using 'remoteAccessSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srasrrsRemoteAccessSession :: Lens.Lens' StopRemoteAccessSessionResponse (Core.Maybe Types.RemoteAccessSession)
srasrrsRemoteAccessSession = Lens.field @"remoteAccessSession"
{-# INLINEABLE srasrrsRemoteAccessSession #-}
{-# DEPRECATED remoteAccessSession "Use generic-lens or generic-optics with 'remoteAccessSession' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srasrrsResponseStatus :: Lens.Lens' StopRemoteAccessSessionResponse Core.Int
srasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
