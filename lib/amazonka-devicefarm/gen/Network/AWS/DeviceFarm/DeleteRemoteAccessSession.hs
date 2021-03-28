{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteRemoteAccessSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a completed remote access session and its results.
module Network.AWS.DeviceFarm.DeleteRemoteAccessSession
    (
    -- * Creating a request
      DeleteRemoteAccessSession (..)
    , mkDeleteRemoteAccessSession
    -- ** Request lenses
    , drasArn

    -- * Destructuring the response
    , DeleteRemoteAccessSessionResponse (..)
    , mkDeleteRemoteAccessSessionResponse
    -- ** Response lenses
    , drasrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to delete the specified remote access session.
--
-- /See:/ 'mkDeleteRemoteAccessSession' smart constructor.
newtype DeleteRemoteAccessSession = DeleteRemoteAccessSession'
  { arn :: Types.AmazonResourceName
    -- ^ The Amazon Resource Name (ARN) of the session for which you want to delete remote access.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRemoteAccessSession' value with any optional fields omitted.
mkDeleteRemoteAccessSession
    :: Types.AmazonResourceName -- ^ 'arn'
    -> DeleteRemoteAccessSession
mkDeleteRemoteAccessSession arn = DeleteRemoteAccessSession'{arn}

-- | The Amazon Resource Name (ARN) of the session for which you want to delete remote access.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drasArn :: Lens.Lens' DeleteRemoteAccessSession Types.AmazonResourceName
drasArn = Lens.field @"arn"
{-# INLINEABLE drasArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery DeleteRemoteAccessSession where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteRemoteAccessSession where
        toHeaders DeleteRemoteAccessSession{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.DeleteRemoteAccessSession")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteRemoteAccessSession where
        toJSON DeleteRemoteAccessSession{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest DeleteRemoteAccessSession where
        type Rs DeleteRemoteAccessSession =
             DeleteRemoteAccessSessionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteRemoteAccessSessionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The response from the server when a request is made to delete the remote access session.
--
-- /See:/ 'mkDeleteRemoteAccessSessionResponse' smart constructor.
newtype DeleteRemoteAccessSessionResponse = DeleteRemoteAccessSessionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRemoteAccessSessionResponse' value with any optional fields omitted.
mkDeleteRemoteAccessSessionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteRemoteAccessSessionResponse
mkDeleteRemoteAccessSessionResponse responseStatus
  = DeleteRemoteAccessSessionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drasrrsResponseStatus :: Lens.Lens' DeleteRemoteAccessSessionResponse Core.Int
drasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
