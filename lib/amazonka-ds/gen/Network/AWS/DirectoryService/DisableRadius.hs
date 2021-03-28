{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DisableRadius
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables multi-factor authentication (MFA) with the Remote Authentication Dial In User Service (RADIUS) server for an AD Connector or Microsoft AD directory.
module Network.AWS.DirectoryService.DisableRadius
    (
    -- * Creating a request
      DisableRadius (..)
    , mkDisableRadius
    -- ** Request lenses
    , drDirectoryId

    -- * Destructuring the response
    , DisableRadiusResponse (..)
    , mkDisableRadiusResponse
    -- ** Response lenses
    , drrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'DisableRadius' operation.
--
-- /See:/ 'mkDisableRadius' smart constructor.
newtype DisableRadius = DisableRadius'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory for which to disable MFA.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableRadius' value with any optional fields omitted.
mkDisableRadius
    :: Types.DirectoryId -- ^ 'directoryId'
    -> DisableRadius
mkDisableRadius directoryId = DisableRadius'{directoryId}

-- | The identifier of the directory for which to disable MFA.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drDirectoryId :: Lens.Lens' DisableRadius Types.DirectoryId
drDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE drDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

instance Core.ToQuery DisableRadius where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisableRadius where
        toHeaders DisableRadius{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.DisableRadius")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisableRadius where
        toJSON DisableRadius{..}
          = Core.object
              (Core.catMaybes [Core.Just ("DirectoryId" Core..= directoryId)])

instance Core.AWSRequest DisableRadius where
        type Rs DisableRadius = DisableRadiusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisableRadiusResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the results of the 'DisableRadius' operation.
--
-- /See:/ 'mkDisableRadiusResponse' smart constructor.
newtype DisableRadiusResponse = DisableRadiusResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableRadiusResponse' value with any optional fields omitted.
mkDisableRadiusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisableRadiusResponse
mkDisableRadiusResponse responseStatus
  = DisableRadiusResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DisableRadiusResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
