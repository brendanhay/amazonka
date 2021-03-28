{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.EnableRadius
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables multi-factor authentication (MFA) with the Remote Authentication Dial In User Service (RADIUS) server for an AD Connector or Microsoft AD directory.
module Network.AWS.DirectoryService.EnableRadius
    (
    -- * Creating a request
      EnableRadius (..)
    , mkEnableRadius
    -- ** Request lenses
    , erDirectoryId
    , erRadiusSettings

    -- * Destructuring the response
    , EnableRadiusResponse (..)
    , mkEnableRadiusResponse
    -- ** Response lenses
    , errrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'EnableRadius' operation.
--
-- /See:/ 'mkEnableRadius' smart constructor.
data EnableRadius = EnableRadius'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory for which to enable MFA.
  , radiusSettings :: Types.RadiusSettings
    -- ^ A 'RadiusSettings' object that contains information about the RADIUS server.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableRadius' value with any optional fields omitted.
mkEnableRadius
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Types.RadiusSettings -- ^ 'radiusSettings'
    -> EnableRadius
mkEnableRadius directoryId radiusSettings
  = EnableRadius'{directoryId, radiusSettings}

-- | The identifier of the directory for which to enable MFA.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erDirectoryId :: Lens.Lens' EnableRadius Types.DirectoryId
erDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE erDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | A 'RadiusSettings' object that contains information about the RADIUS server.
--
-- /Note:/ Consider using 'radiusSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erRadiusSettings :: Lens.Lens' EnableRadius Types.RadiusSettings
erRadiusSettings = Lens.field @"radiusSettings"
{-# INLINEABLE erRadiusSettings #-}
{-# DEPRECATED radiusSettings "Use generic-lens or generic-optics with 'radiusSettings' instead"  #-}

instance Core.ToQuery EnableRadius where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders EnableRadius where
        toHeaders EnableRadius{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.EnableRadius")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON EnableRadius where
        toJSON EnableRadius{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("RadiusSettings" Core..= radiusSettings)])

instance Core.AWSRequest EnableRadius where
        type Rs EnableRadius = EnableRadiusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 EnableRadiusResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the results of the 'EnableRadius' operation.
--
-- /See:/ 'mkEnableRadiusResponse' smart constructor.
newtype EnableRadiusResponse = EnableRadiusResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableRadiusResponse' value with any optional fields omitted.
mkEnableRadiusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EnableRadiusResponse
mkEnableRadiusResponse responseStatus
  = EnableRadiusResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
errrsResponseStatus :: Lens.Lens' EnableRadiusResponse Core.Int
errrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE errrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
