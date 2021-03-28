{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.UpdateNumberOfDomainControllers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or removes domain controllers to or from the directory. Based on the difference between current value and new value (provided through this API call), domain controllers will be added or removed. It may take up to 45 minutes for any new domain controllers to become fully active once the requested number of domain controllers is updated. During this time, you cannot make another update request.
module Network.AWS.DirectoryService.UpdateNumberOfDomainControllers
    (
    -- * Creating a request
      UpdateNumberOfDomainControllers (..)
    , mkUpdateNumberOfDomainControllers
    -- ** Request lenses
    , unodcDirectoryId
    , unodcDesiredNumber

    -- * Destructuring the response
    , UpdateNumberOfDomainControllersResponse (..)
    , mkUpdateNumberOfDomainControllersResponse
    -- ** Response lenses
    , unodcrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateNumberOfDomainControllers' smart constructor.
data UpdateNumberOfDomainControllers = UpdateNumberOfDomainControllers'
  { directoryId :: Types.DirectoryId
    -- ^ Identifier of the directory to which the domain controllers will be added or removed.
  , desiredNumber :: Core.Natural
    -- ^ The number of domain controllers desired in the directory.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNumberOfDomainControllers' value with any optional fields omitted.
mkUpdateNumberOfDomainControllers
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Core.Natural -- ^ 'desiredNumber'
    -> UpdateNumberOfDomainControllers
mkUpdateNumberOfDomainControllers directoryId desiredNumber
  = UpdateNumberOfDomainControllers'{directoryId, desiredNumber}

-- | Identifier of the directory to which the domain controllers will be added or removed.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unodcDirectoryId :: Lens.Lens' UpdateNumberOfDomainControllers Types.DirectoryId
unodcDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE unodcDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The number of domain controllers desired in the directory.
--
-- /Note:/ Consider using 'desiredNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unodcDesiredNumber :: Lens.Lens' UpdateNumberOfDomainControllers Core.Natural
unodcDesiredNumber = Lens.field @"desiredNumber"
{-# INLINEABLE unodcDesiredNumber #-}
{-# DEPRECATED desiredNumber "Use generic-lens or generic-optics with 'desiredNumber' instead"  #-}

instance Core.ToQuery UpdateNumberOfDomainControllers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateNumberOfDomainControllers where
        toHeaders UpdateNumberOfDomainControllers{..}
          = Core.pure
              ("X-Amz-Target",
               "DirectoryService_20150416.UpdateNumberOfDomainControllers")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateNumberOfDomainControllers where
        toJSON UpdateNumberOfDomainControllers{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("DesiredNumber" Core..= desiredNumber)])

instance Core.AWSRequest UpdateNumberOfDomainControllers where
        type Rs UpdateNumberOfDomainControllers =
             UpdateNumberOfDomainControllersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateNumberOfDomainControllersResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateNumberOfDomainControllersResponse' smart constructor.
newtype UpdateNumberOfDomainControllersResponse = UpdateNumberOfDomainControllersResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNumberOfDomainControllersResponse' value with any optional fields omitted.
mkUpdateNumberOfDomainControllersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateNumberOfDomainControllersResponse
mkUpdateNumberOfDomainControllersResponse responseStatus
  = UpdateNumberOfDomainControllersResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unodcrrsResponseStatus :: Lens.Lens' UpdateNumberOfDomainControllersResponse Core.Int
unodcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE unodcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
