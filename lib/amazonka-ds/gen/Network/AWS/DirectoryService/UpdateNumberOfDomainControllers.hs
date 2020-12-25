{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateNumberOfDomainControllers (..),
    mkUpdateNumberOfDomainControllers,

    -- ** Request lenses
    unodcDirectoryId,
    unodcDesiredNumber,

    -- * Destructuring the response
    UpdateNumberOfDomainControllersResponse (..),
    mkUpdateNumberOfDomainControllersResponse,

    -- ** Response lenses
    unodcrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateNumberOfDomainControllers' smart constructor.
data UpdateNumberOfDomainControllers = UpdateNumberOfDomainControllers'
  { -- | Identifier of the directory to which the domain controllers will be added or removed.
    directoryId :: Types.DirectoryId,
    -- | The number of domain controllers desired in the directory.
    desiredNumber :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNumberOfDomainControllers' value with any optional fields omitted.
mkUpdateNumberOfDomainControllers ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'desiredNumber'
  Core.Natural ->
  UpdateNumberOfDomainControllers
mkUpdateNumberOfDomainControllers directoryId desiredNumber =
  UpdateNumberOfDomainControllers' {directoryId, desiredNumber}

-- | Identifier of the directory to which the domain controllers will be added or removed.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unodcDirectoryId :: Lens.Lens' UpdateNumberOfDomainControllers Types.DirectoryId
unodcDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED unodcDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The number of domain controllers desired in the directory.
--
-- /Note:/ Consider using 'desiredNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unodcDesiredNumber :: Lens.Lens' UpdateNumberOfDomainControllers Core.Natural
unodcDesiredNumber = Lens.field @"desiredNumber"
{-# DEPRECATED unodcDesiredNumber "Use generic-lens or generic-optics with 'desiredNumber' instead." #-}

instance Core.FromJSON UpdateNumberOfDomainControllers where
  toJSON UpdateNumberOfDomainControllers {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("DesiredNumber" Core..= desiredNumber)
          ]
      )

instance Core.AWSRequest UpdateNumberOfDomainControllers where
  type
    Rs UpdateNumberOfDomainControllers =
      UpdateNumberOfDomainControllersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "DirectoryService_20150416.UpdateNumberOfDomainControllers"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNumberOfDomainControllersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateNumberOfDomainControllersResponse' smart constructor.
newtype UpdateNumberOfDomainControllersResponse = UpdateNumberOfDomainControllersResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNumberOfDomainControllersResponse' value with any optional fields omitted.
mkUpdateNumberOfDomainControllersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateNumberOfDomainControllersResponse
mkUpdateNumberOfDomainControllersResponse responseStatus =
  UpdateNumberOfDomainControllersResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unodcrrsResponseStatus :: Lens.Lens' UpdateNumberOfDomainControllersResponse Core.Int
unodcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED unodcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
