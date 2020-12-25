{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    EnableRadius (..),
    mkEnableRadius,

    -- ** Request lenses
    erDirectoryId,
    erRadiusSettings,

    -- * Destructuring the response
    EnableRadiusResponse (..),
    mkEnableRadiusResponse,

    -- ** Response lenses
    errrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'EnableRadius' operation.
--
-- /See:/ 'mkEnableRadius' smart constructor.
data EnableRadius = EnableRadius'
  { -- | The identifier of the directory for which to enable MFA.
    directoryId :: Types.DirectoryId,
    -- | A 'RadiusSettings' object that contains information about the RADIUS server.
    radiusSettings :: Types.RadiusSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableRadius' value with any optional fields omitted.
mkEnableRadius ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'radiusSettings'
  Types.RadiusSettings ->
  EnableRadius
mkEnableRadius directoryId radiusSettings =
  EnableRadius' {directoryId, radiusSettings}

-- | The identifier of the directory for which to enable MFA.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erDirectoryId :: Lens.Lens' EnableRadius Types.DirectoryId
erDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED erDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | A 'RadiusSettings' object that contains information about the RADIUS server.
--
-- /Note:/ Consider using 'radiusSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erRadiusSettings :: Lens.Lens' EnableRadius Types.RadiusSettings
erRadiusSettings = Lens.field @"radiusSettings"
{-# DEPRECATED erRadiusSettings "Use generic-lens or generic-optics with 'radiusSettings' instead." #-}

instance Core.FromJSON EnableRadius where
  toJSON EnableRadius {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("RadiusSettings" Core..= radiusSettings)
          ]
      )

instance Core.AWSRequest EnableRadius where
  type Rs EnableRadius = EnableRadiusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.EnableRadius")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableRadiusResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Contains the results of the 'EnableRadius' operation.
--
-- /See:/ 'mkEnableRadiusResponse' smart constructor.
newtype EnableRadiusResponse = EnableRadiusResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableRadiusResponse' value with any optional fields omitted.
mkEnableRadiusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  EnableRadiusResponse
mkEnableRadiusResponse responseStatus =
  EnableRadiusResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
errrsResponseStatus :: Lens.Lens' EnableRadiusResponse Core.Int
errrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED errrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
