{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.UpdateEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings of an existing AWS Cloud9 development environment.
module Network.AWS.Cloud9.UpdateEnvironment
  ( -- * Creating a request
    UpdateEnvironment (..),
    mkUpdateEnvironment,

    -- ** Request lenses
    ueEnvironmentId,
    ueDescription,
    ueName,

    -- * Destructuring the response
    UpdateEnvironmentResponse (..),
    mkUpdateEnvironmentResponse,

    -- ** Response lenses
    uerrsResponseStatus,
  )
where

import qualified Network.AWS.Cloud9.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateEnvironment' smart constructor.
data UpdateEnvironment = UpdateEnvironment'
  { -- | The ID of the environment to change settings.
    environmentId :: Types.EnvironmentId,
    -- | Any new or replacement description for the environment.
    description :: Core.Maybe Types.Description,
    -- | A replacement name for the environment.
    name :: Core.Maybe Types.EnvironmentName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEnvironment' value with any optional fields omitted.
mkUpdateEnvironment ::
  -- | 'environmentId'
  Types.EnvironmentId ->
  UpdateEnvironment
mkUpdateEnvironment environmentId =
  UpdateEnvironment'
    { environmentId,
      description = Core.Nothing,
      name = Core.Nothing
    }

-- | The ID of the environment to change settings.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEnvironmentId :: Lens.Lens' UpdateEnvironment Types.EnvironmentId
ueEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED ueEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | Any new or replacement description for the environment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueDescription :: Lens.Lens' UpdateEnvironment (Core.Maybe Types.Description)
ueDescription = Lens.field @"description"
{-# DEPRECATED ueDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A replacement name for the environment.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueName :: Lens.Lens' UpdateEnvironment (Core.Maybe Types.EnvironmentName)
ueName = Lens.field @"name"
{-# DEPRECATED ueName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateEnvironment where
  toJSON UpdateEnvironment {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("environmentId" Core..= environmentId),
            ("description" Core..=) Core.<$> description,
            ("name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest UpdateEnvironment where
  type Rs UpdateEnvironment = UpdateEnvironmentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCloud9WorkspaceManagementService.UpdateEnvironment"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEnvironmentResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateEnvironmentResponse' smart constructor.
newtype UpdateEnvironmentResponse = UpdateEnvironmentResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEnvironmentResponse' value with any optional fields omitted.
mkUpdateEnvironmentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateEnvironmentResponse
mkUpdateEnvironmentResponse responseStatus =
  UpdateEnvironmentResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uerrsResponseStatus :: Lens.Lens' UpdateEnvironmentResponse Core.Int
uerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
