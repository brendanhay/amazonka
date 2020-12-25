{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.UpdateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the name of an application.
module Network.AWS.CodeDeploy.UpdateApplication
  ( -- * Creating a request
    UpdateApplication (..),
    mkUpdateApplication,

    -- ** Request lenses
    uaApplicationName,
    uaNewApplicationName,

    -- * Destructuring the response
    UpdateApplicationResponse (..),
    mkUpdateApplicationResponse,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an @UpdateApplication@ operation.
--
-- /See:/ 'mkUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | The current name of the application you want to change.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | The new name to give the application.
    newApplicationName :: Core.Maybe Types.NewApplicationName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplication' value with any optional fields omitted.
mkUpdateApplication ::
  UpdateApplication
mkUpdateApplication =
  UpdateApplication'
    { applicationName = Core.Nothing,
      newApplicationName = Core.Nothing
    }

-- | The current name of the application you want to change.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaApplicationName :: Lens.Lens' UpdateApplication (Core.Maybe Types.ApplicationName)
uaApplicationName = Lens.field @"applicationName"
{-# DEPRECATED uaApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The new name to give the application.
--
-- /Note:/ Consider using 'newApplicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaNewApplicationName :: Lens.Lens' UpdateApplication (Core.Maybe Types.NewApplicationName)
uaNewApplicationName = Lens.field @"newApplicationName"
{-# DEPRECATED uaNewApplicationName "Use generic-lens or generic-optics with 'newApplicationName' instead." #-}

instance Core.FromJSON UpdateApplication where
  toJSON UpdateApplication {..} =
    Core.object
      ( Core.catMaybes
          [ ("applicationName" Core..=) Core.<$> applicationName,
            ("newApplicationName" Core..=) Core.<$> newApplicationName
          ]
      )

instance Core.AWSRequest UpdateApplication where
  type Rs UpdateApplication = UpdateApplicationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeDeploy_20141006.UpdateApplication")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateApplicationResponse'

-- | /See:/ 'mkUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplicationResponse' value with any optional fields omitted.
mkUpdateApplicationResponse ::
  UpdateApplicationResponse
mkUpdateApplicationResponse = UpdateApplicationResponse'
