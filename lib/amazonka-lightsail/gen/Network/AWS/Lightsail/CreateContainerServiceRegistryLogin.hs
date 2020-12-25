{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateContainerServiceRegistryLogin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a temporary set of log in credentials that you can use to log in to the Docker process on your local machine. After you're logged in, you can use the native Docker commands to push your local container images to the container image registry of your Amazon Lightsail account so that you can use them with your Lightsail container service. The log in credentials expire 12 hours after they are created, at which point you will need to create a new set of log in credentials.
--
-- After you push your container images to the container image registry of your Lightsail account, use the @RegisterContainerImage@ action to register the pushed images to a specific Lightsail container service.
module Network.AWS.Lightsail.CreateContainerServiceRegistryLogin
  ( -- * Creating a request
    CreateContainerServiceRegistryLogin (..),
    mkCreateContainerServiceRegistryLogin,

    -- * Destructuring the response
    CreateContainerServiceRegistryLoginResponse (..),
    mkCreateContainerServiceRegistryLoginResponse,

    -- ** Response lenses
    ccsrlrrsRegistryLogin,
    ccsrlrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateContainerServiceRegistryLogin' smart constructor.
data CreateContainerServiceRegistryLogin = CreateContainerServiceRegistryLogin'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateContainerServiceRegistryLogin' value with any optional fields omitted.
mkCreateContainerServiceRegistryLogin ::
  CreateContainerServiceRegistryLogin
mkCreateContainerServiceRegistryLogin =
  CreateContainerServiceRegistryLogin'

instance Core.FromJSON CreateContainerServiceRegistryLogin where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest CreateContainerServiceRegistryLogin where
  type
    Rs CreateContainerServiceRegistryLogin =
      CreateContainerServiceRegistryLoginResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Lightsail_20161128.CreateContainerServiceRegistryLogin"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContainerServiceRegistryLoginResponse'
            Core.<$> (x Core..:? "registryLogin")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateContainerServiceRegistryLoginResponse' smart constructor.
data CreateContainerServiceRegistryLoginResponse = CreateContainerServiceRegistryLoginResponse'
  { -- | An object that describes the log in information for the container service registry of your Lightsail account.
    registryLogin :: Core.Maybe Types.ContainerServiceRegistryLogin,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateContainerServiceRegistryLoginResponse' value with any optional fields omitted.
mkCreateContainerServiceRegistryLoginResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateContainerServiceRegistryLoginResponse
mkCreateContainerServiceRegistryLoginResponse responseStatus =
  CreateContainerServiceRegistryLoginResponse'
    { registryLogin =
        Core.Nothing,
      responseStatus
    }

-- | An object that describes the log in information for the container service registry of your Lightsail account.
--
-- /Note:/ Consider using 'registryLogin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrlrrsRegistryLogin :: Lens.Lens' CreateContainerServiceRegistryLoginResponse (Core.Maybe Types.ContainerServiceRegistryLogin)
ccsrlrrsRegistryLogin = Lens.field @"registryLogin"
{-# DEPRECATED ccsrlrrsRegistryLogin "Use generic-lens or generic-optics with 'registryLogin' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrlrrsResponseStatus :: Lens.Lens' CreateContainerServiceRegistryLoginResponse Core.Int
ccsrlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccsrlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
