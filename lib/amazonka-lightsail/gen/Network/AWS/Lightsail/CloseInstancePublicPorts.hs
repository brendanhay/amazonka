{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CloseInstancePublicPorts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Closes ports for a specific Amazon Lightsail instance.
--
-- The @CloseInstancePublicPorts@ action supports tag-based access control via resource tags applied to the resource identified by @instanceName@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CloseInstancePublicPorts
  ( -- * Creating a request
    CloseInstancePublicPorts (..),
    mkCloseInstancePublicPorts,

    -- ** Request lenses
    cippPortInfo,
    cippInstanceName,

    -- * Destructuring the response
    CloseInstancePublicPortsResponse (..),
    mkCloseInstancePublicPortsResponse,

    -- ** Response lenses
    cipprrsOperation,
    cipprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCloseInstancePublicPorts' smart constructor.
data CloseInstancePublicPorts = CloseInstancePublicPorts'
  { -- | An object to describe the ports to close for the specified instance.
    portInfo :: Types.PortInfo,
    -- | The name of the instance for which to close ports.
    instanceName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloseInstancePublicPorts' value with any optional fields omitted.
mkCloseInstancePublicPorts ::
  -- | 'portInfo'
  Types.PortInfo ->
  -- | 'instanceName'
  Types.ResourceName ->
  CloseInstancePublicPorts
mkCloseInstancePublicPorts portInfo instanceName =
  CloseInstancePublicPorts' {portInfo, instanceName}

-- | An object to describe the ports to close for the specified instance.
--
-- /Note:/ Consider using 'portInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cippPortInfo :: Lens.Lens' CloseInstancePublicPorts Types.PortInfo
cippPortInfo = Lens.field @"portInfo"
{-# DEPRECATED cippPortInfo "Use generic-lens or generic-optics with 'portInfo' instead." #-}

-- | The name of the instance for which to close ports.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cippInstanceName :: Lens.Lens' CloseInstancePublicPorts Types.ResourceName
cippInstanceName = Lens.field @"instanceName"
{-# DEPRECATED cippInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Core.FromJSON CloseInstancePublicPorts where
  toJSON CloseInstancePublicPorts {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("portInfo" Core..= portInfo),
            Core.Just ("instanceName" Core..= instanceName)
          ]
      )

instance Core.AWSRequest CloseInstancePublicPorts where
  type Rs CloseInstancePublicPorts = CloseInstancePublicPortsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.CloseInstancePublicPorts")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CloseInstancePublicPortsResponse'
            Core.<$> (x Core..:? "operation") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCloseInstancePublicPortsResponse' smart constructor.
data CloseInstancePublicPortsResponse = CloseInstancePublicPortsResponse'
  { -- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Core.Maybe Types.Operation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CloseInstancePublicPortsResponse' value with any optional fields omitted.
mkCloseInstancePublicPortsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CloseInstancePublicPortsResponse
mkCloseInstancePublicPortsResponse responseStatus =
  CloseInstancePublicPortsResponse'
    { operation = Core.Nothing,
      responseStatus
    }

-- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipprrsOperation :: Lens.Lens' CloseInstancePublicPortsResponse (Core.Maybe Types.Operation)
cipprrsOperation = Lens.field @"operation"
{-# DEPRECATED cipprrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipprrsResponseStatus :: Lens.Lens' CloseInstancePublicPortsResponse Core.Int
cipprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cipprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
