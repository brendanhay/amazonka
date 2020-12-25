{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.OpenInstancePublicPorts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Opens ports for a specific Amazon Lightsail instance, and specifies the IP addresses allowed to connect to the instance through the ports, and the protocol.
--
-- The @OpenInstancePublicPorts@ action supports tag-based access control via resource tags applied to the resource identified by @instanceName@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.OpenInstancePublicPorts
  ( -- * Creating a request
    OpenInstancePublicPorts (..),
    mkOpenInstancePublicPorts,

    -- ** Request lenses
    oippPortInfo,
    oippInstanceName,

    -- * Destructuring the response
    OpenInstancePublicPortsResponse (..),
    mkOpenInstancePublicPortsResponse,

    -- ** Response lenses
    oipprrsOperation,
    oipprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkOpenInstancePublicPorts' smart constructor.
data OpenInstancePublicPorts = OpenInstancePublicPorts'
  { -- | An object to describe the ports to open for the specified instance.
    portInfo :: Types.PortInfo,
    -- | The name of the instance for which to open ports.
    instanceName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OpenInstancePublicPorts' value with any optional fields omitted.
mkOpenInstancePublicPorts ::
  -- | 'portInfo'
  Types.PortInfo ->
  -- | 'instanceName'
  Types.ResourceName ->
  OpenInstancePublicPorts
mkOpenInstancePublicPorts portInfo instanceName =
  OpenInstancePublicPorts' {portInfo, instanceName}

-- | An object to describe the ports to open for the specified instance.
--
-- /Note:/ Consider using 'portInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oippPortInfo :: Lens.Lens' OpenInstancePublicPorts Types.PortInfo
oippPortInfo = Lens.field @"portInfo"
{-# DEPRECATED oippPortInfo "Use generic-lens or generic-optics with 'portInfo' instead." #-}

-- | The name of the instance for which to open ports.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oippInstanceName :: Lens.Lens' OpenInstancePublicPorts Types.ResourceName
oippInstanceName = Lens.field @"instanceName"
{-# DEPRECATED oippInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Core.FromJSON OpenInstancePublicPorts where
  toJSON OpenInstancePublicPorts {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("portInfo" Core..= portInfo),
            Core.Just ("instanceName" Core..= instanceName)
          ]
      )

instance Core.AWSRequest OpenInstancePublicPorts where
  type Rs OpenInstancePublicPorts = OpenInstancePublicPortsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.OpenInstancePublicPorts")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          OpenInstancePublicPortsResponse'
            Core.<$> (x Core..:? "operation") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkOpenInstancePublicPortsResponse' smart constructor.
data OpenInstancePublicPortsResponse = OpenInstancePublicPortsResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Core.Maybe Types.Operation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'OpenInstancePublicPortsResponse' value with any optional fields omitted.
mkOpenInstancePublicPortsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  OpenInstancePublicPortsResponse
mkOpenInstancePublicPortsResponse responseStatus =
  OpenInstancePublicPortsResponse'
    { operation = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oipprrsOperation :: Lens.Lens' OpenInstancePublicPortsResponse (Core.Maybe Types.Operation)
oipprrsOperation = Lens.field @"operation"
{-# DEPRECATED oipprrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oipprrsResponseStatus :: Lens.Lens' OpenInstancePublicPortsResponse Core.Int
oipprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED oipprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
