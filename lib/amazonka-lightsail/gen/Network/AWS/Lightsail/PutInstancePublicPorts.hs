{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.PutInstancePublicPorts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Opens ports for a specific Amazon Lightsail instance, and specifies the IP addresses allowed to connect to the instance through the ports, and the protocol. This action also closes all currently open ports that are not included in the request. Include all of the ports and the protocols you want to open in your @PutInstancePublicPorts@ request. Or use the @OpenInstancePublicPorts@ action to open ports without closing currently open ports.
--
-- The @PutInstancePublicPorts@ action supports tag-based access control via resource tags applied to the resource identified by @instanceName@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.PutInstancePublicPorts
  ( -- * Creating a request
    PutInstancePublicPorts (..),
    mkPutInstancePublicPorts,

    -- ** Request lenses
    pippPortInfos,
    pippInstanceName,

    -- * Destructuring the response
    PutInstancePublicPortsResponse (..),
    mkPutInstancePublicPortsResponse,

    -- ** Response lenses
    pipprrsOperation,
    pipprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutInstancePublicPorts' smart constructor.
data PutInstancePublicPorts = PutInstancePublicPorts'
  { -- | An array of objects to describe the ports to open for the specified instance.
    portInfos :: [Types.PortInfo],
    -- | The name of the instance for which to open ports.
    instanceName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutInstancePublicPorts' value with any optional fields omitted.
mkPutInstancePublicPorts ::
  -- | 'instanceName'
  Types.ResourceName ->
  PutInstancePublicPorts
mkPutInstancePublicPorts instanceName =
  PutInstancePublicPorts' {portInfos = Core.mempty, instanceName}

-- | An array of objects to describe the ports to open for the specified instance.
--
-- /Note:/ Consider using 'portInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pippPortInfos :: Lens.Lens' PutInstancePublicPorts [Types.PortInfo]
pippPortInfos = Lens.field @"portInfos"
{-# DEPRECATED pippPortInfos "Use generic-lens or generic-optics with 'portInfos' instead." #-}

-- | The name of the instance for which to open ports.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pippInstanceName :: Lens.Lens' PutInstancePublicPorts Types.ResourceName
pippInstanceName = Lens.field @"instanceName"
{-# DEPRECATED pippInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Core.FromJSON PutInstancePublicPorts where
  toJSON PutInstancePublicPorts {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("portInfos" Core..= portInfos),
            Core.Just ("instanceName" Core..= instanceName)
          ]
      )

instance Core.AWSRequest PutInstancePublicPorts where
  type Rs PutInstancePublicPorts = PutInstancePublicPortsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.PutInstancePublicPorts")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutInstancePublicPortsResponse'
            Core.<$> (x Core..:? "operation") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutInstancePublicPortsResponse' smart constructor.
data PutInstancePublicPortsResponse = PutInstancePublicPortsResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Core.Maybe Types.Operation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PutInstancePublicPortsResponse' value with any optional fields omitted.
mkPutInstancePublicPortsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutInstancePublicPortsResponse
mkPutInstancePublicPortsResponse responseStatus =
  PutInstancePublicPortsResponse'
    { operation = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipprrsOperation :: Lens.Lens' PutInstancePublicPortsResponse (Core.Maybe Types.Operation)
pipprrsOperation = Lens.field @"operation"
{-# DEPRECATED pipprrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipprrsResponseStatus :: Lens.Lens' PutInstancePublicPortsResponse Core.Int
pipprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pipprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
