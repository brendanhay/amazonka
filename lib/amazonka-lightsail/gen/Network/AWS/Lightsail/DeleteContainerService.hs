{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteContainerService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes your Amazon Lightsail container service.
module Network.AWS.Lightsail.DeleteContainerService
  ( -- * Creating a request
    DeleteContainerService (..),
    mkDeleteContainerService,

    -- ** Request lenses
    dcsServiceName,

    -- * Destructuring the response
    DeleteContainerServiceResponse (..),
    mkDeleteContainerServiceResponse,

    -- ** Response lenses
    dcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteContainerService' smart constructor.
newtype DeleteContainerService = DeleteContainerService'
  { -- | The name of the container service to delete.
    serviceName :: Types.ServiceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContainerService' value with any optional fields omitted.
mkDeleteContainerService ::
  -- | 'serviceName'
  Types.ServiceName ->
  DeleteContainerService
mkDeleteContainerService serviceName =
  DeleteContainerService' {serviceName}

-- | The name of the container service to delete.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsServiceName :: Lens.Lens' DeleteContainerService Types.ServiceName
dcsServiceName = Lens.field @"serviceName"
{-# DEPRECATED dcsServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Core.FromJSON DeleteContainerService where
  toJSON DeleteContainerService {..} =
    Core.object
      (Core.catMaybes [Core.Just ("serviceName" Core..= serviceName)])

instance Core.AWSRequest DeleteContainerService where
  type Rs DeleteContainerService = DeleteContainerServiceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.DeleteContainerService")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContainerServiceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteContainerServiceResponse' smart constructor.
newtype DeleteContainerServiceResponse = DeleteContainerServiceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContainerServiceResponse' value with any optional fields omitted.
mkDeleteContainerServiceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteContainerServiceResponse
mkDeleteContainerServiceResponse responseStatus =
  DeleteContainerServiceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsResponseStatus :: Lens.Lens' DeleteContainerServiceResponse Core.Int
dcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
