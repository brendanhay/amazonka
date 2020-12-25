{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateAssociationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of the Systems Manager document associated with the specified instance.
module Network.AWS.SSM.UpdateAssociationStatus
  ( -- * Creating a request
    UpdateAssociationStatus (..),
    mkUpdateAssociationStatus,

    -- ** Request lenses
    uasName,
    uasInstanceId,
    uasAssociationStatus,

    -- * Destructuring the response
    UpdateAssociationStatusResponse (..),
    mkUpdateAssociationStatusResponse,

    -- ** Response lenses
    uasrrsAssociationDescription,
    uasrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkUpdateAssociationStatus' smart constructor.
data UpdateAssociationStatus = UpdateAssociationStatus'
  { -- | The name of the Systems Manager document.
    name :: Types.DocumentARN,
    -- | The ID of the instance.
    instanceId :: Types.InstanceId,
    -- | The association status.
    associationStatus :: Types.AssociationStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateAssociationStatus' value with any optional fields omitted.
mkUpdateAssociationStatus ::
  -- | 'name'
  Types.DocumentARN ->
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'associationStatus'
  Types.AssociationStatus ->
  UpdateAssociationStatus
mkUpdateAssociationStatus name instanceId associationStatus =
  UpdateAssociationStatus' {name, instanceId, associationStatus}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasName :: Lens.Lens' UpdateAssociationStatus Types.DocumentARN
uasName = Lens.field @"name"
{-# DEPRECATED uasName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasInstanceId :: Lens.Lens' UpdateAssociationStatus Types.InstanceId
uasInstanceId = Lens.field @"instanceId"
{-# DEPRECATED uasInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The association status.
--
-- /Note:/ Consider using 'associationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasAssociationStatus :: Lens.Lens' UpdateAssociationStatus Types.AssociationStatus
uasAssociationStatus = Lens.field @"associationStatus"
{-# DEPRECATED uasAssociationStatus "Use generic-lens or generic-optics with 'associationStatus' instead." #-}

instance Core.FromJSON UpdateAssociationStatus where
  toJSON UpdateAssociationStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("InstanceId" Core..= instanceId),
            Core.Just ("AssociationStatus" Core..= associationStatus)
          ]
      )

instance Core.AWSRequest UpdateAssociationStatus where
  type Rs UpdateAssociationStatus = UpdateAssociationStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.UpdateAssociationStatus")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAssociationStatusResponse'
            Core.<$> (x Core..:? "AssociationDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateAssociationStatusResponse' smart constructor.
data UpdateAssociationStatusResponse = UpdateAssociationStatusResponse'
  { -- | Information about the association.
    associationDescription :: Core.Maybe Types.AssociationDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateAssociationStatusResponse' value with any optional fields omitted.
mkUpdateAssociationStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateAssociationStatusResponse
mkUpdateAssociationStatusResponse responseStatus =
  UpdateAssociationStatusResponse'
    { associationDescription =
        Core.Nothing,
      responseStatus
    }

-- | Information about the association.
--
-- /Note:/ Consider using 'associationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsAssociationDescription :: Lens.Lens' UpdateAssociationStatusResponse (Core.Maybe Types.AssociationDescription)
uasrrsAssociationDescription = Lens.field @"associationDescription"
{-# DEPRECATED uasrrsAssociationDescription "Use generic-lens or generic-optics with 'associationDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsResponseStatus :: Lens.Lens' UpdateAssociationStatusResponse Core.Int
uasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
