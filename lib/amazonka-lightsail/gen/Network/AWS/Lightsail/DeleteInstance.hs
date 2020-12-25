{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Lightsail instance.
--
-- The @delete instance@ operation supports tag-based access control via resource tags applied to the resource identified by @instance name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteInstance
  ( -- * Creating a request
    DeleteInstance (..),
    mkDeleteInstance,

    -- ** Request lenses
    diInstanceName,
    diForceDeleteAddOns,

    -- * Destructuring the response
    DeleteInstanceResponse (..),
    mkDeleteInstanceResponse,

    -- ** Response lenses
    dirrsOperations,
    dirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteInstance' smart constructor.
data DeleteInstance = DeleteInstance'
  { -- | The name of the instance to delete.
    instanceName :: Types.ResourceName,
    -- | A Boolean value to indicate whether to delete the enabled add-ons for the disk.
    forceDeleteAddOns :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInstance' value with any optional fields omitted.
mkDeleteInstance ::
  -- | 'instanceName'
  Types.ResourceName ->
  DeleteInstance
mkDeleteInstance instanceName =
  DeleteInstance' {instanceName, forceDeleteAddOns = Core.Nothing}

-- | The name of the instance to delete.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceName :: Lens.Lens' DeleteInstance Types.ResourceName
diInstanceName = Lens.field @"instanceName"
{-# DEPRECATED diInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

-- | A Boolean value to indicate whether to delete the enabled add-ons for the disk.
--
-- /Note:/ Consider using 'forceDeleteAddOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diForceDeleteAddOns :: Lens.Lens' DeleteInstance (Core.Maybe Core.Bool)
diForceDeleteAddOns = Lens.field @"forceDeleteAddOns"
{-# DEPRECATED diForceDeleteAddOns "Use generic-lens or generic-optics with 'forceDeleteAddOns' instead." #-}

instance Core.FromJSON DeleteInstance where
  toJSON DeleteInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("instanceName" Core..= instanceName),
            ("forceDeleteAddOns" Core..=) Core.<$> forceDeleteAddOns
          ]
      )

instance Core.AWSRequest DeleteInstance where
  type Rs DeleteInstance = DeleteInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.DeleteInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInstanceResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteInstanceResponse' smart constructor.
data DeleteInstanceResponse = DeleteInstanceResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteInstanceResponse' value with any optional fields omitted.
mkDeleteInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteInstanceResponse
mkDeleteInstanceResponse responseStatus =
  DeleteInstanceResponse'
    { operations = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsOperations :: Lens.Lens' DeleteInstanceResponse (Core.Maybe [Types.Operation])
dirrsOperations = Lens.field @"operations"
{-# DEPRECATED dirrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DeleteInstanceResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
