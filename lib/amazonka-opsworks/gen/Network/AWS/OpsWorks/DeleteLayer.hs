{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeleteLayer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified layer. You must first stop and then delete all associated instances or unassign registered instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-delete.html How to Delete a Layer> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DeleteLayer
  ( -- * Creating a request
    DeleteLayer (..),
    mkDeleteLayer,

    -- ** Request lenses
    dlLayerId,

    -- * Destructuring the response
    DeleteLayerResponse (..),
    mkDeleteLayerResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLayer' smart constructor.
newtype DeleteLayer = DeleteLayer'
  { -- | The layer ID.
    layerId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLayer' value with any optional fields omitted.
mkDeleteLayer ::
  -- | 'layerId'
  Types.String ->
  DeleteLayer
mkDeleteLayer layerId = DeleteLayer' {layerId}

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLayerId :: Lens.Lens' DeleteLayer Types.String
dlLayerId = Lens.field @"layerId"
{-# DEPRECATED dlLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

instance Core.FromJSON DeleteLayer where
  toJSON DeleteLayer {..} =
    Core.object
      (Core.catMaybes [Core.Just ("LayerId" Core..= layerId)])

instance Core.AWSRequest DeleteLayer where
  type Rs DeleteLayer = DeleteLayerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.DeleteLayer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteLayerResponse'

-- | /See:/ 'mkDeleteLayerResponse' smart constructor.
data DeleteLayerResponse = DeleteLayerResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLayerResponse' value with any optional fields omitted.
mkDeleteLayerResponse ::
  DeleteLayerResponse
mkDeleteLayerResponse = DeleteLayerResponse'
