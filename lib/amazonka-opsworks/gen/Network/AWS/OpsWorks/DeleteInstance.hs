{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeleteInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified instance, which terminates the associated Amazon EC2 instance. You must stop an instance before you can delete it.
--
-- For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-delete.html Deleting Instances> .
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DeleteInstance
  ( -- * Creating a request
    DeleteInstance (..),
    mkDeleteInstance,

    -- ** Request lenses
    diInstanceId,
    diDeleteElasticIp,
    diDeleteVolumes,

    -- * Destructuring the response
    DeleteInstanceResponse (..),
    mkDeleteInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteInstance' smart constructor.
data DeleteInstance = DeleteInstance'
  { -- | The instance ID.
    instanceId :: Types.String,
    -- | Whether to delete the instance Elastic IP address.
    deleteElasticIp :: Core.Maybe Core.Bool,
    -- | Whether to delete the instance's Amazon EBS volumes.
    deleteVolumes :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInstance' value with any optional fields omitted.
mkDeleteInstance ::
  -- | 'instanceId'
  Types.String ->
  DeleteInstance
mkDeleteInstance instanceId =
  DeleteInstance'
    { instanceId,
      deleteElasticIp = Core.Nothing,
      deleteVolumes = Core.Nothing
    }

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceId :: Lens.Lens' DeleteInstance Types.String
diInstanceId = Lens.field @"instanceId"
{-# DEPRECATED diInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Whether to delete the instance Elastic IP address.
--
-- /Note:/ Consider using 'deleteElasticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeleteElasticIp :: Lens.Lens' DeleteInstance (Core.Maybe Core.Bool)
diDeleteElasticIp = Lens.field @"deleteElasticIp"
{-# DEPRECATED diDeleteElasticIp "Use generic-lens or generic-optics with 'deleteElasticIp' instead." #-}

-- | Whether to delete the instance's Amazon EBS volumes.
--
-- /Note:/ Consider using 'deleteVolumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeleteVolumes :: Lens.Lens' DeleteInstance (Core.Maybe Core.Bool)
diDeleteVolumes = Lens.field @"deleteVolumes"
{-# DEPRECATED diDeleteVolumes "Use generic-lens or generic-optics with 'deleteVolumes' instead." #-}

instance Core.FromJSON DeleteInstance where
  toJSON DeleteInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceId" Core..= instanceId),
            ("DeleteElasticIp" Core..=) Core.<$> deleteElasticIp,
            ("DeleteVolumes" Core..=) Core.<$> deleteVolumes
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
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.DeleteInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteInstanceResponse'

-- | /See:/ 'mkDeleteInstanceResponse' smart constructor.
data DeleteInstanceResponse = DeleteInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInstanceResponse' value with any optional fields omitted.
mkDeleteInstanceResponse ::
  DeleteInstanceResponse
mkDeleteInstanceResponse = DeleteInstanceResponse'
