{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateManagedInstanceRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the Amazon Identity and Access Management (IAM) role that is assigned to the on-premises instance or virtual machines (VM). IAM roles are first assigned to these hybrid instances during the activation process. For more information, see 'CreateActivation' .
module Network.AWS.SSM.UpdateManagedInstanceRole
  ( -- * Creating a request
    UpdateManagedInstanceRole (..),
    mkUpdateManagedInstanceRole,

    -- ** Request lenses
    umirInstanceId,
    umirIamRole,

    -- * Destructuring the response
    UpdateManagedInstanceRoleResponse (..),
    mkUpdateManagedInstanceRoleResponse,

    -- ** Response lenses
    umirrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkUpdateManagedInstanceRole' smart constructor.
data UpdateManagedInstanceRole = UpdateManagedInstanceRole'
  { -- | The ID of the managed instance where you want to update the role.
    instanceId :: Types.InstanceId,
    -- | The IAM role you want to assign or change.
    iamRole :: Types.IamRole
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateManagedInstanceRole' value with any optional fields omitted.
mkUpdateManagedInstanceRole ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'iamRole'
  Types.IamRole ->
  UpdateManagedInstanceRole
mkUpdateManagedInstanceRole instanceId iamRole =
  UpdateManagedInstanceRole' {instanceId, iamRole}

-- | The ID of the managed instance where you want to update the role.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umirInstanceId :: Lens.Lens' UpdateManagedInstanceRole Types.InstanceId
umirInstanceId = Lens.field @"instanceId"
{-# DEPRECATED umirInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The IAM role you want to assign or change.
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umirIamRole :: Lens.Lens' UpdateManagedInstanceRole Types.IamRole
umirIamRole = Lens.field @"iamRole"
{-# DEPRECATED umirIamRole "Use generic-lens or generic-optics with 'iamRole' instead." #-}

instance Core.FromJSON UpdateManagedInstanceRole where
  toJSON UpdateManagedInstanceRole {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceId" Core..= instanceId),
            Core.Just ("IamRole" Core..= iamRole)
          ]
      )

instance Core.AWSRequest UpdateManagedInstanceRole where
  type
    Rs UpdateManagedInstanceRole =
      UpdateManagedInstanceRoleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.UpdateManagedInstanceRole")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateManagedInstanceRoleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateManagedInstanceRoleResponse' smart constructor.
newtype UpdateManagedInstanceRoleResponse = UpdateManagedInstanceRoleResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateManagedInstanceRoleResponse' value with any optional fields omitted.
mkUpdateManagedInstanceRoleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateManagedInstanceRoleResponse
mkUpdateManagedInstanceRoleResponse responseStatus =
  UpdateManagedInstanceRoleResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umirrrsResponseStatus :: Lens.Lens' UpdateManagedInstanceRoleResponse Core.Int
umirrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED umirrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
