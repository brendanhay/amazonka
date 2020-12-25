{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.StopInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specified instance. When you stop a standard instance, the data disappears and must be reinstalled when you restart the instance. You can stop an Amazon EBS-backed instance without losing data. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html Starting, Stopping, and Rebooting Instances> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.StopInstance
  ( -- * Creating a request
    StopInstance (..),
    mkStopInstance,

    -- ** Request lenses
    siInstanceId,
    siForce,

    -- * Destructuring the response
    StopInstanceResponse (..),
    mkStopInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopInstance' smart constructor.
data StopInstance = StopInstance'
  { -- | The instance ID.
    instanceId :: Types.String,
    -- | Specifies whether to force an instance to stop. If the instance's root device type is @ebs@ , or EBS-backed, adding the @Force@ parameter to the @StopInstances@ API call disassociates the AWS OpsWorks Stacks instance from EC2, and forces deletion of /only/ the OpsWorks Stacks instance. You must also delete the formerly-associated instance in EC2 after troubleshooting and replacing the AWS OpsWorks Stacks instance with a new one.
    force :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopInstance' value with any optional fields omitted.
mkStopInstance ::
  -- | 'instanceId'
  Types.String ->
  StopInstance
mkStopInstance instanceId =
  StopInstance' {instanceId, force = Core.Nothing}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siInstanceId :: Lens.Lens' StopInstance Types.String
siInstanceId = Lens.field @"instanceId"
{-# DEPRECATED siInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Specifies whether to force an instance to stop. If the instance's root device type is @ebs@ , or EBS-backed, adding the @Force@ parameter to the @StopInstances@ API call disassociates the AWS OpsWorks Stacks instance from EC2, and forces deletion of /only/ the OpsWorks Stacks instance. You must also delete the formerly-associated instance in EC2 after troubleshooting and replacing the AWS OpsWorks Stacks instance with a new one.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siForce :: Lens.Lens' StopInstance (Core.Maybe Core.Bool)
siForce = Lens.field @"force"
{-# DEPRECATED siForce "Use generic-lens or generic-optics with 'force' instead." #-}

instance Core.FromJSON StopInstance where
  toJSON StopInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceId" Core..= instanceId),
            ("Force" Core..=) Core.<$> force
          ]
      )

instance Core.AWSRequest StopInstance where
  type Rs StopInstance = StopInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.StopInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull StopInstanceResponse'

-- | /See:/ 'mkStopInstanceResponse' smart constructor.
data StopInstanceResponse = StopInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopInstanceResponse' value with any optional fields omitted.
mkStopInstanceResponse ::
  StopInstanceResponse
mkStopInstanceResponse = StopInstanceResponse'
