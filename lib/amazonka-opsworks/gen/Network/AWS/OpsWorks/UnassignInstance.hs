{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UnassignInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns a registered instance from all layers that are using the instance. The instance remains in the stack as an unassigned instance, and can be assigned to another layer as needed. You cannot use this action with instances that were created with AWS OpsWorks Stacks.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UnassignInstance
  ( -- * Creating a request
    UnassignInstance (..),
    mkUnassignInstance,

    -- ** Request lenses
    uInstanceId,

    -- * Destructuring the response
    UnassignInstanceResponse (..),
    mkUnassignInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUnassignInstance' smart constructor.
newtype UnassignInstance = UnassignInstance'
  { -- | The instance ID.
    instanceId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UnassignInstance' value with any optional fields omitted.
mkUnassignInstance ::
  -- | 'instanceId'
  Types.String ->
  UnassignInstance
mkUnassignInstance instanceId = UnassignInstance' {instanceId}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uInstanceId :: Lens.Lens' UnassignInstance Types.String
uInstanceId = Lens.field @"instanceId"
{-# DEPRECATED uInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.FromJSON UnassignInstance where
  toJSON UnassignInstance {..} =
    Core.object
      (Core.catMaybes [Core.Just ("InstanceId" Core..= instanceId)])

instance Core.AWSRequest UnassignInstance where
  type Rs UnassignInstance = UnassignInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.UnassignInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UnassignInstanceResponse'

-- | /See:/ 'mkUnassignInstanceResponse' smart constructor.
data UnassignInstanceResponse = UnassignInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnassignInstanceResponse' value with any optional fields omitted.
mkUnassignInstanceResponse ::
  UnassignInstanceResponse
mkUnassignInstanceResponse = UnassignInstanceResponse'
