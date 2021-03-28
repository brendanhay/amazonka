{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.AssignInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assign a registered instance to a layer.
--
--
--     * You can assign registered on-premises instances to any layer type.
--
--
--     * You can assign registered Amazon EC2 instances only to custom layers.
--
--
--     * You cannot use this action with instances that were created with AWS OpsWorks Stacks.
--
--
-- __Required Permissions__ : To use this action, an AWS Identity and Access Management (IAM) user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.AssignInstance
    (
    -- * Creating a request
      AssignInstance (..)
    , mkAssignInstance
    -- ** Request lenses
    , aiInstanceId
    , aiLayerIds

    -- * Destructuring the response
    , AssignInstanceResponse (..)
    , mkAssignInstanceResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssignInstance' smart constructor.
data AssignInstance = AssignInstance'
  { instanceId :: Core.Text
    -- ^ The instance ID.
  , layerIds :: [Core.Text]
    -- ^ The layer ID, which must correspond to a custom layer. You cannot assign a registered instance to a built-in layer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssignInstance' value with any optional fields omitted.
mkAssignInstance
    :: Core.Text -- ^ 'instanceId'
    -> AssignInstance
mkAssignInstance instanceId
  = AssignInstance'{instanceId, layerIds = Core.mempty}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiInstanceId :: Lens.Lens' AssignInstance Core.Text
aiInstanceId = Lens.field @"instanceId"
{-# INLINEABLE aiInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The layer ID, which must correspond to a custom layer. You cannot assign a registered instance to a built-in layer.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiLayerIds :: Lens.Lens' AssignInstance [Core.Text]
aiLayerIds = Lens.field @"layerIds"
{-# INLINEABLE aiLayerIds #-}
{-# DEPRECATED layerIds "Use generic-lens or generic-optics with 'layerIds' instead"  #-}

instance Core.ToQuery AssignInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssignInstance where
        toHeaders AssignInstance{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.AssignInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssignInstance where
        toJSON AssignInstance{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceId" Core..= instanceId),
                  Core.Just ("LayerIds" Core..= layerIds)])

instance Core.AWSRequest AssignInstance where
        type Rs AssignInstance = AssignInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull AssignInstanceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssignInstanceResponse' smart constructor.
data AssignInstanceResponse = AssignInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssignInstanceResponse' value with any optional fields omitted.
mkAssignInstanceResponse
    :: AssignInstanceResponse
mkAssignInstanceResponse = AssignInstanceResponse'
