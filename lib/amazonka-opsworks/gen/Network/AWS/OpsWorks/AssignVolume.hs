{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.AssignVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns one of the stack's registered Amazon EBS volumes to a specified instance. The volume must first be registered with the stack by calling 'RegisterVolume' . After you register the volume, you must call 'UpdateVolume' to specify a mount point before calling @AssignVolume@ . For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.AssignVolume
    (
    -- * Creating a request
      AssignVolume (..)
    , mkAssignVolume
    -- ** Request lenses
    , avVolumeId
    , avInstanceId

    -- * Destructuring the response
    , AssignVolumeResponse (..)
    , mkAssignVolumeResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssignVolume' smart constructor.
data AssignVolume = AssignVolume'
  { volumeId :: Core.Text
    -- ^ The volume ID.
  , instanceId :: Core.Maybe Core.Text
    -- ^ The instance ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssignVolume' value with any optional fields omitted.
mkAssignVolume
    :: Core.Text -- ^ 'volumeId'
    -> AssignVolume
mkAssignVolume volumeId
  = AssignVolume'{volumeId, instanceId = Core.Nothing}

-- | The volume ID.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avVolumeId :: Lens.Lens' AssignVolume Core.Text
avVolumeId = Lens.field @"volumeId"
{-# INLINEABLE avVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avInstanceId :: Lens.Lens' AssignVolume (Core.Maybe Core.Text)
avInstanceId = Lens.field @"instanceId"
{-# INLINEABLE avInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery AssignVolume where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssignVolume where
        toHeaders AssignVolume{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.AssignVolume")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssignVolume where
        toJSON AssignVolume{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("VolumeId" Core..= volumeId),
                  ("InstanceId" Core..=) Core.<$> instanceId])

instance Core.AWSRequest AssignVolume where
        type Rs AssignVolume = AssignVolumeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull AssignVolumeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssignVolumeResponse' smart constructor.
data AssignVolumeResponse = AssignVolumeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssignVolumeResponse' value with any optional fields omitted.
mkAssignVolumeResponse
    :: AssignVolumeResponse
mkAssignVolumeResponse = AssignVolumeResponse'
