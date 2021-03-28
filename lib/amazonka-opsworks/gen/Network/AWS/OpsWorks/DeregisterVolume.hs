{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeregisterVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an Amazon EBS volume. The volume can then be registered by another stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DeregisterVolume
    (
    -- * Creating a request
      DeregisterVolume (..)
    , mkDeregisterVolume
    -- ** Request lenses
    , dvVolumeId

    -- * Destructuring the response
    , DeregisterVolumeResponse (..)
    , mkDeregisterVolumeResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterVolume' smart constructor.
newtype DeregisterVolume = DeregisterVolume'
  { volumeId :: Core.Text
    -- ^ The AWS OpsWorks Stacks volume ID, which is the GUID that AWS OpsWorks Stacks assigned to the instance when you registered the volume with the stack, not the Amazon EC2 volume ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterVolume' value with any optional fields omitted.
mkDeregisterVolume
    :: Core.Text -- ^ 'volumeId'
    -> DeregisterVolume
mkDeregisterVolume volumeId = DeregisterVolume'{volumeId}

-- | The AWS OpsWorks Stacks volume ID, which is the GUID that AWS OpsWorks Stacks assigned to the instance when you registered the volume with the stack, not the Amazon EC2 volume ID.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvVolumeId :: Lens.Lens' DeregisterVolume Core.Text
dvVolumeId = Lens.field @"volumeId"
{-# INLINEABLE dvVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

instance Core.ToQuery DeregisterVolume where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeregisterVolume where
        toHeaders DeregisterVolume{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.DeregisterVolume")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeregisterVolume where
        toJSON DeregisterVolume{..}
          = Core.object
              (Core.catMaybes [Core.Just ("VolumeId" Core..= volumeId)])

instance Core.AWSRequest DeregisterVolume where
        type Rs DeregisterVolume = DeregisterVolumeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeregisterVolumeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterVolumeResponse' smart constructor.
data DeregisterVolumeResponse = DeregisterVolumeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterVolumeResponse' value with any optional fields omitted.
mkDeregisterVolumeResponse
    :: DeregisterVolumeResponse
mkDeregisterVolumeResponse = DeregisterVolumeResponse'
