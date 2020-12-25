{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeregisterVolume (..),
    mkDeregisterVolume,

    -- ** Request lenses
    dvVolumeId,

    -- * Destructuring the response
    DeregisterVolumeResponse (..),
    mkDeregisterVolumeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterVolume' smart constructor.
newtype DeregisterVolume = DeregisterVolume'
  { -- | The AWS OpsWorks Stacks volume ID, which is the GUID that AWS OpsWorks Stacks assigned to the instance when you registered the volume with the stack, not the Amazon EC2 volume ID.
    volumeId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterVolume' value with any optional fields omitted.
mkDeregisterVolume ::
  -- | 'volumeId'
  Types.String ->
  DeregisterVolume
mkDeregisterVolume volumeId = DeregisterVolume' {volumeId}

-- | The AWS OpsWorks Stacks volume ID, which is the GUID that AWS OpsWorks Stacks assigned to the instance when you registered the volume with the stack, not the Amazon EC2 volume ID.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvVolumeId :: Lens.Lens' DeregisterVolume Types.String
dvVolumeId = Lens.field @"volumeId"
{-# DEPRECATED dvVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

instance Core.FromJSON DeregisterVolume where
  toJSON DeregisterVolume {..} =
    Core.object
      (Core.catMaybes [Core.Just ("VolumeId" Core..= volumeId)])

instance Core.AWSRequest DeregisterVolume where
  type Rs DeregisterVolume = DeregisterVolumeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.DeregisterVolume")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeregisterVolumeResponse'

-- | /See:/ 'mkDeregisterVolumeResponse' smart constructor.
data DeregisterVolumeResponse = DeregisterVolumeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterVolumeResponse' value with any optional fields omitted.
mkDeregisterVolumeResponse ::
  DeregisterVolumeResponse
mkDeregisterVolumeResponse = DeregisterVolumeResponse'
