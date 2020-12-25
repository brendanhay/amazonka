{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon EBS volume's name or mount point. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UpdateVolume
  ( -- * Creating a request
    UpdateVolume (..),
    mkUpdateVolume,

    -- ** Request lenses
    uVolumeId,
    uMountPoint,
    uName,

    -- * Destructuring the response
    UpdateVolumeResponse (..),
    mkUpdateVolumeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateVolume' smart constructor.
data UpdateVolume = UpdateVolume'
  { -- | The volume ID.
    volumeId :: Types.String,
    -- | The new mount point.
    mountPoint :: Core.Maybe Types.String,
    -- | The new name.
    name :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateVolume' value with any optional fields omitted.
mkUpdateVolume ::
  -- | 'volumeId'
  Types.String ->
  UpdateVolume
mkUpdateVolume volumeId =
  UpdateVolume'
    { volumeId,
      mountPoint = Core.Nothing,
      name = Core.Nothing
    }

-- | The volume ID.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uVolumeId :: Lens.Lens' UpdateVolume Types.String
uVolumeId = Lens.field @"volumeId"
{-# DEPRECATED uVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The new mount point.
--
-- /Note:/ Consider using 'mountPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uMountPoint :: Lens.Lens' UpdateVolume (Core.Maybe Types.String)
uMountPoint = Lens.field @"mountPoint"
{-# DEPRECATED uMountPoint "Use generic-lens or generic-optics with 'mountPoint' instead." #-}

-- | The new name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' UpdateVolume (Core.Maybe Types.String)
uName = Lens.field @"name"
{-# DEPRECATED uName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateVolume where
  toJSON UpdateVolume {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("VolumeId" Core..= volumeId),
            ("MountPoint" Core..=) Core.<$> mountPoint,
            ("Name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest UpdateVolume where
  type Rs UpdateVolume = UpdateVolumeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.UpdateVolume")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateVolumeResponse'

-- | /See:/ 'mkUpdateVolumeResponse' smart constructor.
data UpdateVolumeResponse = UpdateVolumeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateVolumeResponse' value with any optional fields omitted.
mkUpdateVolumeResponse ::
  UpdateVolumeResponse
mkUpdateVolumeResponse = UpdateVolumeResponse'
