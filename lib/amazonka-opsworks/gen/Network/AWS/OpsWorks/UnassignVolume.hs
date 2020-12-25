{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UnassignVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns an assigned Amazon EBS volume. The volume remains registered with the stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UnassignVolume
  ( -- * Creating a request
    UnassignVolume (..),
    mkUnassignVolume,

    -- ** Request lenses
    uvVolumeId,

    -- * Destructuring the response
    UnassignVolumeResponse (..),
    mkUnassignVolumeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUnassignVolume' smart constructor.
newtype UnassignVolume = UnassignVolume'
  { -- | The volume ID.
    volumeId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UnassignVolume' value with any optional fields omitted.
mkUnassignVolume ::
  -- | 'volumeId'
  Types.String ->
  UnassignVolume
mkUnassignVolume volumeId = UnassignVolume' {volumeId}

-- | The volume ID.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvVolumeId :: Lens.Lens' UnassignVolume Types.String
uvVolumeId = Lens.field @"volumeId"
{-# DEPRECATED uvVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

instance Core.FromJSON UnassignVolume where
  toJSON UnassignVolume {..} =
    Core.object
      (Core.catMaybes [Core.Just ("VolumeId" Core..= volumeId)])

instance Core.AWSRequest UnassignVolume where
  type Rs UnassignVolume = UnassignVolumeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.UnassignVolume")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UnassignVolumeResponse'

-- | /See:/ 'mkUnassignVolumeResponse' smart constructor.
data UnassignVolumeResponse = UnassignVolumeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnassignVolumeResponse' value with any optional fields omitted.
mkUnassignVolumeResponse ::
  UnassignVolumeResponse
mkUnassignVolumeResponse = UnassignVolumeResponse'
