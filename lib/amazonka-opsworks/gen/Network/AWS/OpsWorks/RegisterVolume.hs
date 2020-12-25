{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.RegisterVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an Amazon EBS volume with a specified stack. A volume can be registered with only one stack at a time. If the volume is already registered, you must first deregister it by calling 'DeregisterVolume' . For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.RegisterVolume
  ( -- * Creating a request
    RegisterVolume (..),
    mkRegisterVolume,

    -- ** Request lenses
    rvStackId,
    rvEc2VolumeId,

    -- * Destructuring the response
    RegisterVolumeResponse (..),
    mkRegisterVolumeResponse,

    -- ** Response lenses
    rvrrsVolumeId,
    rvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterVolume' smart constructor.
data RegisterVolume = RegisterVolume'
  { -- | The stack ID.
    stackId :: Types.String,
    -- | The Amazon EBS volume ID.
    ec2VolumeId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterVolume' value with any optional fields omitted.
mkRegisterVolume ::
  -- | 'stackId'
  Types.String ->
  RegisterVolume
mkRegisterVolume stackId =
  RegisterVolume' {stackId, ec2VolumeId = Core.Nothing}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvStackId :: Lens.Lens' RegisterVolume Types.String
rvStackId = Lens.field @"stackId"
{-# DEPRECATED rvStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The Amazon EBS volume ID.
--
-- /Note:/ Consider using 'ec2VolumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvEc2VolumeId :: Lens.Lens' RegisterVolume (Core.Maybe Types.String)
rvEc2VolumeId = Lens.field @"ec2VolumeId"
{-# DEPRECATED rvEc2VolumeId "Use generic-lens or generic-optics with 'ec2VolumeId' instead." #-}

instance Core.FromJSON RegisterVolume where
  toJSON RegisterVolume {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StackId" Core..= stackId),
            ("Ec2VolumeId" Core..=) Core.<$> ec2VolumeId
          ]
      )

instance Core.AWSRequest RegisterVolume where
  type Rs RegisterVolume = RegisterVolumeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.RegisterVolume")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterVolumeResponse'
            Core.<$> (x Core..:? "VolumeId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @RegisterVolume@ request.
--
-- /See:/ 'mkRegisterVolumeResponse' smart constructor.
data RegisterVolumeResponse = RegisterVolumeResponse'
  { -- | The volume ID.
    volumeId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterVolumeResponse' value with any optional fields omitted.
mkRegisterVolumeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterVolumeResponse
mkRegisterVolumeResponse responseStatus =
  RegisterVolumeResponse' {volumeId = Core.Nothing, responseStatus}

-- | The volume ID.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvrrsVolumeId :: Lens.Lens' RegisterVolumeResponse (Core.Maybe Types.String)
rvrrsVolumeId = Lens.field @"volumeId"
{-# DEPRECATED rvrrsVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvrrsResponseStatus :: Lens.Lens' RegisterVolumeResponse Core.Int
rvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
