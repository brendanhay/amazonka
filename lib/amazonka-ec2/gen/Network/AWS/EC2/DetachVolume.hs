{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DetachVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an EBS volume from an instance. Make sure to unmount any file systems on the device within your operating system before detaching the volume. Failure to do so can result in the volume becoming stuck in the @busy@ state while detaching. If this happens, detachment can be delayed indefinitely until you unmount the volume, force detachment, reboot the instance, or all three. If an EBS volume is the root device of an instance, it can't be detached while the instance is running. To detach the root volume, stop the instance first.
--
-- When a volume with an AWS Marketplace product code is detached from an instance, the product code is no longer associated with the instance.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-detaching-volume.html Detaching an Amazon EBS volume> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DetachVolume
  ( -- * Creating a request
    DetachVolume (..),
    mkDetachVolume,

    -- ** Request lenses
    dvVolumeId,
    dvDevice,
    dvDryRun,
    dvForce,
    dvInstanceId,

    -- * Destructuring the response
    Types.VolumeAttachment (..),
    Types.mkVolumeAttachment,

    -- ** Response lenses
    Types.vaAttachTime,
    Types.vaDeleteOnTermination,
    Types.vaDevice,
    Types.vaInstanceId,
    Types.vaState,
    Types.vaVolumeId,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachVolume' smart constructor.
data DetachVolume = DetachVolume'
  { -- | The ID of the volume.
    volumeId :: Types.VolumeId,
    -- | The device name.
    device :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Forces detachment if the previous detachment attempt did not occur cleanly (for example, logging into an instance, unmounting the volume, and detaching normally). This option can lead to data loss or a corrupted file system. Use this option only as a last resort to detach a volume from a failed instance. The instance won't have an opportunity to flush file system caches or file system metadata. If you use this option, you must perform file system check and repair procedures.
    force :: Core.Maybe Core.Bool,
    -- | The ID of the instance. If you are detaching a Multi-Attach enabled volume, you must specify an instance ID.
    instanceId :: Core.Maybe Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachVolume' value with any optional fields omitted.
mkDetachVolume ::
  -- | 'volumeId'
  Types.VolumeId ->
  DetachVolume
mkDetachVolume volumeId =
  DetachVolume'
    { volumeId,
      device = Core.Nothing,
      dryRun = Core.Nothing,
      force = Core.Nothing,
      instanceId = Core.Nothing
    }

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvVolumeId :: Lens.Lens' DetachVolume Types.VolumeId
dvVolumeId = Lens.field @"volumeId"
{-# DEPRECATED dvVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The device name.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvDevice :: Lens.Lens' DetachVolume (Core.Maybe Types.String)
dvDevice = Lens.field @"device"
{-# DEPRECATED dvDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvDryRun :: Lens.Lens' DetachVolume (Core.Maybe Core.Bool)
dvDryRun = Lens.field @"dryRun"
{-# DEPRECATED dvDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Forces detachment if the previous detachment attempt did not occur cleanly (for example, logging into an instance, unmounting the volume, and detaching normally). This option can lead to data loss or a corrupted file system. Use this option only as a last resort to detach a volume from a failed instance. The instance won't have an opportunity to flush file system caches or file system metadata. If you use this option, you must perform file system check and repair procedures.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvForce :: Lens.Lens' DetachVolume (Core.Maybe Core.Bool)
dvForce = Lens.field @"force"
{-# DEPRECATED dvForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The ID of the instance. If you are detaching a Multi-Attach enabled volume, you must specify an instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvInstanceId :: Lens.Lens' DetachVolume (Core.Maybe Types.InstanceId)
dvInstanceId = Lens.field @"instanceId"
{-# DEPRECATED dvInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.AWSRequest DetachVolume where
  type Rs DetachVolume = Types.VolumeAttachment
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DetachVolume")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "VolumeId" volumeId)
                Core.<> (Core.toQueryValue "Device" Core.<$> device)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "Force" Core.<$> force)
                Core.<> (Core.toQueryValue "InstanceId" Core.<$> instanceId)
            )
      }
  response = Response.receiveXML (\s h x -> Core.parseXML x)
