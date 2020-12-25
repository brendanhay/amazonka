{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AttachVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an EBS volume to a running or stopped instance and exposes it to the instance with the specified device name.
--
-- Encrypted EBS volumes must be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
-- After you attach an EBS volume, you must make it available. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-using-volumes.html Making an EBS volume available for use> .
-- If a volume has an AWS Marketplace product code:
--
--     * The volume can be attached only to a stopped instance.
--
--
--     * AWS Marketplace product codes are copied from the volume to the instance.
--
--
--     * You must be subscribed to the product.
--
--
--     * The instance type and operating system of the instance must support the product. For example, you can't detach a volume from a Windows instance and attach it to a Linux instance.
--
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-attaching-volume.html Attaching Amazon EBS volumes> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.AttachVolume
  ( -- * Creating a request
    AttachVolume (..),
    mkAttachVolume,

    -- ** Request lenses
    avDevice,
    avInstanceId,
    avVolumeId,
    avDryRun,

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

-- | /See:/ 'mkAttachVolume' smart constructor.
data AttachVolume = AttachVolume'
  { -- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
    device :: Types.Device,
    -- | The ID of the instance.
    instanceId :: Types.InstanceId,
    -- | The ID of the EBS volume. The volume and instance must be within the same Availability Zone.
    volumeId :: Types.VolumeId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachVolume' value with any optional fields omitted.
mkAttachVolume ::
  -- | 'device'
  Types.Device ->
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'volumeId'
  Types.VolumeId ->
  AttachVolume
mkAttachVolume device instanceId volumeId =
  AttachVolume'
    { device,
      instanceId,
      volumeId,
      dryRun = Core.Nothing
    }

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avDevice :: Lens.Lens' AttachVolume Types.Device
avDevice = Lens.field @"device"
{-# DEPRECATED avDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avInstanceId :: Lens.Lens' AttachVolume Types.InstanceId
avInstanceId = Lens.field @"instanceId"
{-# DEPRECATED avInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The ID of the EBS volume. The volume and instance must be within the same Availability Zone.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avVolumeId :: Lens.Lens' AttachVolume Types.VolumeId
avVolumeId = Lens.field @"volumeId"
{-# DEPRECATED avVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avDryRun :: Lens.Lens' AttachVolume (Core.Maybe Core.Bool)
avDryRun = Lens.field @"dryRun"
{-# DEPRECATED avDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest AttachVolume where
  type Rs AttachVolume = Types.VolumeAttachment
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
            ( Core.pure ("Action", "AttachVolume")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "Device" device)
                Core.<> (Core.toQueryValue "InstanceId" instanceId)
                Core.<> (Core.toQueryValue "VolumeId" volumeId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response = Response.receiveXML (\s h x -> Core.parseXML x)
