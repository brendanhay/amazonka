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
    dvhInstanceId,
    dvhForce,
    dvhDevice,
    dvhVolumeId,
    dvhDryRun,

    -- * Destructuring the response
    VolumeAttachment (..),
    mkVolumeAttachment,

    -- ** Response lenses
    vafInstanceId,
    vafDeleteOnTermination,
    vafState,
    vafDevice,
    vafVolumeId,
    vafAttachTime,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachVolume' smart constructor.
data DetachVolume = DetachVolume'
  { -- | The ID of the instance. If you are detaching a Multi-Attach enabled volume, you must specify an instance ID.
    instanceId :: Lude.Maybe Lude.Text,
    -- | Forces detachment if the previous detachment attempt did not occur cleanly (for example, logging into an instance, unmounting the volume, and detaching normally). This option can lead to data loss or a corrupted file system. Use this option only as a last resort to detach a volume from a failed instance. The instance won't have an opportunity to flush file system caches or file system metadata. If you use this option, you must perform file system check and repair procedures.
    force :: Lude.Maybe Lude.Bool,
    -- | The device name.
    device :: Lude.Maybe Lude.Text,
    -- | The ID of the volume.
    volumeId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachVolume' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance. If you are detaching a Multi-Attach enabled volume, you must specify an instance ID.
-- * 'force' - Forces detachment if the previous detachment attempt did not occur cleanly (for example, logging into an instance, unmounting the volume, and detaching normally). This option can lead to data loss or a corrupted file system. Use this option only as a last resort to detach a volume from a failed instance. The instance won't have an opportunity to flush file system caches or file system metadata. If you use this option, you must perform file system check and repair procedures.
-- * 'device' - The device name.
-- * 'volumeId' - The ID of the volume.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDetachVolume ::
  -- | 'volumeId'
  Lude.Text ->
  DetachVolume
mkDetachVolume pVolumeId_ =
  DetachVolume'
    { instanceId = Lude.Nothing,
      force = Lude.Nothing,
      device = Lude.Nothing,
      volumeId = pVolumeId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the instance. If you are detaching a Multi-Attach enabled volume, you must specify an instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvhInstanceId :: Lens.Lens' DetachVolume (Lude.Maybe Lude.Text)
dvhInstanceId = Lens.lens (instanceId :: DetachVolume -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: DetachVolume)
{-# DEPRECATED dvhInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Forces detachment if the previous detachment attempt did not occur cleanly (for example, logging into an instance, unmounting the volume, and detaching normally). This option can lead to data loss or a corrupted file system. Use this option only as a last resort to detach a volume from a failed instance. The instance won't have an opportunity to flush file system caches or file system metadata. If you use this option, you must perform file system check and repair procedures.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvhForce :: Lens.Lens' DetachVolume (Lude.Maybe Lude.Bool)
dvhForce = Lens.lens (force :: DetachVolume -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: DetachVolume)
{-# DEPRECATED dvhForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The device name.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvhDevice :: Lens.Lens' DetachVolume (Lude.Maybe Lude.Text)
dvhDevice = Lens.lens (device :: DetachVolume -> Lude.Maybe Lude.Text) (\s a -> s {device = a} :: DetachVolume)
{-# DEPRECATED dvhDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvhVolumeId :: Lens.Lens' DetachVolume Lude.Text
dvhVolumeId = Lens.lens (volumeId :: DetachVolume -> Lude.Text) (\s a -> s {volumeId = a} :: DetachVolume)
{-# DEPRECATED dvhVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvhDryRun :: Lens.Lens' DetachVolume (Lude.Maybe Lude.Bool)
dvhDryRun = Lens.lens (dryRun :: DetachVolume -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DetachVolume)
{-# DEPRECATED dvhDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DetachVolume where
  type Rs DetachVolume = VolumeAttachment
  request = Req.postQuery ec2Service
  response = Res.receiveXML (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders DetachVolume where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetachVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachVolume where
  toQuery DetachVolume' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DetachVolume" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "Force" Lude.=: force,
        "Device" Lude.=: device,
        "VolumeId" Lude.=: volumeId,
        "DryRun" Lude.=: dryRun
      ]
