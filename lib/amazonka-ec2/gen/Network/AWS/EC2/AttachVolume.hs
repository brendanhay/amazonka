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
    avInstanceId,
    avDevice,
    avVolumeId,
    avDryRun,

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

-- | /See:/ 'mkAttachVolume' smart constructor.
data AttachVolume = AttachVolume'
  { -- | The ID of the instance.
    instanceId :: Lude.Text,
    -- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
    device :: Lude.Text,
    -- | The ID of the EBS volume. The volume and instance must be within the same Availability Zone.
    volumeId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachVolume' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'device' - The device name (for example, @/dev/sdh@ or @xvdh@ ).
-- * 'volumeId' - The ID of the EBS volume. The volume and instance must be within the same Availability Zone.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkAttachVolume ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'device'
  Lude.Text ->
  -- | 'volumeId'
  Lude.Text ->
  AttachVolume
mkAttachVolume pInstanceId_ pDevice_ pVolumeId_ =
  AttachVolume'
    { instanceId = pInstanceId_,
      device = pDevice_,
      volumeId = pVolumeId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avInstanceId :: Lens.Lens' AttachVolume Lude.Text
avInstanceId = Lens.lens (instanceId :: AttachVolume -> Lude.Text) (\s a -> s {instanceId = a} :: AttachVolume)
{-# DEPRECATED avInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avDevice :: Lens.Lens' AttachVolume Lude.Text
avDevice = Lens.lens (device :: AttachVolume -> Lude.Text) (\s a -> s {device = a} :: AttachVolume)
{-# DEPRECATED avDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | The ID of the EBS volume. The volume and instance must be within the same Availability Zone.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avVolumeId :: Lens.Lens' AttachVolume Lude.Text
avVolumeId = Lens.lens (volumeId :: AttachVolume -> Lude.Text) (\s a -> s {volumeId = a} :: AttachVolume)
{-# DEPRECATED avVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avDryRun :: Lens.Lens' AttachVolume (Lude.Maybe Lude.Bool)
avDryRun = Lens.lens (dryRun :: AttachVolume -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AttachVolume)
{-# DEPRECATED avDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AttachVolume where
  type Rs AttachVolume = VolumeAttachment
  request = Req.postQuery ec2Service
  response = Res.receiveXML (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders AttachVolume where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AttachVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachVolume where
  toQuery AttachVolume' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AttachVolume" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "Device" Lude.=: device,
        "VolumeId" Lude.=: volumeId,
        "DryRun" Lude.=: dryRun
      ]
