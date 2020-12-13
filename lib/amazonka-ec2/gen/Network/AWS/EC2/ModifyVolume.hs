{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can modify several parameters of an existing EBS volume, including volume size, volume type, and IOPS capacity. If your EBS volume is attached to a current-generation EC2 instance type, you may be able to apply these changes without stopping the instance or detaching the volume from it. For more information about modifying an EBS volume running Linux, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html Modifying the size, IOPS, or type of an EBS volume on Linux> . For more information about modifying an EBS volume running Windows, see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-expand-volume.html Modifying the size, IOPS, or type of an EBS volume on Windows> .
--
-- When you complete a resize operation on your volume, you need to extend the volume's file-system size to take advantage of the new storage capacity. For information about extending a Linux file system, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html#recognize-expanded-volume-linux Extending a Linux file system> . For information about extending a Windows file system, see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-expand-volume.html#recognize-expanded-volume-windows Extending a Windows file system> .
-- You can use CloudWatch Events to check the status of a modification to an EBS volume. For information about CloudWatch Events, see the <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ Amazon CloudWatch Events User Guide> . You can also track the status of a modification using 'DescribeVolumesModifications' . For information about tracking status changes using either method, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html#monitoring_mods Monitoring volume modifications> .
-- With previous-generation instance types, resizing an EBS volume may require detaching and reattaching the volume or stopping and restarting the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html Modifying the size, IOPS, or type of an EBS volume on Linux> and <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-expand-volume.html Modifying the size, IOPS, or type of an EBS volume on Windows> .
-- If you reach the maximum volume modification rate per volume limit, you will need to wait at least six hours before applying further modifications to the affected EBS volume.
module Network.AWS.EC2.ModifyVolume
  ( -- * Creating a request
    ModifyVolume (..),
    mkModifyVolume,

    -- ** Request lenses
    mvSize,
    mvIOPS,
    mvVolumeId,
    mvVolumeType,
    mvDryRun,

    -- * Destructuring the response
    ModifyVolumeResponse (..),
    mkModifyVolumeResponse,

    -- ** Response lenses
    mvrsVolumeModification,
    mvrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyVolume' smart constructor.
data ModifyVolume = ModifyVolume'
  { -- | The target size of the volume, in GiB. The target volume size must be greater than or equal to than the existing size of the volume. For information about available EBS volume sizes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> .
    --
    -- Default: If no size is specified, the existing size is retained.
    size :: Lude.Maybe Lude.Int,
    -- | The target IOPS rate of the volume.
    --
    -- This is only valid for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes. For moreinformation, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html#EBSVolumeTypes_piops Provisioned IOPS SSD (io1 and io2) volumes> .
    -- Default: If no IOPS value is specified, the existing value is retained.
    iops :: Lude.Maybe Lude.Int,
    -- | The ID of the volume.
    volumeId :: Lude.Text,
    -- | The target EBS volume type of the volume.
    --
    -- Default: If no type is specified, the existing type is retained.
    volumeType :: Lude.Maybe VolumeType,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVolume' with the minimum fields required to make a request.
--
-- * 'size' - The target size of the volume, in GiB. The target volume size must be greater than or equal to than the existing size of the volume. For information about available EBS volume sizes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> .
--
-- Default: If no size is specified, the existing size is retained.
-- * 'iops' - The target IOPS rate of the volume.
--
-- This is only valid for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes. For moreinformation, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html#EBSVolumeTypes_piops Provisioned IOPS SSD (io1 and io2) volumes> .
-- Default: If no IOPS value is specified, the existing value is retained.
-- * 'volumeId' - The ID of the volume.
-- * 'volumeType' - The target EBS volume type of the volume.
--
-- Default: If no type is specified, the existing type is retained.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkModifyVolume ::
  -- | 'volumeId'
  Lude.Text ->
  ModifyVolume
mkModifyVolume pVolumeId_ =
  ModifyVolume'
    { size = Lude.Nothing,
      iops = Lude.Nothing,
      volumeId = pVolumeId_,
      volumeType = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The target size of the volume, in GiB. The target volume size must be greater than or equal to than the existing size of the volume. For information about available EBS volume sizes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> .
--
-- Default: If no size is specified, the existing size is retained.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvSize :: Lens.Lens' ModifyVolume (Lude.Maybe Lude.Int)
mvSize = Lens.lens (size :: ModifyVolume -> Lude.Maybe Lude.Int) (\s a -> s {size = a} :: ModifyVolume)
{-# DEPRECATED mvSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The target IOPS rate of the volume.
--
-- This is only valid for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes. For moreinformation, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html#EBSVolumeTypes_piops Provisioned IOPS SSD (io1 and io2) volumes> .
-- Default: If no IOPS value is specified, the existing value is retained.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvIOPS :: Lens.Lens' ModifyVolume (Lude.Maybe Lude.Int)
mvIOPS = Lens.lens (iops :: ModifyVolume -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: ModifyVolume)
{-# DEPRECATED mvIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvVolumeId :: Lens.Lens' ModifyVolume Lude.Text
mvVolumeId = Lens.lens (volumeId :: ModifyVolume -> Lude.Text) (\s a -> s {volumeId = a} :: ModifyVolume)
{-# DEPRECATED mvVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The target EBS volume type of the volume.
--
-- Default: If no type is specified, the existing type is retained.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvVolumeType :: Lens.Lens' ModifyVolume (Lude.Maybe VolumeType)
mvVolumeType = Lens.lens (volumeType :: ModifyVolume -> Lude.Maybe VolumeType) (\s a -> s {volumeType = a} :: ModifyVolume)
{-# DEPRECATED mvVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvDryRun :: Lens.Lens' ModifyVolume (Lude.Maybe Lude.Bool)
mvDryRun = Lens.lens (dryRun :: ModifyVolume -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyVolume)
{-# DEPRECATED mvDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyVolume where
  type Rs ModifyVolume = ModifyVolumeResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyVolumeResponse'
            Lude.<$> (x Lude..@? "volumeModification")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyVolume where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyVolume where
  toQuery ModifyVolume' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyVolume" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Size" Lude.=: size,
        "Iops" Lude.=: iops,
        "VolumeId" Lude.=: volumeId,
        "VolumeType" Lude.=: volumeType,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyVolumeResponse' smart constructor.
data ModifyVolumeResponse = ModifyVolumeResponse'
  { -- | Information about the volume modification.
    volumeModification :: Lude.Maybe VolumeModification,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVolumeResponse' with the minimum fields required to make a request.
--
-- * 'volumeModification' - Information about the volume modification.
-- * 'responseStatus' - The response status code.
mkModifyVolumeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyVolumeResponse
mkModifyVolumeResponse pResponseStatus_ =
  ModifyVolumeResponse'
    { volumeModification = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the volume modification.
--
-- /Note:/ Consider using 'volumeModification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvrsVolumeModification :: Lens.Lens' ModifyVolumeResponse (Lude.Maybe VolumeModification)
mvrsVolumeModification = Lens.lens (volumeModification :: ModifyVolumeResponse -> Lude.Maybe VolumeModification) (\s a -> s {volumeModification = a} :: ModifyVolumeResponse)
{-# DEPRECATED mvrsVolumeModification "Use generic-lens or generic-optics with 'volumeModification' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvrsResponseStatus :: Lens.Lens' ModifyVolumeResponse Lude.Int
mvrsResponseStatus = Lens.lens (responseStatus :: ModifyVolumeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyVolumeResponse)
{-# DEPRECATED mvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
