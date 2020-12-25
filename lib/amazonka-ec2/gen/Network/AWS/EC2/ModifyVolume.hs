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
    mvVolumeId,
    mvDryRun,
    mvIops,
    mvSize,
    mvVolumeType,

    -- * Destructuring the response
    ModifyVolumeResponse (..),
    mkModifyVolumeResponse,

    -- ** Response lenses
    mvrrsVolumeModification,
    mvrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyVolume' smart constructor.
data ModifyVolume = ModifyVolume'
  { -- | The ID of the volume.
    volumeId :: Types.VolumeId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The target IOPS rate of the volume.
    --
    -- This is only valid for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes. For moreinformation, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html#EBSVolumeTypes_piops Provisioned IOPS SSD (io1 and io2) volumes> .
    -- Default: If no IOPS value is specified, the existing value is retained.
    iops :: Core.Maybe Core.Int,
    -- | The target size of the volume, in GiB. The target volume size must be greater than or equal to than the existing size of the volume. For information about available EBS volume sizes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> .
    --
    -- Default: If no size is specified, the existing size is retained.
    size :: Core.Maybe Core.Int,
    -- | The target EBS volume type of the volume.
    --
    -- Default: If no type is specified, the existing type is retained.
    volumeType :: Core.Maybe Types.VolumeType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVolume' value with any optional fields omitted.
mkModifyVolume ::
  -- | 'volumeId'
  Types.VolumeId ->
  ModifyVolume
mkModifyVolume volumeId =
  ModifyVolume'
    { volumeId,
      dryRun = Core.Nothing,
      iops = Core.Nothing,
      size = Core.Nothing,
      volumeType = Core.Nothing
    }

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvVolumeId :: Lens.Lens' ModifyVolume Types.VolumeId
mvVolumeId = Lens.field @"volumeId"
{-# DEPRECATED mvVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvDryRun :: Lens.Lens' ModifyVolume (Core.Maybe Core.Bool)
mvDryRun = Lens.field @"dryRun"
{-# DEPRECATED mvDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The target IOPS rate of the volume.
--
-- This is only valid for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes. For moreinformation, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html#EBSVolumeTypes_piops Provisioned IOPS SSD (io1 and io2) volumes> .
-- Default: If no IOPS value is specified, the existing value is retained.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvIops :: Lens.Lens' ModifyVolume (Core.Maybe Core.Int)
mvIops = Lens.field @"iops"
{-# DEPRECATED mvIops "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The target size of the volume, in GiB. The target volume size must be greater than or equal to than the existing size of the volume. For information about available EBS volume sizes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> .
--
-- Default: If no size is specified, the existing size is retained.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvSize :: Lens.Lens' ModifyVolume (Core.Maybe Core.Int)
mvSize = Lens.field @"size"
{-# DEPRECATED mvSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The target EBS volume type of the volume.
--
-- Default: If no type is specified, the existing type is retained.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvVolumeType :: Lens.Lens' ModifyVolume (Core.Maybe Types.VolumeType)
mvVolumeType = Lens.field @"volumeType"
{-# DEPRECATED mvVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

instance Core.AWSRequest ModifyVolume where
  type Rs ModifyVolume = ModifyVolumeResponse
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
            ( Core.pure ("Action", "ModifyVolume")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "VolumeId" volumeId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "Iops" Core.<$> iops)
                Core.<> (Core.toQueryValue "Size" Core.<$> size)
                Core.<> (Core.toQueryValue "VolumeType" Core.<$> volumeType)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVolumeResponse'
            Core.<$> (x Core..@? "volumeModification")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyVolumeResponse' smart constructor.
data ModifyVolumeResponse = ModifyVolumeResponse'
  { -- | Information about the volume modification.
    volumeModification :: Core.Maybe Types.VolumeModification,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyVolumeResponse' value with any optional fields omitted.
mkModifyVolumeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyVolumeResponse
mkModifyVolumeResponse responseStatus =
  ModifyVolumeResponse'
    { volumeModification = Core.Nothing,
      responseStatus
    }

-- | Information about the volume modification.
--
-- /Note:/ Consider using 'volumeModification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvrrsVolumeModification :: Lens.Lens' ModifyVolumeResponse (Core.Maybe Types.VolumeModification)
mvrrsVolumeModification = Lens.field @"volumeModification"
{-# DEPRECATED mvrrsVolumeModification "Use generic-lens or generic-optics with 'volumeModification' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvrrsResponseStatus :: Lens.Lens' ModifyVolumeResponse Core.Int
mvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
