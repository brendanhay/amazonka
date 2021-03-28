{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyVolume (..)
    , mkModifyVolume
    -- ** Request lenses
    , mvVolumeId
    , mvDryRun
    , mvIops
    , mvSize
    , mvVolumeType

    -- * Destructuring the response
    , ModifyVolumeResponse (..)
    , mkModifyVolumeResponse
    -- ** Response lenses
    , mvrrsVolumeModification
    , mvrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyVolume' smart constructor.
data ModifyVolume = ModifyVolume'
  { volumeId :: Types.VolumeId
    -- ^ The ID of the volume.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , iops :: Core.Maybe Core.Int
    -- ^ The target IOPS rate of the volume.
--
-- This is only valid for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes. For moreinformation, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html#EBSVolumeTypes_piops Provisioned IOPS SSD (io1 and io2) volumes> .
-- Default: If no IOPS value is specified, the existing value is retained.
  , size :: Core.Maybe Core.Int
    -- ^ The target size of the volume, in GiB. The target volume size must be greater than or equal to than the existing size of the volume. For information about available EBS volume sizes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> .
--
-- Default: If no size is specified, the existing size is retained.
  , volumeType :: Core.Maybe Types.VolumeType
    -- ^ The target EBS volume type of the volume.
--
-- Default: If no type is specified, the existing type is retained.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVolume' value with any optional fields omitted.
mkModifyVolume
    :: Types.VolumeId -- ^ 'volumeId'
    -> ModifyVolume
mkModifyVolume volumeId
  = ModifyVolume'{volumeId, dryRun = Core.Nothing,
                  iops = Core.Nothing, size = Core.Nothing,
                  volumeType = Core.Nothing}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvVolumeId :: Lens.Lens' ModifyVolume Types.VolumeId
mvVolumeId = Lens.field @"volumeId"
{-# INLINEABLE mvVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvDryRun :: Lens.Lens' ModifyVolume (Core.Maybe Core.Bool)
mvDryRun = Lens.field @"dryRun"
{-# INLINEABLE mvDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The target IOPS rate of the volume.
--
-- This is only valid for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes. For moreinformation, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html#EBSVolumeTypes_piops Provisioned IOPS SSD (io1 and io2) volumes> .
-- Default: If no IOPS value is specified, the existing value is retained.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvIops :: Lens.Lens' ModifyVolume (Core.Maybe Core.Int)
mvIops = Lens.field @"iops"
{-# INLINEABLE mvIops #-}
{-# DEPRECATED iops "Use generic-lens or generic-optics with 'iops' instead"  #-}

-- | The target size of the volume, in GiB. The target volume size must be greater than or equal to than the existing size of the volume. For information about available EBS volume sizes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> .
--
-- Default: If no size is specified, the existing size is retained.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvSize :: Lens.Lens' ModifyVolume (Core.Maybe Core.Int)
mvSize = Lens.field @"size"
{-# INLINEABLE mvSize #-}
{-# DEPRECATED size "Use generic-lens or generic-optics with 'size' instead"  #-}

-- | The target EBS volume type of the volume.
--
-- Default: If no type is specified, the existing type is retained.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvVolumeType :: Lens.Lens' ModifyVolume (Core.Maybe Types.VolumeType)
mvVolumeType = Lens.field @"volumeType"
{-# INLINEABLE mvVolumeType #-}
{-# DEPRECATED volumeType "Use generic-lens or generic-optics with 'volumeType' instead"  #-}

instance Core.ToQuery ModifyVolume where
        toQuery ModifyVolume{..}
          = Core.toQueryPair "Action" ("ModifyVolume" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VolumeId" volumeId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Iops") iops
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Size") size
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VolumeType") volumeType

instance Core.ToHeaders ModifyVolume where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyVolume where
        type Rs ModifyVolume = ModifyVolumeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ModifyVolumeResponse' Core.<$>
                   (x Core..@? "volumeModification") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyVolumeResponse' smart constructor.
data ModifyVolumeResponse = ModifyVolumeResponse'
  { volumeModification :: Core.Maybe Types.VolumeModification
    -- ^ Information about the volume modification.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyVolumeResponse' value with any optional fields omitted.
mkModifyVolumeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyVolumeResponse
mkModifyVolumeResponse responseStatus
  = ModifyVolumeResponse'{volumeModification = Core.Nothing,
                          responseStatus}

-- | Information about the volume modification.
--
-- /Note:/ Consider using 'volumeModification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvrrsVolumeModification :: Lens.Lens' ModifyVolumeResponse (Core.Maybe Types.VolumeModification)
mvrrsVolumeModification = Lens.field @"volumeModification"
{-# INLINEABLE mvrrsVolumeModification #-}
{-# DEPRECATED volumeModification "Use generic-lens or generic-optics with 'volumeModification' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvrrsResponseStatus :: Lens.Lens' ModifyVolumeResponse Core.Int
mvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
