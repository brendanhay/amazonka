{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.VolumeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.VolumeInfo
  ( VolumeInfo (..),

    -- * Smart constructor
    mkVolumeInfo,

    -- * Lenses
    viGatewayARN,
    viGatewayId,
    viVolumeARN,
    viVolumeAttachmentStatus,
    viVolumeId,
    viVolumeSizeInBytes,
    viVolumeType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.GatewayARN as Types
import qualified Network.AWS.StorageGateway.Types.GatewayId as Types
import qualified Network.AWS.StorageGateway.Types.VolumeARN as Types
import qualified Network.AWS.StorageGateway.Types.VolumeAttachmentStatus as Types
import qualified Network.AWS.StorageGateway.Types.VolumeId as Types
import qualified Network.AWS.StorageGateway.Types.VolumeType as Types

-- | Describes a storage volume object.
--
-- /See:/ 'mkVolumeInfo' smart constructor.
data VolumeInfo = VolumeInfo'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
    --
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
    gatewayId :: Core.Maybe Types.GatewayId,
    -- | The Amazon Resource Name (ARN) for the storage volume. For example, the following is a valid ARN:
    --
    -- @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/volume/vol-1122AABB@
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
    volumeARN :: Core.Maybe Types.VolumeARN,
    -- | One of the VolumeStatus values that indicates the state of the storage volume.
    volumeAttachmentStatus :: Core.Maybe Types.VolumeAttachmentStatus,
    -- | The unique identifier assigned to the volume. This ID becomes part of the volume Amazon Resource Name (ARN), which you use as input for other operations.
    --
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
    volumeId :: Core.Maybe Types.VolumeId,
    -- | The size of the volume in bytes.
    --
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
    volumeSizeInBytes :: Core.Maybe Core.Integer,
    -- | One of the VolumeType enumeration values describing the type of the volume.
    volumeType :: Core.Maybe Types.VolumeType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VolumeInfo' value with any optional fields omitted.
mkVolumeInfo ::
  VolumeInfo
mkVolumeInfo =
  VolumeInfo'
    { gatewayARN = Core.Nothing,
      gatewayId = Core.Nothing,
      volumeARN = Core.Nothing,
      volumeAttachmentStatus = Core.Nothing,
      volumeId = Core.Nothing,
      volumeSizeInBytes = Core.Nothing,
      volumeType = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viGatewayARN :: Lens.Lens' VolumeInfo (Core.Maybe Types.GatewayARN)
viGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED viGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viGatewayId :: Lens.Lens' VolumeInfo (Core.Maybe Types.GatewayId)
viGatewayId = Lens.field @"gatewayId"
{-# DEPRECATED viGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | The Amazon Resource Name (ARN) for the storage volume. For example, the following is a valid ARN:
--
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/volume/vol-1122AABB@
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVolumeARN :: Lens.Lens' VolumeInfo (Core.Maybe Types.VolumeARN)
viVolumeARN = Lens.field @"volumeARN"
{-# DEPRECATED viVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | One of the VolumeStatus values that indicates the state of the storage volume.
--
-- /Note:/ Consider using 'volumeAttachmentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVolumeAttachmentStatus :: Lens.Lens' VolumeInfo (Core.Maybe Types.VolumeAttachmentStatus)
viVolumeAttachmentStatus = Lens.field @"volumeAttachmentStatus"
{-# DEPRECATED viVolumeAttachmentStatus "Use generic-lens or generic-optics with 'volumeAttachmentStatus' instead." #-}

-- | The unique identifier assigned to the volume. This ID becomes part of the volume Amazon Resource Name (ARN), which you use as input for other operations.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVolumeId :: Lens.Lens' VolumeInfo (Core.Maybe Types.VolumeId)
viVolumeId = Lens.field @"volumeId"
{-# DEPRECATED viVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The size of the volume in bytes.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- /Note:/ Consider using 'volumeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVolumeSizeInBytes :: Lens.Lens' VolumeInfo (Core.Maybe Core.Integer)
viVolumeSizeInBytes = Lens.field @"volumeSizeInBytes"
{-# DEPRECATED viVolumeSizeInBytes "Use generic-lens or generic-optics with 'volumeSizeInBytes' instead." #-}

-- | One of the VolumeType enumeration values describing the type of the volume.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVolumeType :: Lens.Lens' VolumeInfo (Core.Maybe Types.VolumeType)
viVolumeType = Lens.field @"volumeType"
{-# DEPRECATED viVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

instance Core.FromJSON VolumeInfo where
  parseJSON =
    Core.withObject "VolumeInfo" Core.$
      \x ->
        VolumeInfo'
          Core.<$> (x Core..:? "GatewayARN")
          Core.<*> (x Core..:? "GatewayId")
          Core.<*> (x Core..:? "VolumeARN")
          Core.<*> (x Core..:? "VolumeAttachmentStatus")
          Core.<*> (x Core..:? "VolumeId")
          Core.<*> (x Core..:? "VolumeSizeInBytes")
          Core.<*> (x Core..:? "VolumeType")
