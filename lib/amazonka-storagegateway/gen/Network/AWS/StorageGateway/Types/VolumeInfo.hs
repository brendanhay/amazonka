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
    viVolumeAttachmentStatus,
    viVolumeARN,
    viVolumeSizeInBytes,
    viVolumeId,
    viGatewayId,
    viVolumeType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a storage volume object.
--
-- /See:/ 'mkVolumeInfo' smart constructor.
data VolumeInfo = VolumeInfo'
  { gatewayARN :: Lude.Maybe Lude.Text,
    volumeAttachmentStatus :: Lude.Maybe Lude.Text,
    volumeARN :: Lude.Maybe Lude.Text,
    volumeSizeInBytes :: Lude.Maybe Lude.Integer,
    volumeId :: Lude.Maybe Lude.Text,
    gatewayId :: Lude.Maybe Lude.Text,
    volumeType :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VolumeInfo' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'gatewayId' - The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
-- * 'volumeARN' - The Amazon Resource Name (ARN) for the storage volume. For example, the following is a valid ARN:
--
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/volume/vol-1122AABB@
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
-- * 'volumeAttachmentStatus' - One of the VolumeStatus values that indicates the state of the storage volume.
-- * 'volumeId' - The unique identifier assigned to the volume. This ID becomes part of the volume Amazon Resource Name (ARN), which you use as input for other operations.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
-- * 'volumeSizeInBytes' - The size of the volume in bytes.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
-- * 'volumeType' - One of the VolumeType enumeration values describing the type of the volume.
mkVolumeInfo ::
  VolumeInfo
mkVolumeInfo =
  VolumeInfo'
    { gatewayARN = Lude.Nothing,
      volumeAttachmentStatus = Lude.Nothing,
      volumeARN = Lude.Nothing,
      volumeSizeInBytes = Lude.Nothing,
      volumeId = Lude.Nothing,
      gatewayId = Lude.Nothing,
      volumeType = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viGatewayARN :: Lens.Lens' VolumeInfo (Lude.Maybe Lude.Text)
viGatewayARN = Lens.lens (gatewayARN :: VolumeInfo -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: VolumeInfo)
{-# DEPRECATED viGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | One of the VolumeStatus values that indicates the state of the storage volume.
--
-- /Note:/ Consider using 'volumeAttachmentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVolumeAttachmentStatus :: Lens.Lens' VolumeInfo (Lude.Maybe Lude.Text)
viVolumeAttachmentStatus = Lens.lens (volumeAttachmentStatus :: VolumeInfo -> Lude.Maybe Lude.Text) (\s a -> s {volumeAttachmentStatus = a} :: VolumeInfo)
{-# DEPRECATED viVolumeAttachmentStatus "Use generic-lens or generic-optics with 'volumeAttachmentStatus' instead." #-}

-- | The Amazon Resource Name (ARN) for the storage volume. For example, the following is a valid ARN:
--
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/volume/vol-1122AABB@
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVolumeARN :: Lens.Lens' VolumeInfo (Lude.Maybe Lude.Text)
viVolumeARN = Lens.lens (volumeARN :: VolumeInfo -> Lude.Maybe Lude.Text) (\s a -> s {volumeARN = a} :: VolumeInfo)
{-# DEPRECATED viVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The size of the volume in bytes.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- /Note:/ Consider using 'volumeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVolumeSizeInBytes :: Lens.Lens' VolumeInfo (Lude.Maybe Lude.Integer)
viVolumeSizeInBytes = Lens.lens (volumeSizeInBytes :: VolumeInfo -> Lude.Maybe Lude.Integer) (\s a -> s {volumeSizeInBytes = a} :: VolumeInfo)
{-# DEPRECATED viVolumeSizeInBytes "Use generic-lens or generic-optics with 'volumeSizeInBytes' instead." #-}

-- | The unique identifier assigned to the volume. This ID becomes part of the volume Amazon Resource Name (ARN), which you use as input for other operations.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVolumeId :: Lens.Lens' VolumeInfo (Lude.Maybe Lude.Text)
viVolumeId = Lens.lens (volumeId :: VolumeInfo -> Lude.Maybe Lude.Text) (\s a -> s {volumeId = a} :: VolumeInfo)
{-# DEPRECATED viVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viGatewayId :: Lens.Lens' VolumeInfo (Lude.Maybe Lude.Text)
viGatewayId = Lens.lens (gatewayId :: VolumeInfo -> Lude.Maybe Lude.Text) (\s a -> s {gatewayId = a} :: VolumeInfo)
{-# DEPRECATED viGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | One of the VolumeType enumeration values describing the type of the volume.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVolumeType :: Lens.Lens' VolumeInfo (Lude.Maybe Lude.Text)
viVolumeType = Lens.lens (volumeType :: VolumeInfo -> Lude.Maybe Lude.Text) (\s a -> s {volumeType = a} :: VolumeInfo)
{-# DEPRECATED viVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

instance Lude.FromJSON VolumeInfo where
  parseJSON =
    Lude.withObject
      "VolumeInfo"
      ( \x ->
          VolumeInfo'
            Lude.<$> (x Lude..:? "GatewayARN")
            Lude.<*> (x Lude..:? "VolumeAttachmentStatus")
            Lude.<*> (x Lude..:? "VolumeARN")
            Lude.<*> (x Lude..:? "VolumeSizeInBytes")
            Lude.<*> (x Lude..:? "VolumeId")
            Lude.<*> (x Lude..:? "GatewayId")
            Lude.<*> (x Lude..:? "VolumeType")
      )
