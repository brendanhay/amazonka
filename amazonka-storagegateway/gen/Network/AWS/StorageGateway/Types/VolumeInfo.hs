{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.VolumeInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.VolumeInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a storage volume object.
--
-- /See:/ 'newVolumeInfo' smart constructor.
data VolumeInfo = VolumeInfo'
  { -- | The Amazon Resource Name (ARN) for the storage volume. For example, the
    -- following is a valid ARN:
    --
    -- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/volume\/vol-1122AABB@
    --
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
    -- hyphens (-).
    volumeARN :: Core.Maybe Core.Text,
    -- | The unique identifier assigned to the volume. This ID becomes part of
    -- the volume Amazon Resource Name (ARN), which you use as input for other
    -- operations.
    --
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
    -- hyphens (-).
    volumeId :: Core.Maybe Core.Text,
    -- | The size of the volume in bytes.
    --
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
    -- hyphens (-).
    volumeSizeInBytes :: Core.Maybe Core.Integer,
    -- | One of the VolumeType enumeration values describing the type of the
    -- volume.
    volumeType :: Core.Maybe Core.Text,
    gatewayARN :: Core.Maybe Core.Text,
    -- | One of the VolumeStatus values that indicates the state of the storage
    -- volume.
    volumeAttachmentStatus :: Core.Maybe Core.Text,
    -- | The unique identifier assigned to your gateway during activation. This
    -- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
    -- as input for other operations.
    --
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
    -- hyphens (-).
    gatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VolumeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARN', 'volumeInfo_volumeARN' - The Amazon Resource Name (ARN) for the storage volume. For example, the
-- following is a valid ARN:
--
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/volume\/vol-1122AABB@
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
--
-- 'volumeId', 'volumeInfo_volumeId' - The unique identifier assigned to the volume. This ID becomes part of
-- the volume Amazon Resource Name (ARN), which you use as input for other
-- operations.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
--
-- 'volumeSizeInBytes', 'volumeInfo_volumeSizeInBytes' - The size of the volume in bytes.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
--
-- 'volumeType', 'volumeInfo_volumeType' - One of the VolumeType enumeration values describing the type of the
-- volume.
--
-- 'gatewayARN', 'volumeInfo_gatewayARN' - Undocumented member.
--
-- 'volumeAttachmentStatus', 'volumeInfo_volumeAttachmentStatus' - One of the VolumeStatus values that indicates the state of the storage
-- volume.
--
-- 'gatewayId', 'volumeInfo_gatewayId' - The unique identifier assigned to your gateway during activation. This
-- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
-- as input for other operations.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
newVolumeInfo ::
  VolumeInfo
newVolumeInfo =
  VolumeInfo'
    { volumeARN = Core.Nothing,
      volumeId = Core.Nothing,
      volumeSizeInBytes = Core.Nothing,
      volumeType = Core.Nothing,
      gatewayARN = Core.Nothing,
      volumeAttachmentStatus = Core.Nothing,
      gatewayId = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) for the storage volume. For example, the
-- following is a valid ARN:
--
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/volume\/vol-1122AABB@
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
volumeInfo_volumeARN :: Lens.Lens' VolumeInfo (Core.Maybe Core.Text)
volumeInfo_volumeARN = Lens.lens (\VolumeInfo' {volumeARN} -> volumeARN) (\s@VolumeInfo' {} a -> s {volumeARN = a} :: VolumeInfo)

-- | The unique identifier assigned to the volume. This ID becomes part of
-- the volume Amazon Resource Name (ARN), which you use as input for other
-- operations.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
volumeInfo_volumeId :: Lens.Lens' VolumeInfo (Core.Maybe Core.Text)
volumeInfo_volumeId = Lens.lens (\VolumeInfo' {volumeId} -> volumeId) (\s@VolumeInfo' {} a -> s {volumeId = a} :: VolumeInfo)

-- | The size of the volume in bytes.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
volumeInfo_volumeSizeInBytes :: Lens.Lens' VolumeInfo (Core.Maybe Core.Integer)
volumeInfo_volumeSizeInBytes = Lens.lens (\VolumeInfo' {volumeSizeInBytes} -> volumeSizeInBytes) (\s@VolumeInfo' {} a -> s {volumeSizeInBytes = a} :: VolumeInfo)

-- | One of the VolumeType enumeration values describing the type of the
-- volume.
volumeInfo_volumeType :: Lens.Lens' VolumeInfo (Core.Maybe Core.Text)
volumeInfo_volumeType = Lens.lens (\VolumeInfo' {volumeType} -> volumeType) (\s@VolumeInfo' {} a -> s {volumeType = a} :: VolumeInfo)

-- | Undocumented member.
volumeInfo_gatewayARN :: Lens.Lens' VolumeInfo (Core.Maybe Core.Text)
volumeInfo_gatewayARN = Lens.lens (\VolumeInfo' {gatewayARN} -> gatewayARN) (\s@VolumeInfo' {} a -> s {gatewayARN = a} :: VolumeInfo)

-- | One of the VolumeStatus values that indicates the state of the storage
-- volume.
volumeInfo_volumeAttachmentStatus :: Lens.Lens' VolumeInfo (Core.Maybe Core.Text)
volumeInfo_volumeAttachmentStatus = Lens.lens (\VolumeInfo' {volumeAttachmentStatus} -> volumeAttachmentStatus) (\s@VolumeInfo' {} a -> s {volumeAttachmentStatus = a} :: VolumeInfo)

-- | The unique identifier assigned to your gateway during activation. This
-- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
-- as input for other operations.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
volumeInfo_gatewayId :: Lens.Lens' VolumeInfo (Core.Maybe Core.Text)
volumeInfo_gatewayId = Lens.lens (\VolumeInfo' {gatewayId} -> gatewayId) (\s@VolumeInfo' {} a -> s {gatewayId = a} :: VolumeInfo)

instance Core.FromJSON VolumeInfo where
  parseJSON =
    Core.withObject
      "VolumeInfo"
      ( \x ->
          VolumeInfo'
            Core.<$> (x Core..:? "VolumeARN")
            Core.<*> (x Core..:? "VolumeId")
            Core.<*> (x Core..:? "VolumeSizeInBytes")
            Core.<*> (x Core..:? "VolumeType")
            Core.<*> (x Core..:? "GatewayARN")
            Core.<*> (x Core..:? "VolumeAttachmentStatus")
            Core.<*> (x Core..:? "GatewayId")
      )

instance Core.Hashable VolumeInfo

instance Core.NFData VolumeInfo
