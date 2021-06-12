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
-- Module      : Network.AWS.OpsWorks.Types.EbsBlockDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.EbsBlockDevice where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.VolumeType

-- | Describes an Amazon EBS volume. This data type maps directly to the
-- Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>
-- data type.
--
-- /See:/ 'newEbsBlockDevice' smart constructor.
data EbsBlockDevice = EbsBlockDevice'
  { -- | Whether the volume is deleted on instance termination.
    deleteOnTermination :: Core.Maybe Core.Bool,
    -- | The snapshot ID.
    snapshotId :: Core.Maybe Core.Text,
    -- | The volume type. @gp2@ for General Purpose (SSD) volumes, @io1@ for
    -- Provisioned IOPS (SSD) volumes, @st1@ for Throughput Optimized hard disk
    -- drives (HDD), @sc1@ for Cold HDD,and @standard@ for Magnetic volumes.
    --
    -- If you specify the @io1@ volume type, you must also specify a value for
    -- the @Iops@ attribute. The maximum ratio of provisioned IOPS to requested
    -- volume size (in GiB) is 50:1. AWS uses the default volume size (in GiB)
    -- specified in the AMI attributes to set IOPS to 50 x (volume size).
    volumeType :: Core.Maybe VolumeType,
    -- | The volume size, in GiB. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
    volumeSize :: Core.Maybe Core.Int,
    -- | The number of I\/O operations per second (IOPS) that the volume
    -- supports. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
    iops :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EbsBlockDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteOnTermination', 'ebsBlockDevice_deleteOnTermination' - Whether the volume is deleted on instance termination.
--
-- 'snapshotId', 'ebsBlockDevice_snapshotId' - The snapshot ID.
--
-- 'volumeType', 'ebsBlockDevice_volumeType' - The volume type. @gp2@ for General Purpose (SSD) volumes, @io1@ for
-- Provisioned IOPS (SSD) volumes, @st1@ for Throughput Optimized hard disk
-- drives (HDD), @sc1@ for Cold HDD,and @standard@ for Magnetic volumes.
--
-- If you specify the @io1@ volume type, you must also specify a value for
-- the @Iops@ attribute. The maximum ratio of provisioned IOPS to requested
-- volume size (in GiB) is 50:1. AWS uses the default volume size (in GiB)
-- specified in the AMI attributes to set IOPS to 50 x (volume size).
--
-- 'volumeSize', 'ebsBlockDevice_volumeSize' - The volume size, in GiB. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
--
-- 'iops', 'ebsBlockDevice_iops' - The number of I\/O operations per second (IOPS) that the volume
-- supports. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
newEbsBlockDevice ::
  EbsBlockDevice
newEbsBlockDevice =
  EbsBlockDevice'
    { deleteOnTermination = Core.Nothing,
      snapshotId = Core.Nothing,
      volumeType = Core.Nothing,
      volumeSize = Core.Nothing,
      iops = Core.Nothing
    }

-- | Whether the volume is deleted on instance termination.
ebsBlockDevice_deleteOnTermination :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Bool)
ebsBlockDevice_deleteOnTermination = Lens.lens (\EbsBlockDevice' {deleteOnTermination} -> deleteOnTermination) (\s@EbsBlockDevice' {} a -> s {deleteOnTermination = a} :: EbsBlockDevice)

-- | The snapshot ID.
ebsBlockDevice_snapshotId :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Text)
ebsBlockDevice_snapshotId = Lens.lens (\EbsBlockDevice' {snapshotId} -> snapshotId) (\s@EbsBlockDevice' {} a -> s {snapshotId = a} :: EbsBlockDevice)

-- | The volume type. @gp2@ for General Purpose (SSD) volumes, @io1@ for
-- Provisioned IOPS (SSD) volumes, @st1@ for Throughput Optimized hard disk
-- drives (HDD), @sc1@ for Cold HDD,and @standard@ for Magnetic volumes.
--
-- If you specify the @io1@ volume type, you must also specify a value for
-- the @Iops@ attribute. The maximum ratio of provisioned IOPS to requested
-- volume size (in GiB) is 50:1. AWS uses the default volume size (in GiB)
-- specified in the AMI attributes to set IOPS to 50 x (volume size).
ebsBlockDevice_volumeType :: Lens.Lens' EbsBlockDevice (Core.Maybe VolumeType)
ebsBlockDevice_volumeType = Lens.lens (\EbsBlockDevice' {volumeType} -> volumeType) (\s@EbsBlockDevice' {} a -> s {volumeType = a} :: EbsBlockDevice)

-- | The volume size, in GiB. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
ebsBlockDevice_volumeSize :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Int)
ebsBlockDevice_volumeSize = Lens.lens (\EbsBlockDevice' {volumeSize} -> volumeSize) (\s@EbsBlockDevice' {} a -> s {volumeSize = a} :: EbsBlockDevice)

-- | The number of I\/O operations per second (IOPS) that the volume
-- supports. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
ebsBlockDevice_iops :: Lens.Lens' EbsBlockDevice (Core.Maybe Core.Int)
ebsBlockDevice_iops = Lens.lens (\EbsBlockDevice' {iops} -> iops) (\s@EbsBlockDevice' {} a -> s {iops = a} :: EbsBlockDevice)

instance Core.FromJSON EbsBlockDevice where
  parseJSON =
    Core.withObject
      "EbsBlockDevice"
      ( \x ->
          EbsBlockDevice'
            Core.<$> (x Core..:? "DeleteOnTermination")
            Core.<*> (x Core..:? "SnapshotId")
            Core.<*> (x Core..:? "VolumeType")
            Core.<*> (x Core..:? "VolumeSize")
            Core.<*> (x Core..:? "Iops")
      )

instance Core.Hashable EbsBlockDevice

instance Core.NFData EbsBlockDevice

instance Core.ToJSON EbsBlockDevice where
  toJSON EbsBlockDevice' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeleteOnTermination" Core..=)
              Core.<$> deleteOnTermination,
            ("SnapshotId" Core..=) Core.<$> snapshotId,
            ("VolumeType" Core..=) Core.<$> volumeType,
            ("VolumeSize" Core..=) Core.<$> volumeSize,
            ("Iops" Core..=) Core.<$> iops
          ]
      )
