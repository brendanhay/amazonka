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
-- Module      : Amazonka.OpsWorks.Types.EbsBlockDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.EbsBlockDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types.VolumeType
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon EBS volume. This data type maps directly to the
-- Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>
-- data type.
--
-- /See:/ 'newEbsBlockDevice' smart constructor.
data EbsBlockDevice = EbsBlockDevice'
  { -- | Whether the volume is deleted on instance termination.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The number of I\/O operations per second (IOPS) that the volume
    -- supports. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The snapshot ID.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The volume size, in GiB. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
    volumeSize :: Prelude.Maybe Prelude.Int,
    -- | The volume type. @gp2@ for General Purpose (SSD) volumes, @io1@ for
    -- Provisioned IOPS (SSD) volumes, @st1@ for Throughput Optimized hard disk
    -- drives (HDD), @sc1@ for Cold HDD,and @standard@ for Magnetic volumes.
    --
    -- If you specify the @io1@ volume type, you must also specify a value for
    -- the @Iops@ attribute. The maximum ratio of provisioned IOPS to requested
    -- volume size (in GiB) is 50:1. AWS uses the default volume size (in GiB)
    -- specified in the AMI attributes to set IOPS to 50 x (volume size).
    volumeType :: Prelude.Maybe VolumeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'iops', 'ebsBlockDevice_iops' - The number of I\/O operations per second (IOPS) that the volume
-- supports. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
--
-- 'snapshotId', 'ebsBlockDevice_snapshotId' - The snapshot ID.
--
-- 'volumeSize', 'ebsBlockDevice_volumeSize' - The volume size, in GiB. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
--
-- 'volumeType', 'ebsBlockDevice_volumeType' - The volume type. @gp2@ for General Purpose (SSD) volumes, @io1@ for
-- Provisioned IOPS (SSD) volumes, @st1@ for Throughput Optimized hard disk
-- drives (HDD), @sc1@ for Cold HDD,and @standard@ for Magnetic volumes.
--
-- If you specify the @io1@ volume type, you must also specify a value for
-- the @Iops@ attribute. The maximum ratio of provisioned IOPS to requested
-- volume size (in GiB) is 50:1. AWS uses the default volume size (in GiB)
-- specified in the AMI attributes to set IOPS to 50 x (volume size).
newEbsBlockDevice ::
  EbsBlockDevice
newEbsBlockDevice =
  EbsBlockDevice'
    { deleteOnTermination =
        Prelude.Nothing,
      iops = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      volumeType = Prelude.Nothing
    }

-- | Whether the volume is deleted on instance termination.
ebsBlockDevice_deleteOnTermination :: Lens.Lens' EbsBlockDevice (Prelude.Maybe Prelude.Bool)
ebsBlockDevice_deleteOnTermination = Lens.lens (\EbsBlockDevice' {deleteOnTermination} -> deleteOnTermination) (\s@EbsBlockDevice' {} a -> s {deleteOnTermination = a} :: EbsBlockDevice)

-- | The number of I\/O operations per second (IOPS) that the volume
-- supports. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
ebsBlockDevice_iops :: Lens.Lens' EbsBlockDevice (Prelude.Maybe Prelude.Int)
ebsBlockDevice_iops = Lens.lens (\EbsBlockDevice' {iops} -> iops) (\s@EbsBlockDevice' {} a -> s {iops = a} :: EbsBlockDevice)

-- | The snapshot ID.
ebsBlockDevice_snapshotId :: Lens.Lens' EbsBlockDevice (Prelude.Maybe Prelude.Text)
ebsBlockDevice_snapshotId = Lens.lens (\EbsBlockDevice' {snapshotId} -> snapshotId) (\s@EbsBlockDevice' {} a -> s {snapshotId = a} :: EbsBlockDevice)

-- | The volume size, in GiB. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
ebsBlockDevice_volumeSize :: Lens.Lens' EbsBlockDevice (Prelude.Maybe Prelude.Int)
ebsBlockDevice_volumeSize = Lens.lens (\EbsBlockDevice' {volumeSize} -> volumeSize) (\s@EbsBlockDevice' {} a -> s {volumeSize = a} :: EbsBlockDevice)

-- | The volume type. @gp2@ for General Purpose (SSD) volumes, @io1@ for
-- Provisioned IOPS (SSD) volumes, @st1@ for Throughput Optimized hard disk
-- drives (HDD), @sc1@ for Cold HDD,and @standard@ for Magnetic volumes.
--
-- If you specify the @io1@ volume type, you must also specify a value for
-- the @Iops@ attribute. The maximum ratio of provisioned IOPS to requested
-- volume size (in GiB) is 50:1. AWS uses the default volume size (in GiB)
-- specified in the AMI attributes to set IOPS to 50 x (volume size).
ebsBlockDevice_volumeType :: Lens.Lens' EbsBlockDevice (Prelude.Maybe VolumeType)
ebsBlockDevice_volumeType = Lens.lens (\EbsBlockDevice' {volumeType} -> volumeType) (\s@EbsBlockDevice' {} a -> s {volumeType = a} :: EbsBlockDevice)

instance Data.FromJSON EbsBlockDevice where
  parseJSON =
    Data.withObject
      "EbsBlockDevice"
      ( \x ->
          EbsBlockDevice'
            Prelude.<$> (x Data..:? "DeleteOnTermination")
            Prelude.<*> (x Data..:? "Iops")
            Prelude.<*> (x Data..:? "SnapshotId")
            Prelude.<*> (x Data..:? "VolumeSize")
            Prelude.<*> (x Data..:? "VolumeType")
      )

instance Prelude.Hashable EbsBlockDevice where
  hashWithSalt _salt EbsBlockDevice' {..} =
    _salt
      `Prelude.hashWithSalt` deleteOnTermination
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` volumeSize
      `Prelude.hashWithSalt` volumeType

instance Prelude.NFData EbsBlockDevice where
  rnf EbsBlockDevice' {..} =
    Prelude.rnf deleteOnTermination `Prelude.seq`
      Prelude.rnf iops `Prelude.seq`
        Prelude.rnf snapshotId `Prelude.seq`
          Prelude.rnf volumeSize `Prelude.seq`
            Prelude.rnf volumeType

instance Data.ToJSON EbsBlockDevice where
  toJSON EbsBlockDevice' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeleteOnTermination" Data..=)
              Prelude.<$> deleteOnTermination,
            ("Iops" Data..=) Prelude.<$> iops,
            ("SnapshotId" Data..=) Prelude.<$> snapshotId,
            ("VolumeSize" Data..=) Prelude.<$> volumeSize,
            ("VolumeType" Data..=) Prelude.<$> volumeType
          ]
      )
