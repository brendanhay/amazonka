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
-- Module      : Amazonka.EC2.Types.LaunchTemplateEbsBlockDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateEbsBlockDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VolumeType
import qualified Amazonka.Prelude as Prelude

-- | Describes a block device for an EBS volume.
--
-- /See:/ 'newLaunchTemplateEbsBlockDevice' smart constructor.
data LaunchTemplateEbsBlockDevice = LaunchTemplateEbsBlockDevice'
  { -- | Indicates whether the EBS volume is deleted on instance termination.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the EBS volume is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The number of I\/O operations per second (IOPS) that the volume
    -- supports.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the Key Management Service (KMS) CMK used for encryption.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The throughput that the volume supports, in MiB\/s.
    throughput :: Prelude.Maybe Prelude.Int,
    -- | The size of the volume, in GiB.
    volumeSize :: Prelude.Maybe Prelude.Int,
    -- | The volume type.
    volumeType :: Prelude.Maybe VolumeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateEbsBlockDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteOnTermination', 'launchTemplateEbsBlockDevice_deleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination.
--
-- 'encrypted', 'launchTemplateEbsBlockDevice_encrypted' - Indicates whether the EBS volume is encrypted.
--
-- 'iops', 'launchTemplateEbsBlockDevice_iops' - The number of I\/O operations per second (IOPS) that the volume
-- supports.
--
-- 'kmsKeyId', 'launchTemplateEbsBlockDevice_kmsKeyId' - The ARN of the Key Management Service (KMS) CMK used for encryption.
--
-- 'snapshotId', 'launchTemplateEbsBlockDevice_snapshotId' - The ID of the snapshot.
--
-- 'throughput', 'launchTemplateEbsBlockDevice_throughput' - The throughput that the volume supports, in MiB\/s.
--
-- 'volumeSize', 'launchTemplateEbsBlockDevice_volumeSize' - The size of the volume, in GiB.
--
-- 'volumeType', 'launchTemplateEbsBlockDevice_volumeType' - The volume type.
newLaunchTemplateEbsBlockDevice ::
  LaunchTemplateEbsBlockDevice
newLaunchTemplateEbsBlockDevice =
  LaunchTemplateEbsBlockDevice'
    { deleteOnTermination =
        Prelude.Nothing,
      encrypted = Prelude.Nothing,
      iops = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      throughput = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      volumeType = Prelude.Nothing
    }

-- | Indicates whether the EBS volume is deleted on instance termination.
launchTemplateEbsBlockDevice_deleteOnTermination :: Lens.Lens' LaunchTemplateEbsBlockDevice (Prelude.Maybe Prelude.Bool)
launchTemplateEbsBlockDevice_deleteOnTermination = Lens.lens (\LaunchTemplateEbsBlockDevice' {deleteOnTermination} -> deleteOnTermination) (\s@LaunchTemplateEbsBlockDevice' {} a -> s {deleteOnTermination = a} :: LaunchTemplateEbsBlockDevice)

-- | Indicates whether the EBS volume is encrypted.
launchTemplateEbsBlockDevice_encrypted :: Lens.Lens' LaunchTemplateEbsBlockDevice (Prelude.Maybe Prelude.Bool)
launchTemplateEbsBlockDevice_encrypted = Lens.lens (\LaunchTemplateEbsBlockDevice' {encrypted} -> encrypted) (\s@LaunchTemplateEbsBlockDevice' {} a -> s {encrypted = a} :: LaunchTemplateEbsBlockDevice)

-- | The number of I\/O operations per second (IOPS) that the volume
-- supports.
launchTemplateEbsBlockDevice_iops :: Lens.Lens' LaunchTemplateEbsBlockDevice (Prelude.Maybe Prelude.Int)
launchTemplateEbsBlockDevice_iops = Lens.lens (\LaunchTemplateEbsBlockDevice' {iops} -> iops) (\s@LaunchTemplateEbsBlockDevice' {} a -> s {iops = a} :: LaunchTemplateEbsBlockDevice)

-- | The ARN of the Key Management Service (KMS) CMK used for encryption.
launchTemplateEbsBlockDevice_kmsKeyId :: Lens.Lens' LaunchTemplateEbsBlockDevice (Prelude.Maybe Prelude.Text)
launchTemplateEbsBlockDevice_kmsKeyId = Lens.lens (\LaunchTemplateEbsBlockDevice' {kmsKeyId} -> kmsKeyId) (\s@LaunchTemplateEbsBlockDevice' {} a -> s {kmsKeyId = a} :: LaunchTemplateEbsBlockDevice)

-- | The ID of the snapshot.
launchTemplateEbsBlockDevice_snapshotId :: Lens.Lens' LaunchTemplateEbsBlockDevice (Prelude.Maybe Prelude.Text)
launchTemplateEbsBlockDevice_snapshotId = Lens.lens (\LaunchTemplateEbsBlockDevice' {snapshotId} -> snapshotId) (\s@LaunchTemplateEbsBlockDevice' {} a -> s {snapshotId = a} :: LaunchTemplateEbsBlockDevice)

-- | The throughput that the volume supports, in MiB\/s.
launchTemplateEbsBlockDevice_throughput :: Lens.Lens' LaunchTemplateEbsBlockDevice (Prelude.Maybe Prelude.Int)
launchTemplateEbsBlockDevice_throughput = Lens.lens (\LaunchTemplateEbsBlockDevice' {throughput} -> throughput) (\s@LaunchTemplateEbsBlockDevice' {} a -> s {throughput = a} :: LaunchTemplateEbsBlockDevice)

-- | The size of the volume, in GiB.
launchTemplateEbsBlockDevice_volumeSize :: Lens.Lens' LaunchTemplateEbsBlockDevice (Prelude.Maybe Prelude.Int)
launchTemplateEbsBlockDevice_volumeSize = Lens.lens (\LaunchTemplateEbsBlockDevice' {volumeSize} -> volumeSize) (\s@LaunchTemplateEbsBlockDevice' {} a -> s {volumeSize = a} :: LaunchTemplateEbsBlockDevice)

-- | The volume type.
launchTemplateEbsBlockDevice_volumeType :: Lens.Lens' LaunchTemplateEbsBlockDevice (Prelude.Maybe VolumeType)
launchTemplateEbsBlockDevice_volumeType = Lens.lens (\LaunchTemplateEbsBlockDevice' {volumeType} -> volumeType) (\s@LaunchTemplateEbsBlockDevice' {} a -> s {volumeType = a} :: LaunchTemplateEbsBlockDevice)

instance Data.FromXML LaunchTemplateEbsBlockDevice where
  parseXML x =
    LaunchTemplateEbsBlockDevice'
      Prelude.<$> (x Data..@? "deleteOnTermination")
      Prelude.<*> (x Data..@? "encrypted")
      Prelude.<*> (x Data..@? "iops")
      Prelude.<*> (x Data..@? "kmsKeyId")
      Prelude.<*> (x Data..@? "snapshotId")
      Prelude.<*> (x Data..@? "throughput")
      Prelude.<*> (x Data..@? "volumeSize")
      Prelude.<*> (x Data..@? "volumeType")

instance
  Prelude.Hashable
    LaunchTemplateEbsBlockDevice
  where
  hashWithSalt _salt LaunchTemplateEbsBlockDevice' {..} =
    _salt `Prelude.hashWithSalt` deleteOnTermination
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` throughput
      `Prelude.hashWithSalt` volumeSize
      `Prelude.hashWithSalt` volumeType

instance Prelude.NFData LaunchTemplateEbsBlockDevice where
  rnf LaunchTemplateEbsBlockDevice' {..} =
    Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf throughput
      `Prelude.seq` Prelude.rnf volumeSize
      `Prelude.seq` Prelude.rnf volumeType
