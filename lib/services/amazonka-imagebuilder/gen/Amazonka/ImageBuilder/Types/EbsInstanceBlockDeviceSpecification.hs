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
-- Module      : Amazonka.ImageBuilder.Types.EbsInstanceBlockDeviceSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.EbsInstanceBlockDeviceSpecification where

import qualified Amazonka.Core as Core
import Amazonka.ImageBuilder.Types.EbsVolumeType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Amazon EBS-specific block device mapping specifications.
--
-- /See:/ 'newEbsInstanceBlockDeviceSpecification' smart constructor.
data EbsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification'
  { -- | Use to configure delete on termination of the associated device.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | __For GP3 volumes only__ – The throughput in MiB\/s that the volume
    -- supports.
    throughput :: Prelude.Maybe Prelude.Natural,
    -- | Use to override the device\'s volume size.
    volumeSize :: Prelude.Maybe Prelude.Natural,
    -- | Use to configure device IOPS.
    iops :: Prelude.Maybe Prelude.Natural,
    -- | Use to configure device encryption.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | Use to configure the KMS key to use when encrypting the device.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Use to override the device\'s volume type.
    volumeType :: Prelude.Maybe EbsVolumeType,
    -- | The snapshot that defines the device contents.
    snapshotId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EbsInstanceBlockDeviceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteOnTermination', 'ebsInstanceBlockDeviceSpecification_deleteOnTermination' - Use to configure delete on termination of the associated device.
--
-- 'throughput', 'ebsInstanceBlockDeviceSpecification_throughput' - __For GP3 volumes only__ – The throughput in MiB\/s that the volume
-- supports.
--
-- 'volumeSize', 'ebsInstanceBlockDeviceSpecification_volumeSize' - Use to override the device\'s volume size.
--
-- 'iops', 'ebsInstanceBlockDeviceSpecification_iops' - Use to configure device IOPS.
--
-- 'encrypted', 'ebsInstanceBlockDeviceSpecification_encrypted' - Use to configure device encryption.
--
-- 'kmsKeyId', 'ebsInstanceBlockDeviceSpecification_kmsKeyId' - Use to configure the KMS key to use when encrypting the device.
--
-- 'volumeType', 'ebsInstanceBlockDeviceSpecification_volumeType' - Use to override the device\'s volume type.
--
-- 'snapshotId', 'ebsInstanceBlockDeviceSpecification_snapshotId' - The snapshot that defines the device contents.
newEbsInstanceBlockDeviceSpecification ::
  EbsInstanceBlockDeviceSpecification
newEbsInstanceBlockDeviceSpecification =
  EbsInstanceBlockDeviceSpecification'
    { deleteOnTermination =
        Prelude.Nothing,
      throughput = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      iops = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      volumeType = Prelude.Nothing,
      snapshotId = Prelude.Nothing
    }

-- | Use to configure delete on termination of the associated device.
ebsInstanceBlockDeviceSpecification_deleteOnTermination :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Bool)
ebsInstanceBlockDeviceSpecification_deleteOnTermination = Lens.lens (\EbsInstanceBlockDeviceSpecification' {deleteOnTermination} -> deleteOnTermination) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {deleteOnTermination = a} :: EbsInstanceBlockDeviceSpecification)

-- | __For GP3 volumes only__ – The throughput in MiB\/s that the volume
-- supports.
ebsInstanceBlockDeviceSpecification_throughput :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Natural)
ebsInstanceBlockDeviceSpecification_throughput = Lens.lens (\EbsInstanceBlockDeviceSpecification' {throughput} -> throughput) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {throughput = a} :: EbsInstanceBlockDeviceSpecification)

-- | Use to override the device\'s volume size.
ebsInstanceBlockDeviceSpecification_volumeSize :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Natural)
ebsInstanceBlockDeviceSpecification_volumeSize = Lens.lens (\EbsInstanceBlockDeviceSpecification' {volumeSize} -> volumeSize) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {volumeSize = a} :: EbsInstanceBlockDeviceSpecification)

-- | Use to configure device IOPS.
ebsInstanceBlockDeviceSpecification_iops :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Natural)
ebsInstanceBlockDeviceSpecification_iops = Lens.lens (\EbsInstanceBlockDeviceSpecification' {iops} -> iops) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {iops = a} :: EbsInstanceBlockDeviceSpecification)

-- | Use to configure device encryption.
ebsInstanceBlockDeviceSpecification_encrypted :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Bool)
ebsInstanceBlockDeviceSpecification_encrypted = Lens.lens (\EbsInstanceBlockDeviceSpecification' {encrypted} -> encrypted) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {encrypted = a} :: EbsInstanceBlockDeviceSpecification)

-- | Use to configure the KMS key to use when encrypting the device.
ebsInstanceBlockDeviceSpecification_kmsKeyId :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Text)
ebsInstanceBlockDeviceSpecification_kmsKeyId = Lens.lens (\EbsInstanceBlockDeviceSpecification' {kmsKeyId} -> kmsKeyId) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {kmsKeyId = a} :: EbsInstanceBlockDeviceSpecification)

-- | Use to override the device\'s volume type.
ebsInstanceBlockDeviceSpecification_volumeType :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe EbsVolumeType)
ebsInstanceBlockDeviceSpecification_volumeType = Lens.lens (\EbsInstanceBlockDeviceSpecification' {volumeType} -> volumeType) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {volumeType = a} :: EbsInstanceBlockDeviceSpecification)

-- | The snapshot that defines the device contents.
ebsInstanceBlockDeviceSpecification_snapshotId :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Text)
ebsInstanceBlockDeviceSpecification_snapshotId = Lens.lens (\EbsInstanceBlockDeviceSpecification' {snapshotId} -> snapshotId) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {snapshotId = a} :: EbsInstanceBlockDeviceSpecification)

instance
  Core.FromJSON
    EbsInstanceBlockDeviceSpecification
  where
  parseJSON =
    Core.withObject
      "EbsInstanceBlockDeviceSpecification"
      ( \x ->
          EbsInstanceBlockDeviceSpecification'
            Prelude.<$> (x Core..:? "deleteOnTermination")
            Prelude.<*> (x Core..:? "throughput")
            Prelude.<*> (x Core..:? "volumeSize")
            Prelude.<*> (x Core..:? "iops")
            Prelude.<*> (x Core..:? "encrypted")
            Prelude.<*> (x Core..:? "kmsKeyId")
            Prelude.<*> (x Core..:? "volumeType")
            Prelude.<*> (x Core..:? "snapshotId")
      )

instance
  Prelude.Hashable
    EbsInstanceBlockDeviceSpecification
  where
  hashWithSalt
    salt'
    EbsInstanceBlockDeviceSpecification' {..} =
      salt' `Prelude.hashWithSalt` snapshotId
        `Prelude.hashWithSalt` volumeType
        `Prelude.hashWithSalt` kmsKeyId
        `Prelude.hashWithSalt` encrypted
        `Prelude.hashWithSalt` iops
        `Prelude.hashWithSalt` volumeSize
        `Prelude.hashWithSalt` throughput
        `Prelude.hashWithSalt` deleteOnTermination

instance
  Prelude.NFData
    EbsInstanceBlockDeviceSpecification
  where
  rnf EbsInstanceBlockDeviceSpecification' {..} =
    Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf volumeType
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf volumeSize
      `Prelude.seq` Prelude.rnf throughput

instance
  Core.ToJSON
    EbsInstanceBlockDeviceSpecification
  where
  toJSON EbsInstanceBlockDeviceSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("deleteOnTermination" Core..=)
              Prelude.<$> deleteOnTermination,
            ("throughput" Core..=) Prelude.<$> throughput,
            ("volumeSize" Core..=) Prelude.<$> volumeSize,
            ("iops" Core..=) Prelude.<$> iops,
            ("encrypted" Core..=) Prelude.<$> encrypted,
            ("kmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("volumeType" Core..=) Prelude.<$> volumeType,
            ("snapshotId" Core..=) Prelude.<$> snapshotId
          ]
      )
