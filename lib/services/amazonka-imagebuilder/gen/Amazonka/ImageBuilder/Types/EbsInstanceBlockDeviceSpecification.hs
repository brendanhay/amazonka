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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.EbsInstanceBlockDeviceSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.EbsVolumeType
import qualified Amazonka.Prelude as Prelude

-- | Amazon EBS-specific block device mapping specifications.
--
-- /See:/ 'newEbsInstanceBlockDeviceSpecification' smart constructor.
data EbsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification'
  { -- | Use to configure delete on termination of the associated device.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | Use to configure device encryption.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | Use to configure device IOPS.
    iops :: Prelude.Maybe Prelude.Natural,
    -- | Use to configure the KMS key to use when encrypting the device.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The snapshot that defines the device contents.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | __For GP3 volumes only__ – The throughput in MiB\/s that the volume
    -- supports.
    throughput :: Prelude.Maybe Prelude.Natural,
    -- | Use to override the device\'s volume size.
    volumeSize :: Prelude.Maybe Prelude.Natural,
    -- | Use to override the device\'s volume type.
    volumeType :: Prelude.Maybe EbsVolumeType
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
-- 'encrypted', 'ebsInstanceBlockDeviceSpecification_encrypted' - Use to configure device encryption.
--
-- 'iops', 'ebsInstanceBlockDeviceSpecification_iops' - Use to configure device IOPS.
--
-- 'kmsKeyId', 'ebsInstanceBlockDeviceSpecification_kmsKeyId' - Use to configure the KMS key to use when encrypting the device.
--
-- 'snapshotId', 'ebsInstanceBlockDeviceSpecification_snapshotId' - The snapshot that defines the device contents.
--
-- 'throughput', 'ebsInstanceBlockDeviceSpecification_throughput' - __For GP3 volumes only__ – The throughput in MiB\/s that the volume
-- supports.
--
-- 'volumeSize', 'ebsInstanceBlockDeviceSpecification_volumeSize' - Use to override the device\'s volume size.
--
-- 'volumeType', 'ebsInstanceBlockDeviceSpecification_volumeType' - Use to override the device\'s volume type.
newEbsInstanceBlockDeviceSpecification ::
  EbsInstanceBlockDeviceSpecification
newEbsInstanceBlockDeviceSpecification =
  EbsInstanceBlockDeviceSpecification'
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

-- | Use to configure delete on termination of the associated device.
ebsInstanceBlockDeviceSpecification_deleteOnTermination :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Bool)
ebsInstanceBlockDeviceSpecification_deleteOnTermination = Lens.lens (\EbsInstanceBlockDeviceSpecification' {deleteOnTermination} -> deleteOnTermination) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {deleteOnTermination = a} :: EbsInstanceBlockDeviceSpecification)

-- | Use to configure device encryption.
ebsInstanceBlockDeviceSpecification_encrypted :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Bool)
ebsInstanceBlockDeviceSpecification_encrypted = Lens.lens (\EbsInstanceBlockDeviceSpecification' {encrypted} -> encrypted) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {encrypted = a} :: EbsInstanceBlockDeviceSpecification)

-- | Use to configure device IOPS.
ebsInstanceBlockDeviceSpecification_iops :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Natural)
ebsInstanceBlockDeviceSpecification_iops = Lens.lens (\EbsInstanceBlockDeviceSpecification' {iops} -> iops) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {iops = a} :: EbsInstanceBlockDeviceSpecification)

-- | Use to configure the KMS key to use when encrypting the device.
ebsInstanceBlockDeviceSpecification_kmsKeyId :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Text)
ebsInstanceBlockDeviceSpecification_kmsKeyId = Lens.lens (\EbsInstanceBlockDeviceSpecification' {kmsKeyId} -> kmsKeyId) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {kmsKeyId = a} :: EbsInstanceBlockDeviceSpecification)

-- | The snapshot that defines the device contents.
ebsInstanceBlockDeviceSpecification_snapshotId :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Text)
ebsInstanceBlockDeviceSpecification_snapshotId = Lens.lens (\EbsInstanceBlockDeviceSpecification' {snapshotId} -> snapshotId) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {snapshotId = a} :: EbsInstanceBlockDeviceSpecification)

-- | __For GP3 volumes only__ – The throughput in MiB\/s that the volume
-- supports.
ebsInstanceBlockDeviceSpecification_throughput :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Natural)
ebsInstanceBlockDeviceSpecification_throughput = Lens.lens (\EbsInstanceBlockDeviceSpecification' {throughput} -> throughput) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {throughput = a} :: EbsInstanceBlockDeviceSpecification)

-- | Use to override the device\'s volume size.
ebsInstanceBlockDeviceSpecification_volumeSize :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Natural)
ebsInstanceBlockDeviceSpecification_volumeSize = Lens.lens (\EbsInstanceBlockDeviceSpecification' {volumeSize} -> volumeSize) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {volumeSize = a} :: EbsInstanceBlockDeviceSpecification)

-- | Use to override the device\'s volume type.
ebsInstanceBlockDeviceSpecification_volumeType :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe EbsVolumeType)
ebsInstanceBlockDeviceSpecification_volumeType = Lens.lens (\EbsInstanceBlockDeviceSpecification' {volumeType} -> volumeType) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {volumeType = a} :: EbsInstanceBlockDeviceSpecification)

instance
  Data.FromJSON
    EbsInstanceBlockDeviceSpecification
  where
  parseJSON =
    Data.withObject
      "EbsInstanceBlockDeviceSpecification"
      ( \x ->
          EbsInstanceBlockDeviceSpecification'
            Prelude.<$> (x Data..:? "deleteOnTermination")
            Prelude.<*> (x Data..:? "encrypted")
            Prelude.<*> (x Data..:? "iops")
            Prelude.<*> (x Data..:? "kmsKeyId")
            Prelude.<*> (x Data..:? "snapshotId")
            Prelude.<*> (x Data..:? "throughput")
            Prelude.<*> (x Data..:? "volumeSize")
            Prelude.<*> (x Data..:? "volumeType")
      )

instance
  Prelude.Hashable
    EbsInstanceBlockDeviceSpecification
  where
  hashWithSalt
    _salt
    EbsInstanceBlockDeviceSpecification' {..} =
      _salt `Prelude.hashWithSalt` deleteOnTermination
        `Prelude.hashWithSalt` encrypted
        `Prelude.hashWithSalt` iops
        `Prelude.hashWithSalt` kmsKeyId
        `Prelude.hashWithSalt` snapshotId
        `Prelude.hashWithSalt` throughput
        `Prelude.hashWithSalt` volumeSize
        `Prelude.hashWithSalt` volumeType

instance
  Prelude.NFData
    EbsInstanceBlockDeviceSpecification
  where
  rnf EbsInstanceBlockDeviceSpecification' {..} =
    Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf throughput
      `Prelude.seq` Prelude.rnf volumeSize
      `Prelude.seq` Prelude.rnf volumeType

instance
  Data.ToJSON
    EbsInstanceBlockDeviceSpecification
  where
  toJSON EbsInstanceBlockDeviceSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deleteOnTermination" Data..=)
              Prelude.<$> deleteOnTermination,
            ("encrypted" Data..=) Prelude.<$> encrypted,
            ("iops" Data..=) Prelude.<$> iops,
            ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("snapshotId" Data..=) Prelude.<$> snapshotId,
            ("throughput" Data..=) Prelude.<$> throughput,
            ("volumeSize" Data..=) Prelude.<$> volumeSize,
            ("volumeType" Data..=) Prelude.<$> volumeType
          ]
      )
