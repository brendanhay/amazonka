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
-- Module      : Amazonka.GuardDuty.Types.VolumeDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.VolumeDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains EBS volume details.
--
-- /See:/ 'newVolumeDetail' smart constructor.
data VolumeDetail = VolumeDetail'
  { -- | The device name for the EBS volume.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | EBS volume encryption type.
    encryptionType :: Prelude.Maybe Prelude.Text,
    -- | KMS key Arn used to encrypt the EBS volume.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Snapshot Arn of the EBS volume.
    snapshotArn :: Prelude.Maybe Prelude.Text,
    -- | EBS volume Arn information.
    volumeArn :: Prelude.Maybe Prelude.Text,
    -- | EBS volume size in GB.
    volumeSizeInGB :: Prelude.Maybe Prelude.Int,
    -- | The EBS volume type.
    volumeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceName', 'volumeDetail_deviceName' - The device name for the EBS volume.
--
-- 'encryptionType', 'volumeDetail_encryptionType' - EBS volume encryption type.
--
-- 'kmsKeyArn', 'volumeDetail_kmsKeyArn' - KMS key Arn used to encrypt the EBS volume.
--
-- 'snapshotArn', 'volumeDetail_snapshotArn' - Snapshot Arn of the EBS volume.
--
-- 'volumeArn', 'volumeDetail_volumeArn' - EBS volume Arn information.
--
-- 'volumeSizeInGB', 'volumeDetail_volumeSizeInGB' - EBS volume size in GB.
--
-- 'volumeType', 'volumeDetail_volumeType' - The EBS volume type.
newVolumeDetail ::
  VolumeDetail
newVolumeDetail =
  VolumeDetail'
    { deviceName = Prelude.Nothing,
      encryptionType = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      snapshotArn = Prelude.Nothing,
      volumeArn = Prelude.Nothing,
      volumeSizeInGB = Prelude.Nothing,
      volumeType = Prelude.Nothing
    }

-- | The device name for the EBS volume.
volumeDetail_deviceName :: Lens.Lens' VolumeDetail (Prelude.Maybe Prelude.Text)
volumeDetail_deviceName = Lens.lens (\VolumeDetail' {deviceName} -> deviceName) (\s@VolumeDetail' {} a -> s {deviceName = a} :: VolumeDetail)

-- | EBS volume encryption type.
volumeDetail_encryptionType :: Lens.Lens' VolumeDetail (Prelude.Maybe Prelude.Text)
volumeDetail_encryptionType = Lens.lens (\VolumeDetail' {encryptionType} -> encryptionType) (\s@VolumeDetail' {} a -> s {encryptionType = a} :: VolumeDetail)

-- | KMS key Arn used to encrypt the EBS volume.
volumeDetail_kmsKeyArn :: Lens.Lens' VolumeDetail (Prelude.Maybe Prelude.Text)
volumeDetail_kmsKeyArn = Lens.lens (\VolumeDetail' {kmsKeyArn} -> kmsKeyArn) (\s@VolumeDetail' {} a -> s {kmsKeyArn = a} :: VolumeDetail)

-- | Snapshot Arn of the EBS volume.
volumeDetail_snapshotArn :: Lens.Lens' VolumeDetail (Prelude.Maybe Prelude.Text)
volumeDetail_snapshotArn = Lens.lens (\VolumeDetail' {snapshotArn} -> snapshotArn) (\s@VolumeDetail' {} a -> s {snapshotArn = a} :: VolumeDetail)

-- | EBS volume Arn information.
volumeDetail_volumeArn :: Lens.Lens' VolumeDetail (Prelude.Maybe Prelude.Text)
volumeDetail_volumeArn = Lens.lens (\VolumeDetail' {volumeArn} -> volumeArn) (\s@VolumeDetail' {} a -> s {volumeArn = a} :: VolumeDetail)

-- | EBS volume size in GB.
volumeDetail_volumeSizeInGB :: Lens.Lens' VolumeDetail (Prelude.Maybe Prelude.Int)
volumeDetail_volumeSizeInGB = Lens.lens (\VolumeDetail' {volumeSizeInGB} -> volumeSizeInGB) (\s@VolumeDetail' {} a -> s {volumeSizeInGB = a} :: VolumeDetail)

-- | The EBS volume type.
volumeDetail_volumeType :: Lens.Lens' VolumeDetail (Prelude.Maybe Prelude.Text)
volumeDetail_volumeType = Lens.lens (\VolumeDetail' {volumeType} -> volumeType) (\s@VolumeDetail' {} a -> s {volumeType = a} :: VolumeDetail)

instance Data.FromJSON VolumeDetail where
  parseJSON =
    Data.withObject
      "VolumeDetail"
      ( \x ->
          VolumeDetail'
            Prelude.<$> (x Data..:? "deviceName")
            Prelude.<*> (x Data..:? "encryptionType")
            Prelude.<*> (x Data..:? "kmsKeyArn")
            Prelude.<*> (x Data..:? "snapshotArn")
            Prelude.<*> (x Data..:? "volumeArn")
            Prelude.<*> (x Data..:? "volumeSizeInGB")
            Prelude.<*> (x Data..:? "volumeType")
      )

instance Prelude.Hashable VolumeDetail where
  hashWithSalt _salt VolumeDetail' {..} =
    _salt `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` encryptionType
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` snapshotArn
      `Prelude.hashWithSalt` volumeArn
      `Prelude.hashWithSalt` volumeSizeInGB
      `Prelude.hashWithSalt` volumeType

instance Prelude.NFData VolumeDetail where
  rnf VolumeDetail' {..} =
    Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf encryptionType
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf snapshotArn
      `Prelude.seq` Prelude.rnf volumeArn
      `Prelude.seq` Prelude.rnf volumeSizeInGB
      `Prelude.seq` Prelude.rnf volumeType
