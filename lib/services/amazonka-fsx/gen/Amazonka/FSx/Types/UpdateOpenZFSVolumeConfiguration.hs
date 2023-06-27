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
-- Module      : Amazonka.FSx.Types.UpdateOpenZFSVolumeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.UpdateOpenZFSVolumeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.OpenZFSDataCompressionType
import Amazonka.FSx.Types.OpenZFSNfsExport
import Amazonka.FSx.Types.OpenZFSUserOrGroupQuota
import qualified Amazonka.Prelude as Prelude

-- | Used to specify changes to the OpenZFS configuration for the volume that
-- you are updating.
--
-- /See:/ 'newUpdateOpenZFSVolumeConfiguration' smart constructor.
data UpdateOpenZFSVolumeConfiguration = UpdateOpenZFSVolumeConfiguration'
  { -- | Specifies the method used to compress the data on the volume. The
    -- compression type is @NONE@ by default.
    --
    -- -   @NONE@ - Doesn\'t compress the data on the volume. @NONE@ is the
    --     default.
    --
    -- -   @ZSTD@ - Compresses the data in the volume using the Zstandard
    --     (ZSTD) compression algorithm. Compared to LZ4, Z-Standard provides a
    --     better compression ratio to minimize on-disk storage utilization.
    --
    -- -   @LZ4@ - Compresses the data in the volume using the LZ4 compression
    --     algorithm. Compared to Z-Standard, LZ4 is less compute-intensive and
    --     delivers higher write throughput speeds.
    dataCompressionType :: Prelude.Maybe OpenZFSDataCompressionType,
    -- | The configuration object for mounting a Network File System (NFS) file
    -- system.
    nfsExports :: Prelude.Maybe [OpenZFSNfsExport],
    -- | A Boolean value indicating whether the volume is read-only.
    readOnly :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the record size of an OpenZFS volume, in kibibytes (KiB).
    -- Valid values are 4, 8, 16, 32, 64, 128, 256, 512, or 1024 KiB. The
    -- default is 128 KiB. Most workloads should use the default record size.
    -- Database workflows can benefit from a smaller record size, while
    -- streaming workflows can benefit from a larger record size. For
    -- additional guidance on when to set a custom record size, see
    -- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/performance.html#performance-tips-zfs Tips for maximizing performance>
    -- in the /Amazon FSx for OpenZFS User Guide/.
    recordSizeKiB :: Prelude.Maybe Prelude.Natural,
    -- | The maximum amount of storage in gibibytes (GiB) that the volume can use
    -- from its parent. You can specify a quota larger than the storage on the
    -- parent volume. You can specify a value of @-1@ to unset a volume\'s
    -- storage capacity quota.
    storageCapacityQuotaGiB :: Prelude.Maybe Prelude.Int,
    -- | The amount of storage in gibibytes (GiB) to reserve from the parent
    -- volume. You can\'t reserve more storage than the parent volume has
    -- reserved. You can specify a value of @-1@ to unset a volume\'s storage
    -- capacity reservation.
    storageCapacityReservationGiB :: Prelude.Maybe Prelude.Int,
    -- | An object specifying how much storage users or groups can use on the
    -- volume.
    userAndGroupQuotas :: Prelude.Maybe [OpenZFSUserOrGroupQuota]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOpenZFSVolumeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataCompressionType', 'updateOpenZFSVolumeConfiguration_dataCompressionType' - Specifies the method used to compress the data on the volume. The
-- compression type is @NONE@ by default.
--
-- -   @NONE@ - Doesn\'t compress the data on the volume. @NONE@ is the
--     default.
--
-- -   @ZSTD@ - Compresses the data in the volume using the Zstandard
--     (ZSTD) compression algorithm. Compared to LZ4, Z-Standard provides a
--     better compression ratio to minimize on-disk storage utilization.
--
-- -   @LZ4@ - Compresses the data in the volume using the LZ4 compression
--     algorithm. Compared to Z-Standard, LZ4 is less compute-intensive and
--     delivers higher write throughput speeds.
--
-- 'nfsExports', 'updateOpenZFSVolumeConfiguration_nfsExports' - The configuration object for mounting a Network File System (NFS) file
-- system.
--
-- 'readOnly', 'updateOpenZFSVolumeConfiguration_readOnly' - A Boolean value indicating whether the volume is read-only.
--
-- 'recordSizeKiB', 'updateOpenZFSVolumeConfiguration_recordSizeKiB' - Specifies the record size of an OpenZFS volume, in kibibytes (KiB).
-- Valid values are 4, 8, 16, 32, 64, 128, 256, 512, or 1024 KiB. The
-- default is 128 KiB. Most workloads should use the default record size.
-- Database workflows can benefit from a smaller record size, while
-- streaming workflows can benefit from a larger record size. For
-- additional guidance on when to set a custom record size, see
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/performance.html#performance-tips-zfs Tips for maximizing performance>
-- in the /Amazon FSx for OpenZFS User Guide/.
--
-- 'storageCapacityQuotaGiB', 'updateOpenZFSVolumeConfiguration_storageCapacityQuotaGiB' - The maximum amount of storage in gibibytes (GiB) that the volume can use
-- from its parent. You can specify a quota larger than the storage on the
-- parent volume. You can specify a value of @-1@ to unset a volume\'s
-- storage capacity quota.
--
-- 'storageCapacityReservationGiB', 'updateOpenZFSVolumeConfiguration_storageCapacityReservationGiB' - The amount of storage in gibibytes (GiB) to reserve from the parent
-- volume. You can\'t reserve more storage than the parent volume has
-- reserved. You can specify a value of @-1@ to unset a volume\'s storage
-- capacity reservation.
--
-- 'userAndGroupQuotas', 'updateOpenZFSVolumeConfiguration_userAndGroupQuotas' - An object specifying how much storage users or groups can use on the
-- volume.
newUpdateOpenZFSVolumeConfiguration ::
  UpdateOpenZFSVolumeConfiguration
newUpdateOpenZFSVolumeConfiguration =
  UpdateOpenZFSVolumeConfiguration'
    { dataCompressionType =
        Prelude.Nothing,
      nfsExports = Prelude.Nothing,
      readOnly = Prelude.Nothing,
      recordSizeKiB = Prelude.Nothing,
      storageCapacityQuotaGiB = Prelude.Nothing,
      storageCapacityReservationGiB =
        Prelude.Nothing,
      userAndGroupQuotas = Prelude.Nothing
    }

-- | Specifies the method used to compress the data on the volume. The
-- compression type is @NONE@ by default.
--
-- -   @NONE@ - Doesn\'t compress the data on the volume. @NONE@ is the
--     default.
--
-- -   @ZSTD@ - Compresses the data in the volume using the Zstandard
--     (ZSTD) compression algorithm. Compared to LZ4, Z-Standard provides a
--     better compression ratio to minimize on-disk storage utilization.
--
-- -   @LZ4@ - Compresses the data in the volume using the LZ4 compression
--     algorithm. Compared to Z-Standard, LZ4 is less compute-intensive and
--     delivers higher write throughput speeds.
updateOpenZFSVolumeConfiguration_dataCompressionType :: Lens.Lens' UpdateOpenZFSVolumeConfiguration (Prelude.Maybe OpenZFSDataCompressionType)
updateOpenZFSVolumeConfiguration_dataCompressionType = Lens.lens (\UpdateOpenZFSVolumeConfiguration' {dataCompressionType} -> dataCompressionType) (\s@UpdateOpenZFSVolumeConfiguration' {} a -> s {dataCompressionType = a} :: UpdateOpenZFSVolumeConfiguration)

-- | The configuration object for mounting a Network File System (NFS) file
-- system.
updateOpenZFSVolumeConfiguration_nfsExports :: Lens.Lens' UpdateOpenZFSVolumeConfiguration (Prelude.Maybe [OpenZFSNfsExport])
updateOpenZFSVolumeConfiguration_nfsExports = Lens.lens (\UpdateOpenZFSVolumeConfiguration' {nfsExports} -> nfsExports) (\s@UpdateOpenZFSVolumeConfiguration' {} a -> s {nfsExports = a} :: UpdateOpenZFSVolumeConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean value indicating whether the volume is read-only.
updateOpenZFSVolumeConfiguration_readOnly :: Lens.Lens' UpdateOpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Bool)
updateOpenZFSVolumeConfiguration_readOnly = Lens.lens (\UpdateOpenZFSVolumeConfiguration' {readOnly} -> readOnly) (\s@UpdateOpenZFSVolumeConfiguration' {} a -> s {readOnly = a} :: UpdateOpenZFSVolumeConfiguration)

-- | Specifies the record size of an OpenZFS volume, in kibibytes (KiB).
-- Valid values are 4, 8, 16, 32, 64, 128, 256, 512, or 1024 KiB. The
-- default is 128 KiB. Most workloads should use the default record size.
-- Database workflows can benefit from a smaller record size, while
-- streaming workflows can benefit from a larger record size. For
-- additional guidance on when to set a custom record size, see
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/performance.html#performance-tips-zfs Tips for maximizing performance>
-- in the /Amazon FSx for OpenZFS User Guide/.
updateOpenZFSVolumeConfiguration_recordSizeKiB :: Lens.Lens' UpdateOpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Natural)
updateOpenZFSVolumeConfiguration_recordSizeKiB = Lens.lens (\UpdateOpenZFSVolumeConfiguration' {recordSizeKiB} -> recordSizeKiB) (\s@UpdateOpenZFSVolumeConfiguration' {} a -> s {recordSizeKiB = a} :: UpdateOpenZFSVolumeConfiguration)

-- | The maximum amount of storage in gibibytes (GiB) that the volume can use
-- from its parent. You can specify a quota larger than the storage on the
-- parent volume. You can specify a value of @-1@ to unset a volume\'s
-- storage capacity quota.
updateOpenZFSVolumeConfiguration_storageCapacityQuotaGiB :: Lens.Lens' UpdateOpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Int)
updateOpenZFSVolumeConfiguration_storageCapacityQuotaGiB = Lens.lens (\UpdateOpenZFSVolumeConfiguration' {storageCapacityQuotaGiB} -> storageCapacityQuotaGiB) (\s@UpdateOpenZFSVolumeConfiguration' {} a -> s {storageCapacityQuotaGiB = a} :: UpdateOpenZFSVolumeConfiguration)

-- | The amount of storage in gibibytes (GiB) to reserve from the parent
-- volume. You can\'t reserve more storage than the parent volume has
-- reserved. You can specify a value of @-1@ to unset a volume\'s storage
-- capacity reservation.
updateOpenZFSVolumeConfiguration_storageCapacityReservationGiB :: Lens.Lens' UpdateOpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Int)
updateOpenZFSVolumeConfiguration_storageCapacityReservationGiB = Lens.lens (\UpdateOpenZFSVolumeConfiguration' {storageCapacityReservationGiB} -> storageCapacityReservationGiB) (\s@UpdateOpenZFSVolumeConfiguration' {} a -> s {storageCapacityReservationGiB = a} :: UpdateOpenZFSVolumeConfiguration)

-- | An object specifying how much storage users or groups can use on the
-- volume.
updateOpenZFSVolumeConfiguration_userAndGroupQuotas :: Lens.Lens' UpdateOpenZFSVolumeConfiguration (Prelude.Maybe [OpenZFSUserOrGroupQuota])
updateOpenZFSVolumeConfiguration_userAndGroupQuotas = Lens.lens (\UpdateOpenZFSVolumeConfiguration' {userAndGroupQuotas} -> userAndGroupQuotas) (\s@UpdateOpenZFSVolumeConfiguration' {} a -> s {userAndGroupQuotas = a} :: UpdateOpenZFSVolumeConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    UpdateOpenZFSVolumeConfiguration
  where
  hashWithSalt
    _salt
    UpdateOpenZFSVolumeConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` dataCompressionType
        `Prelude.hashWithSalt` nfsExports
        `Prelude.hashWithSalt` readOnly
        `Prelude.hashWithSalt` recordSizeKiB
        `Prelude.hashWithSalt` storageCapacityQuotaGiB
        `Prelude.hashWithSalt` storageCapacityReservationGiB
        `Prelude.hashWithSalt` userAndGroupQuotas

instance
  Prelude.NFData
    UpdateOpenZFSVolumeConfiguration
  where
  rnf UpdateOpenZFSVolumeConfiguration' {..} =
    Prelude.rnf dataCompressionType
      `Prelude.seq` Prelude.rnf nfsExports
      `Prelude.seq` Prelude.rnf readOnly
      `Prelude.seq` Prelude.rnf recordSizeKiB
      `Prelude.seq` Prelude.rnf storageCapacityQuotaGiB
      `Prelude.seq` Prelude.rnf storageCapacityReservationGiB
      `Prelude.seq` Prelude.rnf userAndGroupQuotas

instance Data.ToJSON UpdateOpenZFSVolumeConfiguration where
  toJSON UpdateOpenZFSVolumeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataCompressionType" Data..=)
              Prelude.<$> dataCompressionType,
            ("NfsExports" Data..=) Prelude.<$> nfsExports,
            ("ReadOnly" Data..=) Prelude.<$> readOnly,
            ("RecordSizeKiB" Data..=) Prelude.<$> recordSizeKiB,
            ("StorageCapacityQuotaGiB" Data..=)
              Prelude.<$> storageCapacityQuotaGiB,
            ("StorageCapacityReservationGiB" Data..=)
              Prelude.<$> storageCapacityReservationGiB,
            ("UserAndGroupQuotas" Data..=)
              Prelude.<$> userAndGroupQuotas
          ]
      )
