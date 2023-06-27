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
-- Module      : Amazonka.FSx.Types.CreateOpenZFSVolumeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.CreateOpenZFSVolumeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.CreateOpenZFSOriginSnapshotConfiguration
import Amazonka.FSx.Types.OpenZFSDataCompressionType
import Amazonka.FSx.Types.OpenZFSNfsExport
import Amazonka.FSx.Types.OpenZFSUserOrGroupQuota
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration of the Amazon FSx for OpenZFS volume that
-- you are creating.
--
-- /See:/ 'newCreateOpenZFSVolumeConfiguration' smart constructor.
data CreateOpenZFSVolumeConfiguration = CreateOpenZFSVolumeConfiguration'
  { -- | A Boolean value indicating whether tags for the volume should be copied
    -- to snapshots. This value defaults to @false@. If it\'s set to @true@,
    -- all tags for the volume are copied to snapshots where the user doesn\'t
    -- specify tags. If this value is @true@, and you specify one or more tags,
    -- only the specified tags are copied to snapshots. If you specify one or
    -- more tags when creating the snapshot, no tags are copied from the
    -- volume, regardless of this value.
    copyTagsToSnapshots :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the method used to compress the data on the volume. The
    -- compression type is @NONE@ by default.
    --
    -- -   @NONE@ - Doesn\'t compress the data on the volume. @NONE@ is the
    --     default.
    --
    -- -   @ZSTD@ - Compresses the data in the volume using the Zstandard
    --     (ZSTD) compression algorithm. ZSTD compression provides a higher
    --     level of data compression and higher read throughput performance
    --     than LZ4 compression.
    --
    -- -   @LZ4@ - Compresses the data in the volume using the LZ4 compression
    --     algorithm. LZ4 compression provides a lower level of compression and
    --     higher write throughput performance than ZSTD compression.
    --
    -- For more information about volume compression types and the performance
    -- of your Amazon FSx for OpenZFS file system, see
    -- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/performance.html#performance-tips-zfs Tips for maximizing performance>
    -- File system and volume settings in the /Amazon FSx for OpenZFS User
    -- Guide/.
    dataCompressionType :: Prelude.Maybe OpenZFSDataCompressionType,
    -- | The configuration object for mounting a Network File System (NFS) file
    -- system.
    nfsExports :: Prelude.Maybe [OpenZFSNfsExport],
    -- | The configuration object that specifies the snapshot to use as the
    -- origin of the data for the volume.
    originSnapshot :: Prelude.Maybe CreateOpenZFSOriginSnapshotConfiguration,
    -- | A Boolean value indicating whether the volume is read-only.
    readOnly :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the suggested block size for a volume in a ZFS dataset, in
    -- kibibytes (KiB). Valid values are 4, 8, 16, 32, 64, 128, 256, 512, or
    -- 1024 KiB. The default is 128 KiB. We recommend using the default setting
    -- for the majority of use cases. Generally, workloads that write in fixed
    -- small or large record sizes may benefit from setting a custom record
    -- size, like database workloads (small record size) or media streaming
    -- workloads (large record size). For additional guidance on when to set a
    -- custom record size, see
    -- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/performance.html#record-size-performance ZFS Record size>
    -- in the /Amazon FSx for OpenZFS User Guide/.
    recordSizeKiB :: Prelude.Maybe Prelude.Natural,
    -- | Sets the maximum storage size in gibibytes (GiB) for the volume. You can
    -- specify a quota that is larger than the storage on the parent volume. A
    -- volume quota limits the amount of storage that the volume can consume to
    -- the configured amount, but does not guarantee the space will be
    -- available on the parent volume. To guarantee quota space, you must also
    -- set @StorageCapacityReservationGiB@. To /not/ specify a storage capacity
    -- quota, set this to @-1@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/managing-volumes.html#volume-properties Volume properties>
    -- in the /Amazon FSx for OpenZFS User Guide/.
    storageCapacityQuotaGiB :: Prelude.Maybe Prelude.Int,
    -- | Specifies the amount of storage in gibibytes (GiB) to reserve from the
    -- parent volume. Setting @StorageCapacityReservationGiB@ guarantees that
    -- the specified amount of storage space on the parent volume will always
    -- be available for the volume. You can\'t reserve more storage than the
    -- parent volume has. To /not/ specify a storage capacity reservation, set
    -- this to @0@ or @-1@. For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/managing-volumes.html#volume-properties Volume properties>
    -- in the /Amazon FSx for OpenZFS User Guide/.
    storageCapacityReservationGiB :: Prelude.Maybe Prelude.Int,
    -- | An object specifying how much storage users or groups can use on the
    -- volume.
    userAndGroupQuotas :: Prelude.Maybe [OpenZFSUserOrGroupQuota],
    -- | The ID of the volume to use as the parent volume of the volume that you
    -- are creating.
    parentVolumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOpenZFSVolumeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyTagsToSnapshots', 'createOpenZFSVolumeConfiguration_copyTagsToSnapshots' - A Boolean value indicating whether tags for the volume should be copied
-- to snapshots. This value defaults to @false@. If it\'s set to @true@,
-- all tags for the volume are copied to snapshots where the user doesn\'t
-- specify tags. If this value is @true@, and you specify one or more tags,
-- only the specified tags are copied to snapshots. If you specify one or
-- more tags when creating the snapshot, no tags are copied from the
-- volume, regardless of this value.
--
-- 'dataCompressionType', 'createOpenZFSVolumeConfiguration_dataCompressionType' - Specifies the method used to compress the data on the volume. The
-- compression type is @NONE@ by default.
--
-- -   @NONE@ - Doesn\'t compress the data on the volume. @NONE@ is the
--     default.
--
-- -   @ZSTD@ - Compresses the data in the volume using the Zstandard
--     (ZSTD) compression algorithm. ZSTD compression provides a higher
--     level of data compression and higher read throughput performance
--     than LZ4 compression.
--
-- -   @LZ4@ - Compresses the data in the volume using the LZ4 compression
--     algorithm. LZ4 compression provides a lower level of compression and
--     higher write throughput performance than ZSTD compression.
--
-- For more information about volume compression types and the performance
-- of your Amazon FSx for OpenZFS file system, see
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/performance.html#performance-tips-zfs Tips for maximizing performance>
-- File system and volume settings in the /Amazon FSx for OpenZFS User
-- Guide/.
--
-- 'nfsExports', 'createOpenZFSVolumeConfiguration_nfsExports' - The configuration object for mounting a Network File System (NFS) file
-- system.
--
-- 'originSnapshot', 'createOpenZFSVolumeConfiguration_originSnapshot' - The configuration object that specifies the snapshot to use as the
-- origin of the data for the volume.
--
-- 'readOnly', 'createOpenZFSVolumeConfiguration_readOnly' - A Boolean value indicating whether the volume is read-only.
--
-- 'recordSizeKiB', 'createOpenZFSVolumeConfiguration_recordSizeKiB' - Specifies the suggested block size for a volume in a ZFS dataset, in
-- kibibytes (KiB). Valid values are 4, 8, 16, 32, 64, 128, 256, 512, or
-- 1024 KiB. The default is 128 KiB. We recommend using the default setting
-- for the majority of use cases. Generally, workloads that write in fixed
-- small or large record sizes may benefit from setting a custom record
-- size, like database workloads (small record size) or media streaming
-- workloads (large record size). For additional guidance on when to set a
-- custom record size, see
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/performance.html#record-size-performance ZFS Record size>
-- in the /Amazon FSx for OpenZFS User Guide/.
--
-- 'storageCapacityQuotaGiB', 'createOpenZFSVolumeConfiguration_storageCapacityQuotaGiB' - Sets the maximum storage size in gibibytes (GiB) for the volume. You can
-- specify a quota that is larger than the storage on the parent volume. A
-- volume quota limits the amount of storage that the volume can consume to
-- the configured amount, but does not guarantee the space will be
-- available on the parent volume. To guarantee quota space, you must also
-- set @StorageCapacityReservationGiB@. To /not/ specify a storage capacity
-- quota, set this to @-1@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/managing-volumes.html#volume-properties Volume properties>
-- in the /Amazon FSx for OpenZFS User Guide/.
--
-- 'storageCapacityReservationGiB', 'createOpenZFSVolumeConfiguration_storageCapacityReservationGiB' - Specifies the amount of storage in gibibytes (GiB) to reserve from the
-- parent volume. Setting @StorageCapacityReservationGiB@ guarantees that
-- the specified amount of storage space on the parent volume will always
-- be available for the volume. You can\'t reserve more storage than the
-- parent volume has. To /not/ specify a storage capacity reservation, set
-- this to @0@ or @-1@. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/managing-volumes.html#volume-properties Volume properties>
-- in the /Amazon FSx for OpenZFS User Guide/.
--
-- 'userAndGroupQuotas', 'createOpenZFSVolumeConfiguration_userAndGroupQuotas' - An object specifying how much storage users or groups can use on the
-- volume.
--
-- 'parentVolumeId', 'createOpenZFSVolumeConfiguration_parentVolumeId' - The ID of the volume to use as the parent volume of the volume that you
-- are creating.
newCreateOpenZFSVolumeConfiguration ::
  -- | 'parentVolumeId'
  Prelude.Text ->
  CreateOpenZFSVolumeConfiguration
newCreateOpenZFSVolumeConfiguration pParentVolumeId_ =
  CreateOpenZFSVolumeConfiguration'
    { copyTagsToSnapshots =
        Prelude.Nothing,
      dataCompressionType = Prelude.Nothing,
      nfsExports = Prelude.Nothing,
      originSnapshot = Prelude.Nothing,
      readOnly = Prelude.Nothing,
      recordSizeKiB = Prelude.Nothing,
      storageCapacityQuotaGiB = Prelude.Nothing,
      storageCapacityReservationGiB =
        Prelude.Nothing,
      userAndGroupQuotas = Prelude.Nothing,
      parentVolumeId = pParentVolumeId_
    }

-- | A Boolean value indicating whether tags for the volume should be copied
-- to snapshots. This value defaults to @false@. If it\'s set to @true@,
-- all tags for the volume are copied to snapshots where the user doesn\'t
-- specify tags. If this value is @true@, and you specify one or more tags,
-- only the specified tags are copied to snapshots. If you specify one or
-- more tags when creating the snapshot, no tags are copied from the
-- volume, regardless of this value.
createOpenZFSVolumeConfiguration_copyTagsToSnapshots :: Lens.Lens' CreateOpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Bool)
createOpenZFSVolumeConfiguration_copyTagsToSnapshots = Lens.lens (\CreateOpenZFSVolumeConfiguration' {copyTagsToSnapshots} -> copyTagsToSnapshots) (\s@CreateOpenZFSVolumeConfiguration' {} a -> s {copyTagsToSnapshots = a} :: CreateOpenZFSVolumeConfiguration)

-- | Specifies the method used to compress the data on the volume. The
-- compression type is @NONE@ by default.
--
-- -   @NONE@ - Doesn\'t compress the data on the volume. @NONE@ is the
--     default.
--
-- -   @ZSTD@ - Compresses the data in the volume using the Zstandard
--     (ZSTD) compression algorithm. ZSTD compression provides a higher
--     level of data compression and higher read throughput performance
--     than LZ4 compression.
--
-- -   @LZ4@ - Compresses the data in the volume using the LZ4 compression
--     algorithm. LZ4 compression provides a lower level of compression and
--     higher write throughput performance than ZSTD compression.
--
-- For more information about volume compression types and the performance
-- of your Amazon FSx for OpenZFS file system, see
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/performance.html#performance-tips-zfs Tips for maximizing performance>
-- File system and volume settings in the /Amazon FSx for OpenZFS User
-- Guide/.
createOpenZFSVolumeConfiguration_dataCompressionType :: Lens.Lens' CreateOpenZFSVolumeConfiguration (Prelude.Maybe OpenZFSDataCompressionType)
createOpenZFSVolumeConfiguration_dataCompressionType = Lens.lens (\CreateOpenZFSVolumeConfiguration' {dataCompressionType} -> dataCompressionType) (\s@CreateOpenZFSVolumeConfiguration' {} a -> s {dataCompressionType = a} :: CreateOpenZFSVolumeConfiguration)

-- | The configuration object for mounting a Network File System (NFS) file
-- system.
createOpenZFSVolumeConfiguration_nfsExports :: Lens.Lens' CreateOpenZFSVolumeConfiguration (Prelude.Maybe [OpenZFSNfsExport])
createOpenZFSVolumeConfiguration_nfsExports = Lens.lens (\CreateOpenZFSVolumeConfiguration' {nfsExports} -> nfsExports) (\s@CreateOpenZFSVolumeConfiguration' {} a -> s {nfsExports = a} :: CreateOpenZFSVolumeConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The configuration object that specifies the snapshot to use as the
-- origin of the data for the volume.
createOpenZFSVolumeConfiguration_originSnapshot :: Lens.Lens' CreateOpenZFSVolumeConfiguration (Prelude.Maybe CreateOpenZFSOriginSnapshotConfiguration)
createOpenZFSVolumeConfiguration_originSnapshot = Lens.lens (\CreateOpenZFSVolumeConfiguration' {originSnapshot} -> originSnapshot) (\s@CreateOpenZFSVolumeConfiguration' {} a -> s {originSnapshot = a} :: CreateOpenZFSVolumeConfiguration)

-- | A Boolean value indicating whether the volume is read-only.
createOpenZFSVolumeConfiguration_readOnly :: Lens.Lens' CreateOpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Bool)
createOpenZFSVolumeConfiguration_readOnly = Lens.lens (\CreateOpenZFSVolumeConfiguration' {readOnly} -> readOnly) (\s@CreateOpenZFSVolumeConfiguration' {} a -> s {readOnly = a} :: CreateOpenZFSVolumeConfiguration)

-- | Specifies the suggested block size for a volume in a ZFS dataset, in
-- kibibytes (KiB). Valid values are 4, 8, 16, 32, 64, 128, 256, 512, or
-- 1024 KiB. The default is 128 KiB. We recommend using the default setting
-- for the majority of use cases. Generally, workloads that write in fixed
-- small or large record sizes may benefit from setting a custom record
-- size, like database workloads (small record size) or media streaming
-- workloads (large record size). For additional guidance on when to set a
-- custom record size, see
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/performance.html#record-size-performance ZFS Record size>
-- in the /Amazon FSx for OpenZFS User Guide/.
createOpenZFSVolumeConfiguration_recordSizeKiB :: Lens.Lens' CreateOpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Natural)
createOpenZFSVolumeConfiguration_recordSizeKiB = Lens.lens (\CreateOpenZFSVolumeConfiguration' {recordSizeKiB} -> recordSizeKiB) (\s@CreateOpenZFSVolumeConfiguration' {} a -> s {recordSizeKiB = a} :: CreateOpenZFSVolumeConfiguration)

-- | Sets the maximum storage size in gibibytes (GiB) for the volume. You can
-- specify a quota that is larger than the storage on the parent volume. A
-- volume quota limits the amount of storage that the volume can consume to
-- the configured amount, but does not guarantee the space will be
-- available on the parent volume. To guarantee quota space, you must also
-- set @StorageCapacityReservationGiB@. To /not/ specify a storage capacity
-- quota, set this to @-1@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/managing-volumes.html#volume-properties Volume properties>
-- in the /Amazon FSx for OpenZFS User Guide/.
createOpenZFSVolumeConfiguration_storageCapacityQuotaGiB :: Lens.Lens' CreateOpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Int)
createOpenZFSVolumeConfiguration_storageCapacityQuotaGiB = Lens.lens (\CreateOpenZFSVolumeConfiguration' {storageCapacityQuotaGiB} -> storageCapacityQuotaGiB) (\s@CreateOpenZFSVolumeConfiguration' {} a -> s {storageCapacityQuotaGiB = a} :: CreateOpenZFSVolumeConfiguration)

-- | Specifies the amount of storage in gibibytes (GiB) to reserve from the
-- parent volume. Setting @StorageCapacityReservationGiB@ guarantees that
-- the specified amount of storage space on the parent volume will always
-- be available for the volume. You can\'t reserve more storage than the
-- parent volume has. To /not/ specify a storage capacity reservation, set
-- this to @0@ or @-1@. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/managing-volumes.html#volume-properties Volume properties>
-- in the /Amazon FSx for OpenZFS User Guide/.
createOpenZFSVolumeConfiguration_storageCapacityReservationGiB :: Lens.Lens' CreateOpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Int)
createOpenZFSVolumeConfiguration_storageCapacityReservationGiB = Lens.lens (\CreateOpenZFSVolumeConfiguration' {storageCapacityReservationGiB} -> storageCapacityReservationGiB) (\s@CreateOpenZFSVolumeConfiguration' {} a -> s {storageCapacityReservationGiB = a} :: CreateOpenZFSVolumeConfiguration)

-- | An object specifying how much storage users or groups can use on the
-- volume.
createOpenZFSVolumeConfiguration_userAndGroupQuotas :: Lens.Lens' CreateOpenZFSVolumeConfiguration (Prelude.Maybe [OpenZFSUserOrGroupQuota])
createOpenZFSVolumeConfiguration_userAndGroupQuotas = Lens.lens (\CreateOpenZFSVolumeConfiguration' {userAndGroupQuotas} -> userAndGroupQuotas) (\s@CreateOpenZFSVolumeConfiguration' {} a -> s {userAndGroupQuotas = a} :: CreateOpenZFSVolumeConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the volume to use as the parent volume of the volume that you
-- are creating.
createOpenZFSVolumeConfiguration_parentVolumeId :: Lens.Lens' CreateOpenZFSVolumeConfiguration Prelude.Text
createOpenZFSVolumeConfiguration_parentVolumeId = Lens.lens (\CreateOpenZFSVolumeConfiguration' {parentVolumeId} -> parentVolumeId) (\s@CreateOpenZFSVolumeConfiguration' {} a -> s {parentVolumeId = a} :: CreateOpenZFSVolumeConfiguration)

instance
  Prelude.Hashable
    CreateOpenZFSVolumeConfiguration
  where
  hashWithSalt
    _salt
    CreateOpenZFSVolumeConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` copyTagsToSnapshots
        `Prelude.hashWithSalt` dataCompressionType
        `Prelude.hashWithSalt` nfsExports
        `Prelude.hashWithSalt` originSnapshot
        `Prelude.hashWithSalt` readOnly
        `Prelude.hashWithSalt` recordSizeKiB
        `Prelude.hashWithSalt` storageCapacityQuotaGiB
        `Prelude.hashWithSalt` storageCapacityReservationGiB
        `Prelude.hashWithSalt` userAndGroupQuotas
        `Prelude.hashWithSalt` parentVolumeId

instance
  Prelude.NFData
    CreateOpenZFSVolumeConfiguration
  where
  rnf CreateOpenZFSVolumeConfiguration' {..} =
    Prelude.rnf copyTagsToSnapshots
      `Prelude.seq` Prelude.rnf dataCompressionType
      `Prelude.seq` Prelude.rnf nfsExports
      `Prelude.seq` Prelude.rnf originSnapshot
      `Prelude.seq` Prelude.rnf readOnly
      `Prelude.seq` Prelude.rnf recordSizeKiB
      `Prelude.seq` Prelude.rnf storageCapacityQuotaGiB
      `Prelude.seq` Prelude.rnf storageCapacityReservationGiB
      `Prelude.seq` Prelude.rnf userAndGroupQuotas
      `Prelude.seq` Prelude.rnf parentVolumeId

instance Data.ToJSON CreateOpenZFSVolumeConfiguration where
  toJSON CreateOpenZFSVolumeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CopyTagsToSnapshots" Data..=)
              Prelude.<$> copyTagsToSnapshots,
            ("DataCompressionType" Data..=)
              Prelude.<$> dataCompressionType,
            ("NfsExports" Data..=) Prelude.<$> nfsExports,
            ("OriginSnapshot" Data..=)
              Prelude.<$> originSnapshot,
            ("ReadOnly" Data..=) Prelude.<$> readOnly,
            ("RecordSizeKiB" Data..=) Prelude.<$> recordSizeKiB,
            ("StorageCapacityQuotaGiB" Data..=)
              Prelude.<$> storageCapacityQuotaGiB,
            ("StorageCapacityReservationGiB" Data..=)
              Prelude.<$> storageCapacityReservationGiB,
            ("UserAndGroupQuotas" Data..=)
              Prelude.<$> userAndGroupQuotas,
            Prelude.Just
              ("ParentVolumeId" Data..= parentVolumeId)
          ]
      )
