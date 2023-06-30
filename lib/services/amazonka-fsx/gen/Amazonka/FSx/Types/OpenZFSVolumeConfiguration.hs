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
-- Module      : Amazonka.FSx.Types.OpenZFSVolumeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.OpenZFSVolumeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.OpenZFSDataCompressionType
import Amazonka.FSx.Types.OpenZFSNfsExport
import Amazonka.FSx.Types.OpenZFSOriginSnapshotConfiguration
import Amazonka.FSx.Types.OpenZFSUserOrGroupQuota
import qualified Amazonka.Prelude as Prelude

-- | The configuration of an Amazon FSx for OpenZFS volume.
--
-- /See:/ 'newOpenZFSVolumeConfiguration' smart constructor.
data OpenZFSVolumeConfiguration = OpenZFSVolumeConfiguration'
  { -- | A Boolean value indicating whether tags for the volume should be copied
    -- to snapshots. This value defaults to @false@. If it\'s set to @true@,
    -- all tags for the volume are copied to snapshots where the user doesn\'t
    -- specify tags. If this value is @true@ and you specify one or more tags,
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
    --     (ZSTD) compression algorithm. Compared to LZ4, Z-Standard provides a
    --     better compression ratio to minimize on-disk storage utilization.
    --
    -- -   @LZ4@ - Compresses the data in the volume using the LZ4 compression
    --     algorithm. Compared to Z-Standard, LZ4 is less compute-intensive and
    --     delivers higher write throughput speeds.
    dataCompressionType :: Prelude.Maybe OpenZFSDataCompressionType,
    -- | A Boolean value indicating whether dependent clone volumes created from
    -- intermediate snapshots should be deleted when a volume is restored from
    -- snapshot.
    deleteClonedVolumes :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value indicating whether snapshots between the current state
    -- and the specified snapshot should be deleted when a volume is restored
    -- from snapshot.
    deleteIntermediateSnaphots :: Prelude.Maybe Prelude.Bool,
    -- | The configuration object for mounting a Network File System (NFS) file
    -- system.
    nfsExports :: Prelude.Maybe [OpenZFSNfsExport],
    -- | The configuration object that specifies the snapshot to use as the
    -- origin of the data for the volume.
    originSnapshot :: Prelude.Maybe OpenZFSOriginSnapshotConfiguration,
    -- | The ID of the parent volume.
    parentVolumeId :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether the volume is read-only.
    readOnly :: Prelude.Maybe Prelude.Bool,
    -- | The record size of an OpenZFS volume, in kibibytes (KiB). Valid values
    -- are 4, 8, 16, 32, 64, 128, 256, 512, or 1024 KiB. The default is 128
    -- KiB. Most workloads should use the default record size. For guidance on
    -- when to set a custom record size, see the /Amazon FSx for OpenZFS User
    -- Guide/.
    recordSizeKiB :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the ID of the snapshot to which the volume was restored.
    restoreToSnapshot :: Prelude.Maybe Prelude.Text,
    -- | The maximum amount of storage in gibibtyes (GiB) that the volume can use
    -- from its parent. You can specify a quota larger than the storage on the
    -- parent volume.
    storageCapacityQuotaGiB :: Prelude.Maybe Prelude.Natural,
    -- | The amount of storage in gibibytes (GiB) to reserve from the parent
    -- volume. You can\'t reserve more storage than the parent volume has
    -- reserved.
    storageCapacityReservationGiB :: Prelude.Maybe Prelude.Natural,
    -- | An object specifying how much storage users or groups can use on the
    -- volume.
    userAndGroupQuotas :: Prelude.Maybe [OpenZFSUserOrGroupQuota],
    -- | The path to the volume from the root volume. For example,
    -- @fsx\/parentVolume\/volume1@.
    volumePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenZFSVolumeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyTagsToSnapshots', 'openZFSVolumeConfiguration_copyTagsToSnapshots' - A Boolean value indicating whether tags for the volume should be copied
-- to snapshots. This value defaults to @false@. If it\'s set to @true@,
-- all tags for the volume are copied to snapshots where the user doesn\'t
-- specify tags. If this value is @true@ and you specify one or more tags,
-- only the specified tags are copied to snapshots. If you specify one or
-- more tags when creating the snapshot, no tags are copied from the
-- volume, regardless of this value.
--
-- 'dataCompressionType', 'openZFSVolumeConfiguration_dataCompressionType' - Specifies the method used to compress the data on the volume. The
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
-- 'deleteClonedVolumes', 'openZFSVolumeConfiguration_deleteClonedVolumes' - A Boolean value indicating whether dependent clone volumes created from
-- intermediate snapshots should be deleted when a volume is restored from
-- snapshot.
--
-- 'deleteIntermediateSnaphots', 'openZFSVolumeConfiguration_deleteIntermediateSnaphots' - A Boolean value indicating whether snapshots between the current state
-- and the specified snapshot should be deleted when a volume is restored
-- from snapshot.
--
-- 'nfsExports', 'openZFSVolumeConfiguration_nfsExports' - The configuration object for mounting a Network File System (NFS) file
-- system.
--
-- 'originSnapshot', 'openZFSVolumeConfiguration_originSnapshot' - The configuration object that specifies the snapshot to use as the
-- origin of the data for the volume.
--
-- 'parentVolumeId', 'openZFSVolumeConfiguration_parentVolumeId' - The ID of the parent volume.
--
-- 'readOnly', 'openZFSVolumeConfiguration_readOnly' - A Boolean value indicating whether the volume is read-only.
--
-- 'recordSizeKiB', 'openZFSVolumeConfiguration_recordSizeKiB' - The record size of an OpenZFS volume, in kibibytes (KiB). Valid values
-- are 4, 8, 16, 32, 64, 128, 256, 512, or 1024 KiB. The default is 128
-- KiB. Most workloads should use the default record size. For guidance on
-- when to set a custom record size, see the /Amazon FSx for OpenZFS User
-- Guide/.
--
-- 'restoreToSnapshot', 'openZFSVolumeConfiguration_restoreToSnapshot' - Specifies the ID of the snapshot to which the volume was restored.
--
-- 'storageCapacityQuotaGiB', 'openZFSVolumeConfiguration_storageCapacityQuotaGiB' - The maximum amount of storage in gibibtyes (GiB) that the volume can use
-- from its parent. You can specify a quota larger than the storage on the
-- parent volume.
--
-- 'storageCapacityReservationGiB', 'openZFSVolumeConfiguration_storageCapacityReservationGiB' - The amount of storage in gibibytes (GiB) to reserve from the parent
-- volume. You can\'t reserve more storage than the parent volume has
-- reserved.
--
-- 'userAndGroupQuotas', 'openZFSVolumeConfiguration_userAndGroupQuotas' - An object specifying how much storage users or groups can use on the
-- volume.
--
-- 'volumePath', 'openZFSVolumeConfiguration_volumePath' - The path to the volume from the root volume. For example,
-- @fsx\/parentVolume\/volume1@.
newOpenZFSVolumeConfiguration ::
  OpenZFSVolumeConfiguration
newOpenZFSVolumeConfiguration =
  OpenZFSVolumeConfiguration'
    { copyTagsToSnapshots =
        Prelude.Nothing,
      dataCompressionType = Prelude.Nothing,
      deleteClonedVolumes = Prelude.Nothing,
      deleteIntermediateSnaphots = Prelude.Nothing,
      nfsExports = Prelude.Nothing,
      originSnapshot = Prelude.Nothing,
      parentVolumeId = Prelude.Nothing,
      readOnly = Prelude.Nothing,
      recordSizeKiB = Prelude.Nothing,
      restoreToSnapshot = Prelude.Nothing,
      storageCapacityQuotaGiB = Prelude.Nothing,
      storageCapacityReservationGiB = Prelude.Nothing,
      userAndGroupQuotas = Prelude.Nothing,
      volumePath = Prelude.Nothing
    }

-- | A Boolean value indicating whether tags for the volume should be copied
-- to snapshots. This value defaults to @false@. If it\'s set to @true@,
-- all tags for the volume are copied to snapshots where the user doesn\'t
-- specify tags. If this value is @true@ and you specify one or more tags,
-- only the specified tags are copied to snapshots. If you specify one or
-- more tags when creating the snapshot, no tags are copied from the
-- volume, regardless of this value.
openZFSVolumeConfiguration_copyTagsToSnapshots :: Lens.Lens' OpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Bool)
openZFSVolumeConfiguration_copyTagsToSnapshots = Lens.lens (\OpenZFSVolumeConfiguration' {copyTagsToSnapshots} -> copyTagsToSnapshots) (\s@OpenZFSVolumeConfiguration' {} a -> s {copyTagsToSnapshots = a} :: OpenZFSVolumeConfiguration)

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
openZFSVolumeConfiguration_dataCompressionType :: Lens.Lens' OpenZFSVolumeConfiguration (Prelude.Maybe OpenZFSDataCompressionType)
openZFSVolumeConfiguration_dataCompressionType = Lens.lens (\OpenZFSVolumeConfiguration' {dataCompressionType} -> dataCompressionType) (\s@OpenZFSVolumeConfiguration' {} a -> s {dataCompressionType = a} :: OpenZFSVolumeConfiguration)

-- | A Boolean value indicating whether dependent clone volumes created from
-- intermediate snapshots should be deleted when a volume is restored from
-- snapshot.
openZFSVolumeConfiguration_deleteClonedVolumes :: Lens.Lens' OpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Bool)
openZFSVolumeConfiguration_deleteClonedVolumes = Lens.lens (\OpenZFSVolumeConfiguration' {deleteClonedVolumes} -> deleteClonedVolumes) (\s@OpenZFSVolumeConfiguration' {} a -> s {deleteClonedVolumes = a} :: OpenZFSVolumeConfiguration)

-- | A Boolean value indicating whether snapshots between the current state
-- and the specified snapshot should be deleted when a volume is restored
-- from snapshot.
openZFSVolumeConfiguration_deleteIntermediateSnaphots :: Lens.Lens' OpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Bool)
openZFSVolumeConfiguration_deleteIntermediateSnaphots = Lens.lens (\OpenZFSVolumeConfiguration' {deleteIntermediateSnaphots} -> deleteIntermediateSnaphots) (\s@OpenZFSVolumeConfiguration' {} a -> s {deleteIntermediateSnaphots = a} :: OpenZFSVolumeConfiguration)

-- | The configuration object for mounting a Network File System (NFS) file
-- system.
openZFSVolumeConfiguration_nfsExports :: Lens.Lens' OpenZFSVolumeConfiguration (Prelude.Maybe [OpenZFSNfsExport])
openZFSVolumeConfiguration_nfsExports = Lens.lens (\OpenZFSVolumeConfiguration' {nfsExports} -> nfsExports) (\s@OpenZFSVolumeConfiguration' {} a -> s {nfsExports = a} :: OpenZFSVolumeConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The configuration object that specifies the snapshot to use as the
-- origin of the data for the volume.
openZFSVolumeConfiguration_originSnapshot :: Lens.Lens' OpenZFSVolumeConfiguration (Prelude.Maybe OpenZFSOriginSnapshotConfiguration)
openZFSVolumeConfiguration_originSnapshot = Lens.lens (\OpenZFSVolumeConfiguration' {originSnapshot} -> originSnapshot) (\s@OpenZFSVolumeConfiguration' {} a -> s {originSnapshot = a} :: OpenZFSVolumeConfiguration)

-- | The ID of the parent volume.
openZFSVolumeConfiguration_parentVolumeId :: Lens.Lens' OpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Text)
openZFSVolumeConfiguration_parentVolumeId = Lens.lens (\OpenZFSVolumeConfiguration' {parentVolumeId} -> parentVolumeId) (\s@OpenZFSVolumeConfiguration' {} a -> s {parentVolumeId = a} :: OpenZFSVolumeConfiguration)

-- | A Boolean value indicating whether the volume is read-only.
openZFSVolumeConfiguration_readOnly :: Lens.Lens' OpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Bool)
openZFSVolumeConfiguration_readOnly = Lens.lens (\OpenZFSVolumeConfiguration' {readOnly} -> readOnly) (\s@OpenZFSVolumeConfiguration' {} a -> s {readOnly = a} :: OpenZFSVolumeConfiguration)

-- | The record size of an OpenZFS volume, in kibibytes (KiB). Valid values
-- are 4, 8, 16, 32, 64, 128, 256, 512, or 1024 KiB. The default is 128
-- KiB. Most workloads should use the default record size. For guidance on
-- when to set a custom record size, see the /Amazon FSx for OpenZFS User
-- Guide/.
openZFSVolumeConfiguration_recordSizeKiB :: Lens.Lens' OpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Natural)
openZFSVolumeConfiguration_recordSizeKiB = Lens.lens (\OpenZFSVolumeConfiguration' {recordSizeKiB} -> recordSizeKiB) (\s@OpenZFSVolumeConfiguration' {} a -> s {recordSizeKiB = a} :: OpenZFSVolumeConfiguration)

-- | Specifies the ID of the snapshot to which the volume was restored.
openZFSVolumeConfiguration_restoreToSnapshot :: Lens.Lens' OpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Text)
openZFSVolumeConfiguration_restoreToSnapshot = Lens.lens (\OpenZFSVolumeConfiguration' {restoreToSnapshot} -> restoreToSnapshot) (\s@OpenZFSVolumeConfiguration' {} a -> s {restoreToSnapshot = a} :: OpenZFSVolumeConfiguration)

-- | The maximum amount of storage in gibibtyes (GiB) that the volume can use
-- from its parent. You can specify a quota larger than the storage on the
-- parent volume.
openZFSVolumeConfiguration_storageCapacityQuotaGiB :: Lens.Lens' OpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Natural)
openZFSVolumeConfiguration_storageCapacityQuotaGiB = Lens.lens (\OpenZFSVolumeConfiguration' {storageCapacityQuotaGiB} -> storageCapacityQuotaGiB) (\s@OpenZFSVolumeConfiguration' {} a -> s {storageCapacityQuotaGiB = a} :: OpenZFSVolumeConfiguration)

-- | The amount of storage in gibibytes (GiB) to reserve from the parent
-- volume. You can\'t reserve more storage than the parent volume has
-- reserved.
openZFSVolumeConfiguration_storageCapacityReservationGiB :: Lens.Lens' OpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Natural)
openZFSVolumeConfiguration_storageCapacityReservationGiB = Lens.lens (\OpenZFSVolumeConfiguration' {storageCapacityReservationGiB} -> storageCapacityReservationGiB) (\s@OpenZFSVolumeConfiguration' {} a -> s {storageCapacityReservationGiB = a} :: OpenZFSVolumeConfiguration)

-- | An object specifying how much storage users or groups can use on the
-- volume.
openZFSVolumeConfiguration_userAndGroupQuotas :: Lens.Lens' OpenZFSVolumeConfiguration (Prelude.Maybe [OpenZFSUserOrGroupQuota])
openZFSVolumeConfiguration_userAndGroupQuotas = Lens.lens (\OpenZFSVolumeConfiguration' {userAndGroupQuotas} -> userAndGroupQuotas) (\s@OpenZFSVolumeConfiguration' {} a -> s {userAndGroupQuotas = a} :: OpenZFSVolumeConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The path to the volume from the root volume. For example,
-- @fsx\/parentVolume\/volume1@.
openZFSVolumeConfiguration_volumePath :: Lens.Lens' OpenZFSVolumeConfiguration (Prelude.Maybe Prelude.Text)
openZFSVolumeConfiguration_volumePath = Lens.lens (\OpenZFSVolumeConfiguration' {volumePath} -> volumePath) (\s@OpenZFSVolumeConfiguration' {} a -> s {volumePath = a} :: OpenZFSVolumeConfiguration)

instance Data.FromJSON OpenZFSVolumeConfiguration where
  parseJSON =
    Data.withObject
      "OpenZFSVolumeConfiguration"
      ( \x ->
          OpenZFSVolumeConfiguration'
            Prelude.<$> (x Data..:? "CopyTagsToSnapshots")
            Prelude.<*> (x Data..:? "DataCompressionType")
            Prelude.<*> (x Data..:? "DeleteClonedVolumes")
            Prelude.<*> (x Data..:? "DeleteIntermediateSnaphots")
            Prelude.<*> (x Data..:? "NfsExports" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "OriginSnapshot")
            Prelude.<*> (x Data..:? "ParentVolumeId")
            Prelude.<*> (x Data..:? "ReadOnly")
            Prelude.<*> (x Data..:? "RecordSizeKiB")
            Prelude.<*> (x Data..:? "RestoreToSnapshot")
            Prelude.<*> (x Data..:? "StorageCapacityQuotaGiB")
            Prelude.<*> (x Data..:? "StorageCapacityReservationGiB")
            Prelude.<*> ( x
                            Data..:? "UserAndGroupQuotas"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "VolumePath")
      )

instance Prelude.Hashable OpenZFSVolumeConfiguration where
  hashWithSalt _salt OpenZFSVolumeConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` copyTagsToSnapshots
      `Prelude.hashWithSalt` dataCompressionType
      `Prelude.hashWithSalt` deleteClonedVolumes
      `Prelude.hashWithSalt` deleteIntermediateSnaphots
      `Prelude.hashWithSalt` nfsExports
      `Prelude.hashWithSalt` originSnapshot
      `Prelude.hashWithSalt` parentVolumeId
      `Prelude.hashWithSalt` readOnly
      `Prelude.hashWithSalt` recordSizeKiB
      `Prelude.hashWithSalt` restoreToSnapshot
      `Prelude.hashWithSalt` storageCapacityQuotaGiB
      `Prelude.hashWithSalt` storageCapacityReservationGiB
      `Prelude.hashWithSalt` userAndGroupQuotas
      `Prelude.hashWithSalt` volumePath

instance Prelude.NFData OpenZFSVolumeConfiguration where
  rnf OpenZFSVolumeConfiguration' {..} =
    Prelude.rnf copyTagsToSnapshots
      `Prelude.seq` Prelude.rnf dataCompressionType
      `Prelude.seq` Prelude.rnf deleteClonedVolumes
      `Prelude.seq` Prelude.rnf deleteIntermediateSnaphots
      `Prelude.seq` Prelude.rnf nfsExports
      `Prelude.seq` Prelude.rnf originSnapshot
      `Prelude.seq` Prelude.rnf parentVolumeId
      `Prelude.seq` Prelude.rnf readOnly
      `Prelude.seq` Prelude.rnf recordSizeKiB
      `Prelude.seq` Prelude.rnf restoreToSnapshot
      `Prelude.seq` Prelude.rnf storageCapacityQuotaGiB
      `Prelude.seq` Prelude.rnf storageCapacityReservationGiB
      `Prelude.seq` Prelude.rnf userAndGroupQuotas
      `Prelude.seq` Prelude.rnf volumePath
