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
-- Module      : Amazonka.FSx.Types.OpenZFSCreateRootVolumeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.OpenZFSCreateRootVolumeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.OpenZFSDataCompressionType
import Amazonka.FSx.Types.OpenZFSNfsExport
import Amazonka.FSx.Types.OpenZFSUserOrGroupQuota
import qualified Amazonka.Prelude as Prelude

-- | The configuration of an Amazon FSx for OpenZFS root volume.
--
-- /See:/ 'newOpenZFSCreateRootVolumeConfiguration' smart constructor.
data OpenZFSCreateRootVolumeConfiguration = OpenZFSCreateRootVolumeConfiguration'
  { -- | Specifies the record size of an OpenZFS root volume, in kibibytes (KiB).
    -- Valid values are 4, 8, 16, 32, 64, 128, 256, 512, or 1024 KiB. The
    -- default is 128 KiB. Most workloads should use the default record size.
    -- Database workflows can benefit from a smaller record size, while
    -- streaming workflows can benefit from a larger record size. For
    -- additional guidance on setting a custom record size, see
    -- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/performance.html#performance-tips-zfs Tips for maximizing performance>
    -- in the /Amazon FSx for OpenZFS User Guide/.
    recordSizeKiB :: Prelude.Maybe Prelude.Natural,
    -- | A Boolean value indicating whether the volume is read-only. Setting this
    -- value to @true@ can be useful after you have completed changes to a
    -- volume and no longer want changes to occur.
    readOnly :: Prelude.Maybe Prelude.Bool,
    -- | The configuration object for mounting a file system.
    nfsExports :: Prelude.Maybe [OpenZFSNfsExport],
    -- | A Boolean value indicating whether tags for the volume should be copied
    -- to snapshots of the volume. This value defaults to @false@. If it\'s set
    -- to @true@, all tags for the volume are copied to snapshots where the
    -- user doesn\'t specify tags. If this value is @true@ and you specify one
    -- or more tags, only the specified tags are copied to snapshots. If you
    -- specify one or more tags when creating the snapshot, no tags are copied
    -- from the volume, regardless of this value.
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
    -- | An object specifying how much storage users or groups can use on the
    -- volume.
    userAndGroupQuotas :: Prelude.Maybe [OpenZFSUserOrGroupQuota]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenZFSCreateRootVolumeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordSizeKiB', 'openZFSCreateRootVolumeConfiguration_recordSizeKiB' - Specifies the record size of an OpenZFS root volume, in kibibytes (KiB).
-- Valid values are 4, 8, 16, 32, 64, 128, 256, 512, or 1024 KiB. The
-- default is 128 KiB. Most workloads should use the default record size.
-- Database workflows can benefit from a smaller record size, while
-- streaming workflows can benefit from a larger record size. For
-- additional guidance on setting a custom record size, see
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/performance.html#performance-tips-zfs Tips for maximizing performance>
-- in the /Amazon FSx for OpenZFS User Guide/.
--
-- 'readOnly', 'openZFSCreateRootVolumeConfiguration_readOnly' - A Boolean value indicating whether the volume is read-only. Setting this
-- value to @true@ can be useful after you have completed changes to a
-- volume and no longer want changes to occur.
--
-- 'nfsExports', 'openZFSCreateRootVolumeConfiguration_nfsExports' - The configuration object for mounting a file system.
--
-- 'copyTagsToSnapshots', 'openZFSCreateRootVolumeConfiguration_copyTagsToSnapshots' - A Boolean value indicating whether tags for the volume should be copied
-- to snapshots of the volume. This value defaults to @false@. If it\'s set
-- to @true@, all tags for the volume are copied to snapshots where the
-- user doesn\'t specify tags. If this value is @true@ and you specify one
-- or more tags, only the specified tags are copied to snapshots. If you
-- specify one or more tags when creating the snapshot, no tags are copied
-- from the volume, regardless of this value.
--
-- 'dataCompressionType', 'openZFSCreateRootVolumeConfiguration_dataCompressionType' - Specifies the method used to compress the data on the volume. The
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
-- 'userAndGroupQuotas', 'openZFSCreateRootVolumeConfiguration_userAndGroupQuotas' - An object specifying how much storage users or groups can use on the
-- volume.
newOpenZFSCreateRootVolumeConfiguration ::
  OpenZFSCreateRootVolumeConfiguration
newOpenZFSCreateRootVolumeConfiguration =
  OpenZFSCreateRootVolumeConfiguration'
    { recordSizeKiB =
        Prelude.Nothing,
      readOnly = Prelude.Nothing,
      nfsExports = Prelude.Nothing,
      copyTagsToSnapshots = Prelude.Nothing,
      dataCompressionType = Prelude.Nothing,
      userAndGroupQuotas = Prelude.Nothing
    }

-- | Specifies the record size of an OpenZFS root volume, in kibibytes (KiB).
-- Valid values are 4, 8, 16, 32, 64, 128, 256, 512, or 1024 KiB. The
-- default is 128 KiB. Most workloads should use the default record size.
-- Database workflows can benefit from a smaller record size, while
-- streaming workflows can benefit from a larger record size. For
-- additional guidance on setting a custom record size, see
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/performance.html#performance-tips-zfs Tips for maximizing performance>
-- in the /Amazon FSx for OpenZFS User Guide/.
openZFSCreateRootVolumeConfiguration_recordSizeKiB :: Lens.Lens' OpenZFSCreateRootVolumeConfiguration (Prelude.Maybe Prelude.Natural)
openZFSCreateRootVolumeConfiguration_recordSizeKiB = Lens.lens (\OpenZFSCreateRootVolumeConfiguration' {recordSizeKiB} -> recordSizeKiB) (\s@OpenZFSCreateRootVolumeConfiguration' {} a -> s {recordSizeKiB = a} :: OpenZFSCreateRootVolumeConfiguration)

-- | A Boolean value indicating whether the volume is read-only. Setting this
-- value to @true@ can be useful after you have completed changes to a
-- volume and no longer want changes to occur.
openZFSCreateRootVolumeConfiguration_readOnly :: Lens.Lens' OpenZFSCreateRootVolumeConfiguration (Prelude.Maybe Prelude.Bool)
openZFSCreateRootVolumeConfiguration_readOnly = Lens.lens (\OpenZFSCreateRootVolumeConfiguration' {readOnly} -> readOnly) (\s@OpenZFSCreateRootVolumeConfiguration' {} a -> s {readOnly = a} :: OpenZFSCreateRootVolumeConfiguration)

-- | The configuration object for mounting a file system.
openZFSCreateRootVolumeConfiguration_nfsExports :: Lens.Lens' OpenZFSCreateRootVolumeConfiguration (Prelude.Maybe [OpenZFSNfsExport])
openZFSCreateRootVolumeConfiguration_nfsExports = Lens.lens (\OpenZFSCreateRootVolumeConfiguration' {nfsExports} -> nfsExports) (\s@OpenZFSCreateRootVolumeConfiguration' {} a -> s {nfsExports = a} :: OpenZFSCreateRootVolumeConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean value indicating whether tags for the volume should be copied
-- to snapshots of the volume. This value defaults to @false@. If it\'s set
-- to @true@, all tags for the volume are copied to snapshots where the
-- user doesn\'t specify tags. If this value is @true@ and you specify one
-- or more tags, only the specified tags are copied to snapshots. If you
-- specify one or more tags when creating the snapshot, no tags are copied
-- from the volume, regardless of this value.
openZFSCreateRootVolumeConfiguration_copyTagsToSnapshots :: Lens.Lens' OpenZFSCreateRootVolumeConfiguration (Prelude.Maybe Prelude.Bool)
openZFSCreateRootVolumeConfiguration_copyTagsToSnapshots = Lens.lens (\OpenZFSCreateRootVolumeConfiguration' {copyTagsToSnapshots} -> copyTagsToSnapshots) (\s@OpenZFSCreateRootVolumeConfiguration' {} a -> s {copyTagsToSnapshots = a} :: OpenZFSCreateRootVolumeConfiguration)

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
openZFSCreateRootVolumeConfiguration_dataCompressionType :: Lens.Lens' OpenZFSCreateRootVolumeConfiguration (Prelude.Maybe OpenZFSDataCompressionType)
openZFSCreateRootVolumeConfiguration_dataCompressionType = Lens.lens (\OpenZFSCreateRootVolumeConfiguration' {dataCompressionType} -> dataCompressionType) (\s@OpenZFSCreateRootVolumeConfiguration' {} a -> s {dataCompressionType = a} :: OpenZFSCreateRootVolumeConfiguration)

-- | An object specifying how much storage users or groups can use on the
-- volume.
openZFSCreateRootVolumeConfiguration_userAndGroupQuotas :: Lens.Lens' OpenZFSCreateRootVolumeConfiguration (Prelude.Maybe [OpenZFSUserOrGroupQuota])
openZFSCreateRootVolumeConfiguration_userAndGroupQuotas = Lens.lens (\OpenZFSCreateRootVolumeConfiguration' {userAndGroupQuotas} -> userAndGroupQuotas) (\s@OpenZFSCreateRootVolumeConfiguration' {} a -> s {userAndGroupQuotas = a} :: OpenZFSCreateRootVolumeConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    OpenZFSCreateRootVolumeConfiguration
  where
  hashWithSalt
    _salt
    OpenZFSCreateRootVolumeConfiguration' {..} =
      _salt `Prelude.hashWithSalt` recordSizeKiB
        `Prelude.hashWithSalt` readOnly
        `Prelude.hashWithSalt` nfsExports
        `Prelude.hashWithSalt` copyTagsToSnapshots
        `Prelude.hashWithSalt` dataCompressionType
        `Prelude.hashWithSalt` userAndGroupQuotas

instance
  Prelude.NFData
    OpenZFSCreateRootVolumeConfiguration
  where
  rnf OpenZFSCreateRootVolumeConfiguration' {..} =
    Prelude.rnf recordSizeKiB
      `Prelude.seq` Prelude.rnf readOnly
      `Prelude.seq` Prelude.rnf nfsExports
      `Prelude.seq` Prelude.rnf copyTagsToSnapshots
      `Prelude.seq` Prelude.rnf dataCompressionType
      `Prelude.seq` Prelude.rnf userAndGroupQuotas

instance
  Data.ToJSON
    OpenZFSCreateRootVolumeConfiguration
  where
  toJSON OpenZFSCreateRootVolumeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RecordSizeKiB" Data..=) Prelude.<$> recordSizeKiB,
            ("ReadOnly" Data..=) Prelude.<$> readOnly,
            ("NfsExports" Data..=) Prelude.<$> nfsExports,
            ("CopyTagsToSnapshots" Data..=)
              Prelude.<$> copyTagsToSnapshots,
            ("DataCompressionType" Data..=)
              Prelude.<$> dataCompressionType,
            ("UserAndGroupQuotas" Data..=)
              Prelude.<$> userAndGroupQuotas
          ]
      )
