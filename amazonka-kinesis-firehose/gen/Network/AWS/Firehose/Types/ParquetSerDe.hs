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
-- Module      : Network.AWS.Firehose.Types.ParquetSerDe
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ParquetSerDe where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.ParquetCompression
import Network.AWS.Firehose.Types.ParquetWriterVersion
import qualified Network.AWS.Lens as Lens

-- | A serializer to use for converting data to the Parquet format before
-- storing it in Amazon S3. For more information, see
-- <https://parquet.apache.org/documentation/latest/ Apache Parquet>.
--
-- /See:/ 'newParquetSerDe' smart constructor.
data ParquetSerDe = ParquetSerDe'
  { -- | The Parquet page size. Column chunks are divided into pages. A page is
    -- conceptually an indivisible unit (in terms of compression and encoding).
    -- The minimum value is 64 KiB and the default is 1 MiB.
    pageSizeBytes :: Core.Maybe Core.Natural,
    -- | Indicates whether to enable dictionary compression.
    enableDictionaryCompression :: Core.Maybe Core.Bool,
    -- | The maximum amount of padding to apply. This is useful if you intend to
    -- copy the data from Amazon S3 to HDFS before querying. The default is 0.
    maxPaddingBytes :: Core.Maybe Core.Natural,
    -- | The compression code to use over data blocks. The possible values are
    -- @UNCOMPRESSED@, @SNAPPY@, and @GZIP@, with the default being @SNAPPY@.
    -- Use @SNAPPY@ for higher decompression speed. Use @GZIP@ if the
    -- compression ratio is more important than speed.
    compression :: Core.Maybe ParquetCompression,
    -- | Indicates the version of row format to output. The possible values are
    -- @V1@ and @V2@. The default is @V1@.
    writerVersion :: Core.Maybe ParquetWriterVersion,
    -- | The Hadoop Distributed File System (HDFS) block size. This is useful if
    -- you intend to copy the data from Amazon S3 to HDFS before querying. The
    -- default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses
    -- this value for padding calculations.
    blockSizeBytes :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ParquetSerDe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSizeBytes', 'parquetSerDe_pageSizeBytes' - The Parquet page size. Column chunks are divided into pages. A page is
-- conceptually an indivisible unit (in terms of compression and encoding).
-- The minimum value is 64 KiB and the default is 1 MiB.
--
-- 'enableDictionaryCompression', 'parquetSerDe_enableDictionaryCompression' - Indicates whether to enable dictionary compression.
--
-- 'maxPaddingBytes', 'parquetSerDe_maxPaddingBytes' - The maximum amount of padding to apply. This is useful if you intend to
-- copy the data from Amazon S3 to HDFS before querying. The default is 0.
--
-- 'compression', 'parquetSerDe_compression' - The compression code to use over data blocks. The possible values are
-- @UNCOMPRESSED@, @SNAPPY@, and @GZIP@, with the default being @SNAPPY@.
-- Use @SNAPPY@ for higher decompression speed. Use @GZIP@ if the
-- compression ratio is more important than speed.
--
-- 'writerVersion', 'parquetSerDe_writerVersion' - Indicates the version of row format to output. The possible values are
-- @V1@ and @V2@. The default is @V1@.
--
-- 'blockSizeBytes', 'parquetSerDe_blockSizeBytes' - The Hadoop Distributed File System (HDFS) block size. This is useful if
-- you intend to copy the data from Amazon S3 to HDFS before querying. The
-- default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses
-- this value for padding calculations.
newParquetSerDe ::
  ParquetSerDe
newParquetSerDe =
  ParquetSerDe'
    { pageSizeBytes = Core.Nothing,
      enableDictionaryCompression = Core.Nothing,
      maxPaddingBytes = Core.Nothing,
      compression = Core.Nothing,
      writerVersion = Core.Nothing,
      blockSizeBytes = Core.Nothing
    }

-- | The Parquet page size. Column chunks are divided into pages. A page is
-- conceptually an indivisible unit (in terms of compression and encoding).
-- The minimum value is 64 KiB and the default is 1 MiB.
parquetSerDe_pageSizeBytes :: Lens.Lens' ParquetSerDe (Core.Maybe Core.Natural)
parquetSerDe_pageSizeBytes = Lens.lens (\ParquetSerDe' {pageSizeBytes} -> pageSizeBytes) (\s@ParquetSerDe' {} a -> s {pageSizeBytes = a} :: ParquetSerDe)

-- | Indicates whether to enable dictionary compression.
parquetSerDe_enableDictionaryCompression :: Lens.Lens' ParquetSerDe (Core.Maybe Core.Bool)
parquetSerDe_enableDictionaryCompression = Lens.lens (\ParquetSerDe' {enableDictionaryCompression} -> enableDictionaryCompression) (\s@ParquetSerDe' {} a -> s {enableDictionaryCompression = a} :: ParquetSerDe)

-- | The maximum amount of padding to apply. This is useful if you intend to
-- copy the data from Amazon S3 to HDFS before querying. The default is 0.
parquetSerDe_maxPaddingBytes :: Lens.Lens' ParquetSerDe (Core.Maybe Core.Natural)
parquetSerDe_maxPaddingBytes = Lens.lens (\ParquetSerDe' {maxPaddingBytes} -> maxPaddingBytes) (\s@ParquetSerDe' {} a -> s {maxPaddingBytes = a} :: ParquetSerDe)

-- | The compression code to use over data blocks. The possible values are
-- @UNCOMPRESSED@, @SNAPPY@, and @GZIP@, with the default being @SNAPPY@.
-- Use @SNAPPY@ for higher decompression speed. Use @GZIP@ if the
-- compression ratio is more important than speed.
parquetSerDe_compression :: Lens.Lens' ParquetSerDe (Core.Maybe ParquetCompression)
parquetSerDe_compression = Lens.lens (\ParquetSerDe' {compression} -> compression) (\s@ParquetSerDe' {} a -> s {compression = a} :: ParquetSerDe)

-- | Indicates the version of row format to output. The possible values are
-- @V1@ and @V2@. The default is @V1@.
parquetSerDe_writerVersion :: Lens.Lens' ParquetSerDe (Core.Maybe ParquetWriterVersion)
parquetSerDe_writerVersion = Lens.lens (\ParquetSerDe' {writerVersion} -> writerVersion) (\s@ParquetSerDe' {} a -> s {writerVersion = a} :: ParquetSerDe)

-- | The Hadoop Distributed File System (HDFS) block size. This is useful if
-- you intend to copy the data from Amazon S3 to HDFS before querying. The
-- default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses
-- this value for padding calculations.
parquetSerDe_blockSizeBytes :: Lens.Lens' ParquetSerDe (Core.Maybe Core.Natural)
parquetSerDe_blockSizeBytes = Lens.lens (\ParquetSerDe' {blockSizeBytes} -> blockSizeBytes) (\s@ParquetSerDe' {} a -> s {blockSizeBytes = a} :: ParquetSerDe)

instance Core.FromJSON ParquetSerDe where
  parseJSON =
    Core.withObject
      "ParquetSerDe"
      ( \x ->
          ParquetSerDe'
            Core.<$> (x Core..:? "PageSizeBytes")
            Core.<*> (x Core..:? "EnableDictionaryCompression")
            Core.<*> (x Core..:? "MaxPaddingBytes")
            Core.<*> (x Core..:? "Compression")
            Core.<*> (x Core..:? "WriterVersion")
            Core.<*> (x Core..:? "BlockSizeBytes")
      )

instance Core.Hashable ParquetSerDe

instance Core.NFData ParquetSerDe

instance Core.ToJSON ParquetSerDe where
  toJSON ParquetSerDe' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PageSizeBytes" Core..=) Core.<$> pageSizeBytes,
            ("EnableDictionaryCompression" Core..=)
              Core.<$> enableDictionaryCompression,
            ("MaxPaddingBytes" Core..=) Core.<$> maxPaddingBytes,
            ("Compression" Core..=) Core.<$> compression,
            ("WriterVersion" Core..=) Core.<$> writerVersion,
            ("BlockSizeBytes" Core..=) Core.<$> blockSizeBytes
          ]
      )
