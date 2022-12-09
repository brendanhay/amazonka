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
-- Module      : Amazonka.Firehose.Types.ParquetSerDe
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.ParquetSerDe where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.ParquetCompression
import Amazonka.Firehose.Types.ParquetWriterVersion
import qualified Amazonka.Prelude as Prelude

-- | A serializer to use for converting data to the Parquet format before
-- storing it in Amazon S3. For more information, see
-- <https://parquet.apache.org/documentation/latest/ Apache Parquet>.
--
-- /See:/ 'newParquetSerDe' smart constructor.
data ParquetSerDe = ParquetSerDe'
  { -- | The Hadoop Distributed File System (HDFS) block size. This is useful if
    -- you intend to copy the data from Amazon S3 to HDFS before querying. The
    -- default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses
    -- this value for padding calculations.
    blockSizeBytes :: Prelude.Maybe Prelude.Natural,
    -- | The compression code to use over data blocks. The possible values are
    -- @UNCOMPRESSED@, @SNAPPY@, and @GZIP@, with the default being @SNAPPY@.
    -- Use @SNAPPY@ for higher decompression speed. Use @GZIP@ if the
    -- compression ratio is more important than speed.
    compression :: Prelude.Maybe ParquetCompression,
    -- | Indicates whether to enable dictionary compression.
    enableDictionaryCompression :: Prelude.Maybe Prelude.Bool,
    -- | The maximum amount of padding to apply. This is useful if you intend to
    -- copy the data from Amazon S3 to HDFS before querying. The default is 0.
    maxPaddingBytes :: Prelude.Maybe Prelude.Natural,
    -- | The Parquet page size. Column chunks are divided into pages. A page is
    -- conceptually an indivisible unit (in terms of compression and encoding).
    -- The minimum value is 64 KiB and the default is 1 MiB.
    pageSizeBytes :: Prelude.Maybe Prelude.Natural,
    -- | Indicates the version of row format to output. The possible values are
    -- @V1@ and @V2@. The default is @V1@.
    writerVersion :: Prelude.Maybe ParquetWriterVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParquetSerDe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockSizeBytes', 'parquetSerDe_blockSizeBytes' - The Hadoop Distributed File System (HDFS) block size. This is useful if
-- you intend to copy the data from Amazon S3 to HDFS before querying. The
-- default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses
-- this value for padding calculations.
--
-- 'compression', 'parquetSerDe_compression' - The compression code to use over data blocks. The possible values are
-- @UNCOMPRESSED@, @SNAPPY@, and @GZIP@, with the default being @SNAPPY@.
-- Use @SNAPPY@ for higher decompression speed. Use @GZIP@ if the
-- compression ratio is more important than speed.
--
-- 'enableDictionaryCompression', 'parquetSerDe_enableDictionaryCompression' - Indicates whether to enable dictionary compression.
--
-- 'maxPaddingBytes', 'parquetSerDe_maxPaddingBytes' - The maximum amount of padding to apply. This is useful if you intend to
-- copy the data from Amazon S3 to HDFS before querying. The default is 0.
--
-- 'pageSizeBytes', 'parquetSerDe_pageSizeBytes' - The Parquet page size. Column chunks are divided into pages. A page is
-- conceptually an indivisible unit (in terms of compression and encoding).
-- The minimum value is 64 KiB and the default is 1 MiB.
--
-- 'writerVersion', 'parquetSerDe_writerVersion' - Indicates the version of row format to output. The possible values are
-- @V1@ and @V2@. The default is @V1@.
newParquetSerDe ::
  ParquetSerDe
newParquetSerDe =
  ParquetSerDe'
    { blockSizeBytes = Prelude.Nothing,
      compression = Prelude.Nothing,
      enableDictionaryCompression = Prelude.Nothing,
      maxPaddingBytes = Prelude.Nothing,
      pageSizeBytes = Prelude.Nothing,
      writerVersion = Prelude.Nothing
    }

-- | The Hadoop Distributed File System (HDFS) block size. This is useful if
-- you intend to copy the data from Amazon S3 to HDFS before querying. The
-- default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses
-- this value for padding calculations.
parquetSerDe_blockSizeBytes :: Lens.Lens' ParquetSerDe (Prelude.Maybe Prelude.Natural)
parquetSerDe_blockSizeBytes = Lens.lens (\ParquetSerDe' {blockSizeBytes} -> blockSizeBytes) (\s@ParquetSerDe' {} a -> s {blockSizeBytes = a} :: ParquetSerDe)

-- | The compression code to use over data blocks. The possible values are
-- @UNCOMPRESSED@, @SNAPPY@, and @GZIP@, with the default being @SNAPPY@.
-- Use @SNAPPY@ for higher decompression speed. Use @GZIP@ if the
-- compression ratio is more important than speed.
parquetSerDe_compression :: Lens.Lens' ParquetSerDe (Prelude.Maybe ParquetCompression)
parquetSerDe_compression = Lens.lens (\ParquetSerDe' {compression} -> compression) (\s@ParquetSerDe' {} a -> s {compression = a} :: ParquetSerDe)

-- | Indicates whether to enable dictionary compression.
parquetSerDe_enableDictionaryCompression :: Lens.Lens' ParquetSerDe (Prelude.Maybe Prelude.Bool)
parquetSerDe_enableDictionaryCompression = Lens.lens (\ParquetSerDe' {enableDictionaryCompression} -> enableDictionaryCompression) (\s@ParquetSerDe' {} a -> s {enableDictionaryCompression = a} :: ParquetSerDe)

-- | The maximum amount of padding to apply. This is useful if you intend to
-- copy the data from Amazon S3 to HDFS before querying. The default is 0.
parquetSerDe_maxPaddingBytes :: Lens.Lens' ParquetSerDe (Prelude.Maybe Prelude.Natural)
parquetSerDe_maxPaddingBytes = Lens.lens (\ParquetSerDe' {maxPaddingBytes} -> maxPaddingBytes) (\s@ParquetSerDe' {} a -> s {maxPaddingBytes = a} :: ParquetSerDe)

-- | The Parquet page size. Column chunks are divided into pages. A page is
-- conceptually an indivisible unit (in terms of compression and encoding).
-- The minimum value is 64 KiB and the default is 1 MiB.
parquetSerDe_pageSizeBytes :: Lens.Lens' ParquetSerDe (Prelude.Maybe Prelude.Natural)
parquetSerDe_pageSizeBytes = Lens.lens (\ParquetSerDe' {pageSizeBytes} -> pageSizeBytes) (\s@ParquetSerDe' {} a -> s {pageSizeBytes = a} :: ParquetSerDe)

-- | Indicates the version of row format to output. The possible values are
-- @V1@ and @V2@. The default is @V1@.
parquetSerDe_writerVersion :: Lens.Lens' ParquetSerDe (Prelude.Maybe ParquetWriterVersion)
parquetSerDe_writerVersion = Lens.lens (\ParquetSerDe' {writerVersion} -> writerVersion) (\s@ParquetSerDe' {} a -> s {writerVersion = a} :: ParquetSerDe)

instance Data.FromJSON ParquetSerDe where
  parseJSON =
    Data.withObject
      "ParquetSerDe"
      ( \x ->
          ParquetSerDe'
            Prelude.<$> (x Data..:? "BlockSizeBytes")
            Prelude.<*> (x Data..:? "Compression")
            Prelude.<*> (x Data..:? "EnableDictionaryCompression")
            Prelude.<*> (x Data..:? "MaxPaddingBytes")
            Prelude.<*> (x Data..:? "PageSizeBytes")
            Prelude.<*> (x Data..:? "WriterVersion")
      )

instance Prelude.Hashable ParquetSerDe where
  hashWithSalt _salt ParquetSerDe' {..} =
    _salt `Prelude.hashWithSalt` blockSizeBytes
      `Prelude.hashWithSalt` compression
      `Prelude.hashWithSalt` enableDictionaryCompression
      `Prelude.hashWithSalt` maxPaddingBytes
      `Prelude.hashWithSalt` pageSizeBytes
      `Prelude.hashWithSalt` writerVersion

instance Prelude.NFData ParquetSerDe where
  rnf ParquetSerDe' {..} =
    Prelude.rnf blockSizeBytes
      `Prelude.seq` Prelude.rnf compression
      `Prelude.seq` Prelude.rnf enableDictionaryCompression
      `Prelude.seq` Prelude.rnf maxPaddingBytes
      `Prelude.seq` Prelude.rnf pageSizeBytes
      `Prelude.seq` Prelude.rnf writerVersion

instance Data.ToJSON ParquetSerDe where
  toJSON ParquetSerDe' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BlockSizeBytes" Data..=)
              Prelude.<$> blockSizeBytes,
            ("Compression" Data..=) Prelude.<$> compression,
            ("EnableDictionaryCompression" Data..=)
              Prelude.<$> enableDictionaryCompression,
            ("MaxPaddingBytes" Data..=)
              Prelude.<$> maxPaddingBytes,
            ("PageSizeBytes" Data..=) Prelude.<$> pageSizeBytes,
            ("WriterVersion" Data..=) Prelude.<$> writerVersion
          ]
      )
