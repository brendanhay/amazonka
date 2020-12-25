{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ParquetSerDe
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ParquetSerDe
  ( ParquetSerDe (..),

    -- * Smart constructor
    mkParquetSerDe,

    -- * Lenses
    psdBlockSizeBytes,
    psdCompression,
    psdEnableDictionaryCompression,
    psdMaxPaddingBytes,
    psdPageSizeBytes,
    psdWriterVersion,
  )
where

import qualified Network.AWS.Firehose.Types.ParquetCompression as Types
import qualified Network.AWS.Firehose.Types.ParquetWriterVersion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A serializer to use for converting data to the Parquet format before storing it in Amazon S3. For more information, see <https://parquet.apache.org/documentation/latest/ Apache Parquet> .
--
-- /See:/ 'mkParquetSerDe' smart constructor.
data ParquetSerDe = ParquetSerDe'
  { -- | The Hadoop Distributed File System (HDFS) block size. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses this value for padding calculations.
    blockSizeBytes :: Core.Maybe Core.Natural,
    -- | The compression code to use over data blocks. The possible values are @UNCOMPRESSED@ , @SNAPPY@ , and @GZIP@ , with the default being @SNAPPY@ . Use @SNAPPY@ for higher decompression speed. Use @GZIP@ if the compression ratio is more important than speed.
    compression :: Core.Maybe Types.ParquetCompression,
    -- | Indicates whether to enable dictionary compression.
    enableDictionaryCompression :: Core.Maybe Core.Bool,
    -- | The maximum amount of padding to apply. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 0.
    maxPaddingBytes :: Core.Maybe Core.Natural,
    -- | The Parquet page size. Column chunks are divided into pages. A page is conceptually an indivisible unit (in terms of compression and encoding). The minimum value is 64 KiB and the default is 1 MiB.
    pageSizeBytes :: Core.Maybe Core.Natural,
    -- | Indicates the version of row format to output. The possible values are @V1@ and @V2@ . The default is @V1@ .
    writerVersion :: Core.Maybe Types.ParquetWriterVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParquetSerDe' value with any optional fields omitted.
mkParquetSerDe ::
  ParquetSerDe
mkParquetSerDe =
  ParquetSerDe'
    { blockSizeBytes = Core.Nothing,
      compression = Core.Nothing,
      enableDictionaryCompression = Core.Nothing,
      maxPaddingBytes = Core.Nothing,
      pageSizeBytes = Core.Nothing,
      writerVersion = Core.Nothing
    }

-- | The Hadoop Distributed File System (HDFS) block size. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses this value for padding calculations.
--
-- /Note:/ Consider using 'blockSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psdBlockSizeBytes :: Lens.Lens' ParquetSerDe (Core.Maybe Core.Natural)
psdBlockSizeBytes = Lens.field @"blockSizeBytes"
{-# DEPRECATED psdBlockSizeBytes "Use generic-lens or generic-optics with 'blockSizeBytes' instead." #-}

-- | The compression code to use over data blocks. The possible values are @UNCOMPRESSED@ , @SNAPPY@ , and @GZIP@ , with the default being @SNAPPY@ . Use @SNAPPY@ for higher decompression speed. Use @GZIP@ if the compression ratio is more important than speed.
--
-- /Note:/ Consider using 'compression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psdCompression :: Lens.Lens' ParquetSerDe (Core.Maybe Types.ParquetCompression)
psdCompression = Lens.field @"compression"
{-# DEPRECATED psdCompression "Use generic-lens or generic-optics with 'compression' instead." #-}

-- | Indicates whether to enable dictionary compression.
--
-- /Note:/ Consider using 'enableDictionaryCompression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psdEnableDictionaryCompression :: Lens.Lens' ParquetSerDe (Core.Maybe Core.Bool)
psdEnableDictionaryCompression = Lens.field @"enableDictionaryCompression"
{-# DEPRECATED psdEnableDictionaryCompression "Use generic-lens or generic-optics with 'enableDictionaryCompression' instead." #-}

-- | The maximum amount of padding to apply. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 0.
--
-- /Note:/ Consider using 'maxPaddingBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psdMaxPaddingBytes :: Lens.Lens' ParquetSerDe (Core.Maybe Core.Natural)
psdMaxPaddingBytes = Lens.field @"maxPaddingBytes"
{-# DEPRECATED psdMaxPaddingBytes "Use generic-lens or generic-optics with 'maxPaddingBytes' instead." #-}

-- | The Parquet page size. Column chunks are divided into pages. A page is conceptually an indivisible unit (in terms of compression and encoding). The minimum value is 64 KiB and the default is 1 MiB.
--
-- /Note:/ Consider using 'pageSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psdPageSizeBytes :: Lens.Lens' ParquetSerDe (Core.Maybe Core.Natural)
psdPageSizeBytes = Lens.field @"pageSizeBytes"
{-# DEPRECATED psdPageSizeBytes "Use generic-lens or generic-optics with 'pageSizeBytes' instead." #-}

-- | Indicates the version of row format to output. The possible values are @V1@ and @V2@ . The default is @V1@ .
--
-- /Note:/ Consider using 'writerVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psdWriterVersion :: Lens.Lens' ParquetSerDe (Core.Maybe Types.ParquetWriterVersion)
psdWriterVersion = Lens.field @"writerVersion"
{-# DEPRECATED psdWriterVersion "Use generic-lens or generic-optics with 'writerVersion' instead." #-}

instance Core.FromJSON ParquetSerDe where
  toJSON ParquetSerDe {..} =
    Core.object
      ( Core.catMaybes
          [ ("BlockSizeBytes" Core..=) Core.<$> blockSizeBytes,
            ("Compression" Core..=) Core.<$> compression,
            ("EnableDictionaryCompression" Core..=)
              Core.<$> enableDictionaryCompression,
            ("MaxPaddingBytes" Core..=) Core.<$> maxPaddingBytes,
            ("PageSizeBytes" Core..=) Core.<$> pageSizeBytes,
            ("WriterVersion" Core..=) Core.<$> writerVersion
          ]
      )

instance Core.FromJSON ParquetSerDe where
  parseJSON =
    Core.withObject "ParquetSerDe" Core.$
      \x ->
        ParquetSerDe'
          Core.<$> (x Core..:? "BlockSizeBytes")
          Core.<*> (x Core..:? "Compression")
          Core.<*> (x Core..:? "EnableDictionaryCompression")
          Core.<*> (x Core..:? "MaxPaddingBytes")
          Core.<*> (x Core..:? "PageSizeBytes")
          Core.<*> (x Core..:? "WriterVersion")
