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
    psdWriterVersion,
    psdCompression,
    psdMaxPaddingBytes,
    psdEnableDictionaryCompression,
    psdPageSizeBytes,
    psdBlockSizeBytes,
  )
where

import Network.AWS.Firehose.Types.ParquetCompression
import Network.AWS.Firehose.Types.ParquetWriterVersion
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A serializer to use for converting data to the Parquet format before storing it in Amazon S3. For more information, see <https://parquet.apache.org/documentation/latest/ Apache Parquet> .
--
-- /See:/ 'mkParquetSerDe' smart constructor.
data ParquetSerDe = ParquetSerDe'
  { writerVersion ::
      Lude.Maybe ParquetWriterVersion,
    compression :: Lude.Maybe ParquetCompression,
    maxPaddingBytes :: Lude.Maybe Lude.Natural,
    enableDictionaryCompression :: Lude.Maybe Lude.Bool,
    pageSizeBytes :: Lude.Maybe Lude.Natural,
    blockSizeBytes :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParquetSerDe' with the minimum fields required to make a request.
--
-- * 'blockSizeBytes' - The Hadoop Distributed File System (HDFS) block size. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses this value for padding calculations.
-- * 'compression' - The compression code to use over data blocks. The possible values are @UNCOMPRESSED@ , @SNAPPY@ , and @GZIP@ , with the default being @SNAPPY@ . Use @SNAPPY@ for higher decompression speed. Use @GZIP@ if the compression ratio is more important than speed.
-- * 'enableDictionaryCompression' - Indicates whether to enable dictionary compression.
-- * 'maxPaddingBytes' - The maximum amount of padding to apply. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 0.
-- * 'pageSizeBytes' - The Parquet page size. Column chunks are divided into pages. A page is conceptually an indivisible unit (in terms of compression and encoding). The minimum value is 64 KiB and the default is 1 MiB.
-- * 'writerVersion' - Indicates the version of row format to output. The possible values are @V1@ and @V2@ . The default is @V1@ .
mkParquetSerDe ::
  ParquetSerDe
mkParquetSerDe =
  ParquetSerDe'
    { writerVersion = Lude.Nothing,
      compression = Lude.Nothing,
      maxPaddingBytes = Lude.Nothing,
      enableDictionaryCompression = Lude.Nothing,
      pageSizeBytes = Lude.Nothing,
      blockSizeBytes = Lude.Nothing
    }

-- | Indicates the version of row format to output. The possible values are @V1@ and @V2@ . The default is @V1@ .
--
-- /Note:/ Consider using 'writerVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psdWriterVersion :: Lens.Lens' ParquetSerDe (Lude.Maybe ParquetWriterVersion)
psdWriterVersion = Lens.lens (writerVersion :: ParquetSerDe -> Lude.Maybe ParquetWriterVersion) (\s a -> s {writerVersion = a} :: ParquetSerDe)
{-# DEPRECATED psdWriterVersion "Use generic-lens or generic-optics with 'writerVersion' instead." #-}

-- | The compression code to use over data blocks. The possible values are @UNCOMPRESSED@ , @SNAPPY@ , and @GZIP@ , with the default being @SNAPPY@ . Use @SNAPPY@ for higher decompression speed. Use @GZIP@ if the compression ratio is more important than speed.
--
-- /Note:/ Consider using 'compression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psdCompression :: Lens.Lens' ParquetSerDe (Lude.Maybe ParquetCompression)
psdCompression = Lens.lens (compression :: ParquetSerDe -> Lude.Maybe ParquetCompression) (\s a -> s {compression = a} :: ParquetSerDe)
{-# DEPRECATED psdCompression "Use generic-lens or generic-optics with 'compression' instead." #-}

-- | The maximum amount of padding to apply. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 0.
--
-- /Note:/ Consider using 'maxPaddingBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psdMaxPaddingBytes :: Lens.Lens' ParquetSerDe (Lude.Maybe Lude.Natural)
psdMaxPaddingBytes = Lens.lens (maxPaddingBytes :: ParquetSerDe -> Lude.Maybe Lude.Natural) (\s a -> s {maxPaddingBytes = a} :: ParquetSerDe)
{-# DEPRECATED psdMaxPaddingBytes "Use generic-lens or generic-optics with 'maxPaddingBytes' instead." #-}

-- | Indicates whether to enable dictionary compression.
--
-- /Note:/ Consider using 'enableDictionaryCompression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psdEnableDictionaryCompression :: Lens.Lens' ParquetSerDe (Lude.Maybe Lude.Bool)
psdEnableDictionaryCompression = Lens.lens (enableDictionaryCompression :: ParquetSerDe -> Lude.Maybe Lude.Bool) (\s a -> s {enableDictionaryCompression = a} :: ParquetSerDe)
{-# DEPRECATED psdEnableDictionaryCompression "Use generic-lens or generic-optics with 'enableDictionaryCompression' instead." #-}

-- | The Parquet page size. Column chunks are divided into pages. A page is conceptually an indivisible unit (in terms of compression and encoding). The minimum value is 64 KiB and the default is 1 MiB.
--
-- /Note:/ Consider using 'pageSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psdPageSizeBytes :: Lens.Lens' ParquetSerDe (Lude.Maybe Lude.Natural)
psdPageSizeBytes = Lens.lens (pageSizeBytes :: ParquetSerDe -> Lude.Maybe Lude.Natural) (\s a -> s {pageSizeBytes = a} :: ParquetSerDe)
{-# DEPRECATED psdPageSizeBytes "Use generic-lens or generic-optics with 'pageSizeBytes' instead." #-}

-- | The Hadoop Distributed File System (HDFS) block size. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses this value for padding calculations.
--
-- /Note:/ Consider using 'blockSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psdBlockSizeBytes :: Lens.Lens' ParquetSerDe (Lude.Maybe Lude.Natural)
psdBlockSizeBytes = Lens.lens (blockSizeBytes :: ParquetSerDe -> Lude.Maybe Lude.Natural) (\s a -> s {blockSizeBytes = a} :: ParquetSerDe)
{-# DEPRECATED psdBlockSizeBytes "Use generic-lens or generic-optics with 'blockSizeBytes' instead." #-}

instance Lude.FromJSON ParquetSerDe where
  parseJSON =
    Lude.withObject
      "ParquetSerDe"
      ( \x ->
          ParquetSerDe'
            Lude.<$> (x Lude..:? "WriterVersion")
            Lude.<*> (x Lude..:? "Compression")
            Lude.<*> (x Lude..:? "MaxPaddingBytes")
            Lude.<*> (x Lude..:? "EnableDictionaryCompression")
            Lude.<*> (x Lude..:? "PageSizeBytes")
            Lude.<*> (x Lude..:? "BlockSizeBytes")
      )

instance Lude.ToJSON ParquetSerDe where
  toJSON ParquetSerDe' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("WriterVersion" Lude..=) Lude.<$> writerVersion,
            ("Compression" Lude..=) Lude.<$> compression,
            ("MaxPaddingBytes" Lude..=) Lude.<$> maxPaddingBytes,
            ("EnableDictionaryCompression" Lude..=)
              Lude.<$> enableDictionaryCompression,
            ("PageSizeBytes" Lude..=) Lude.<$> pageSizeBytes,
            ("BlockSizeBytes" Lude..=) Lude.<$> blockSizeBytes
          ]
      )
