{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.OrcSerDe
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.OrcSerDe
  ( OrcSerDe (..),

    -- * Smart constructor
    mkOrcSerDe,

    -- * Lenses
    osdBloomFilterFalsePositiveProbability,
    osdDictionaryKeyThreshold,
    osdEnablePadding,
    osdCompression,
    osdBloomFilterColumns,
    osdRowIndexStride,
    osdFormatVersion,
    osdBlockSizeBytes,
    osdStripeSizeBytes,
    osdPaddingTolerance,
  )
where

import Network.AWS.Firehose.Types.OrcCompression
import Network.AWS.Firehose.Types.OrcFormatVersion
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A serializer to use for converting data to the ORC format before storing it in Amazon S3. For more information, see <https://orc.apache.org/docs/ Apache ORC> .
--
-- /See:/ 'mkOrcSerDe' smart constructor.
data OrcSerDe = OrcSerDe'
  { bloomFilterFalsePositiveProbability ::
      Lude.Maybe Lude.Double,
    dictionaryKeyThreshold :: Lude.Maybe Lude.Double,
    enablePadding :: Lude.Maybe Lude.Bool,
    compression :: Lude.Maybe OrcCompression,
    bloomFilterColumns :: Lude.Maybe [Lude.Text],
    rowIndexStride :: Lude.Maybe Lude.Natural,
    formatVersion :: Lude.Maybe OrcFormatVersion,
    blockSizeBytes :: Lude.Maybe Lude.Natural,
    stripeSizeBytes :: Lude.Maybe Lude.Natural,
    paddingTolerance :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrcSerDe' with the minimum fields required to make a request.
--
-- * 'blockSizeBytes' - The Hadoop Distributed File System (HDFS) block size. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses this value for padding calculations.
-- * 'bloomFilterColumns' - The column names for which you want Kinesis Data Firehose to create bloom filters. The default is @null@ .
-- * 'bloomFilterFalsePositiveProbability' - The Bloom filter false positive probability (FPP). The lower the FPP, the bigger the Bloom filter. The default value is 0.05, the minimum is 0, and the maximum is 1.
-- * 'compression' - The compression code to use over data blocks. The default is @SNAPPY@ .
-- * 'dictionaryKeyThreshold' - Represents the fraction of the total number of non-null rows. To turn off dictionary encoding, set this fraction to a number that is less than the number of distinct keys in a dictionary. To always use dictionary encoding, set this threshold to 1.
-- * 'enablePadding' - Set this to @true@ to indicate that you want stripes to be padded to the HDFS block boundaries. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is @false@ .
-- * 'formatVersion' - The version of the file to write. The possible values are @V0_11@ and @V0_12@ . The default is @V0_12@ .
-- * 'paddingTolerance' - A number between 0 and 1 that defines the tolerance for block padding as a decimal fraction of stripe size. The default value is 0.05, which means 5 percent of stripe size.
--
-- For the default values of 64 MiB ORC stripes and 256 MiB HDFS blocks, the default block padding tolerance of 5 percent reserves a maximum of 3.2 MiB for padding within the 256 MiB block. In such a case, if the available size within the block is more than 3.2 MiB, a new, smaller stripe is inserted to fit within that space. This ensures that no stripe crosses block boundaries and causes remote reads within a node-local task.
-- Kinesis Data Firehose ignores this parameter when 'OrcSerDe$EnablePadding' is @false@ .
-- * 'rowIndexStride' - The number of rows between index entries. The default is 10,000 and the minimum is 1,000.
-- * 'stripeSizeBytes' - The number of bytes in each stripe. The default is 64 MiB and the minimum is 8 MiB.
mkOrcSerDe ::
  OrcSerDe
mkOrcSerDe =
  OrcSerDe'
    { bloomFilterFalsePositiveProbability = Lude.Nothing,
      dictionaryKeyThreshold = Lude.Nothing,
      enablePadding = Lude.Nothing,
      compression = Lude.Nothing,
      bloomFilterColumns = Lude.Nothing,
      rowIndexStride = Lude.Nothing,
      formatVersion = Lude.Nothing,
      blockSizeBytes = Lude.Nothing,
      stripeSizeBytes = Lude.Nothing,
      paddingTolerance = Lude.Nothing
    }

-- | The Bloom filter false positive probability (FPP). The lower the FPP, the bigger the Bloom filter. The default value is 0.05, the minimum is 0, and the maximum is 1.
--
-- /Note:/ Consider using 'bloomFilterFalsePositiveProbability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdBloomFilterFalsePositiveProbability :: Lens.Lens' OrcSerDe (Lude.Maybe Lude.Double)
osdBloomFilterFalsePositiveProbability = Lens.lens (bloomFilterFalsePositiveProbability :: OrcSerDe -> Lude.Maybe Lude.Double) (\s a -> s {bloomFilterFalsePositiveProbability = a} :: OrcSerDe)
{-# DEPRECATED osdBloomFilterFalsePositiveProbability "Use generic-lens or generic-optics with 'bloomFilterFalsePositiveProbability' instead." #-}

-- | Represents the fraction of the total number of non-null rows. To turn off dictionary encoding, set this fraction to a number that is less than the number of distinct keys in a dictionary. To always use dictionary encoding, set this threshold to 1.
--
-- /Note:/ Consider using 'dictionaryKeyThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdDictionaryKeyThreshold :: Lens.Lens' OrcSerDe (Lude.Maybe Lude.Double)
osdDictionaryKeyThreshold = Lens.lens (dictionaryKeyThreshold :: OrcSerDe -> Lude.Maybe Lude.Double) (\s a -> s {dictionaryKeyThreshold = a} :: OrcSerDe)
{-# DEPRECATED osdDictionaryKeyThreshold "Use generic-lens or generic-optics with 'dictionaryKeyThreshold' instead." #-}

-- | Set this to @true@ to indicate that you want stripes to be padded to the HDFS block boundaries. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is @false@ .
--
-- /Note:/ Consider using 'enablePadding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdEnablePadding :: Lens.Lens' OrcSerDe (Lude.Maybe Lude.Bool)
osdEnablePadding = Lens.lens (enablePadding :: OrcSerDe -> Lude.Maybe Lude.Bool) (\s a -> s {enablePadding = a} :: OrcSerDe)
{-# DEPRECATED osdEnablePadding "Use generic-lens or generic-optics with 'enablePadding' instead." #-}

-- | The compression code to use over data blocks. The default is @SNAPPY@ .
--
-- /Note:/ Consider using 'compression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdCompression :: Lens.Lens' OrcSerDe (Lude.Maybe OrcCompression)
osdCompression = Lens.lens (compression :: OrcSerDe -> Lude.Maybe OrcCompression) (\s a -> s {compression = a} :: OrcSerDe)
{-# DEPRECATED osdCompression "Use generic-lens or generic-optics with 'compression' instead." #-}

-- | The column names for which you want Kinesis Data Firehose to create bloom filters. The default is @null@ .
--
-- /Note:/ Consider using 'bloomFilterColumns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdBloomFilterColumns :: Lens.Lens' OrcSerDe (Lude.Maybe [Lude.Text])
osdBloomFilterColumns = Lens.lens (bloomFilterColumns :: OrcSerDe -> Lude.Maybe [Lude.Text]) (\s a -> s {bloomFilterColumns = a} :: OrcSerDe)
{-# DEPRECATED osdBloomFilterColumns "Use generic-lens or generic-optics with 'bloomFilterColumns' instead." #-}

-- | The number of rows between index entries. The default is 10,000 and the minimum is 1,000.
--
-- /Note:/ Consider using 'rowIndexStride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdRowIndexStride :: Lens.Lens' OrcSerDe (Lude.Maybe Lude.Natural)
osdRowIndexStride = Lens.lens (rowIndexStride :: OrcSerDe -> Lude.Maybe Lude.Natural) (\s a -> s {rowIndexStride = a} :: OrcSerDe)
{-# DEPRECATED osdRowIndexStride "Use generic-lens or generic-optics with 'rowIndexStride' instead." #-}

-- | The version of the file to write. The possible values are @V0_11@ and @V0_12@ . The default is @V0_12@ .
--
-- /Note:/ Consider using 'formatVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdFormatVersion :: Lens.Lens' OrcSerDe (Lude.Maybe OrcFormatVersion)
osdFormatVersion = Lens.lens (formatVersion :: OrcSerDe -> Lude.Maybe OrcFormatVersion) (\s a -> s {formatVersion = a} :: OrcSerDe)
{-# DEPRECATED osdFormatVersion "Use generic-lens or generic-optics with 'formatVersion' instead." #-}

-- | The Hadoop Distributed File System (HDFS) block size. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses this value for padding calculations.
--
-- /Note:/ Consider using 'blockSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdBlockSizeBytes :: Lens.Lens' OrcSerDe (Lude.Maybe Lude.Natural)
osdBlockSizeBytes = Lens.lens (blockSizeBytes :: OrcSerDe -> Lude.Maybe Lude.Natural) (\s a -> s {blockSizeBytes = a} :: OrcSerDe)
{-# DEPRECATED osdBlockSizeBytes "Use generic-lens or generic-optics with 'blockSizeBytes' instead." #-}

-- | The number of bytes in each stripe. The default is 64 MiB and the minimum is 8 MiB.
--
-- /Note:/ Consider using 'stripeSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdStripeSizeBytes :: Lens.Lens' OrcSerDe (Lude.Maybe Lude.Natural)
osdStripeSizeBytes = Lens.lens (stripeSizeBytes :: OrcSerDe -> Lude.Maybe Lude.Natural) (\s a -> s {stripeSizeBytes = a} :: OrcSerDe)
{-# DEPRECATED osdStripeSizeBytes "Use generic-lens or generic-optics with 'stripeSizeBytes' instead." #-}

-- | A number between 0 and 1 that defines the tolerance for block padding as a decimal fraction of stripe size. The default value is 0.05, which means 5 percent of stripe size.
--
-- For the default values of 64 MiB ORC stripes and 256 MiB HDFS blocks, the default block padding tolerance of 5 percent reserves a maximum of 3.2 MiB for padding within the 256 MiB block. In such a case, if the available size within the block is more than 3.2 MiB, a new, smaller stripe is inserted to fit within that space. This ensures that no stripe crosses block boundaries and causes remote reads within a node-local task.
-- Kinesis Data Firehose ignores this parameter when 'OrcSerDe$EnablePadding' is @false@ .
--
-- /Note:/ Consider using 'paddingTolerance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdPaddingTolerance :: Lens.Lens' OrcSerDe (Lude.Maybe Lude.Double)
osdPaddingTolerance = Lens.lens (paddingTolerance :: OrcSerDe -> Lude.Maybe Lude.Double) (\s a -> s {paddingTolerance = a} :: OrcSerDe)
{-# DEPRECATED osdPaddingTolerance "Use generic-lens or generic-optics with 'paddingTolerance' instead." #-}

instance Lude.FromJSON OrcSerDe where
  parseJSON =
    Lude.withObject
      "OrcSerDe"
      ( \x ->
          OrcSerDe'
            Lude.<$> (x Lude..:? "BloomFilterFalsePositiveProbability")
            Lude.<*> (x Lude..:? "DictionaryKeyThreshold")
            Lude.<*> (x Lude..:? "EnablePadding")
            Lude.<*> (x Lude..:? "Compression")
            Lude.<*> (x Lude..:? "BloomFilterColumns" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "RowIndexStride")
            Lude.<*> (x Lude..:? "FormatVersion")
            Lude.<*> (x Lude..:? "BlockSizeBytes")
            Lude.<*> (x Lude..:? "StripeSizeBytes")
            Lude.<*> (x Lude..:? "PaddingTolerance")
      )

instance Lude.ToJSON OrcSerDe where
  toJSON OrcSerDe' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BloomFilterFalsePositiveProbability" Lude..=)
              Lude.<$> bloomFilterFalsePositiveProbability,
            ("DictionaryKeyThreshold" Lude..=) Lude.<$> dictionaryKeyThreshold,
            ("EnablePadding" Lude..=) Lude.<$> enablePadding,
            ("Compression" Lude..=) Lude.<$> compression,
            ("BloomFilterColumns" Lude..=) Lude.<$> bloomFilterColumns,
            ("RowIndexStride" Lude..=) Lude.<$> rowIndexStride,
            ("FormatVersion" Lude..=) Lude.<$> formatVersion,
            ("BlockSizeBytes" Lude..=) Lude.<$> blockSizeBytes,
            ("StripeSizeBytes" Lude..=) Lude.<$> stripeSizeBytes,
            ("PaddingTolerance" Lude..=) Lude.<$> paddingTolerance
          ]
      )
