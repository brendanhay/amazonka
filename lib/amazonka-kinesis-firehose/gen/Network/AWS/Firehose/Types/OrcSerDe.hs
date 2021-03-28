{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.OrcSerDe
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.OrcSerDe
  ( OrcSerDe (..)
  -- * Smart constructor
  , mkOrcSerDe
  -- * Lenses
  , osdBlockSizeBytes
  , osdBloomFilterColumns
  , osdBloomFilterFalsePositiveProbability
  , osdCompression
  , osdDictionaryKeyThreshold
  , osdEnablePadding
  , osdFormatVersion
  , osdPaddingTolerance
  , osdRowIndexStride
  , osdStripeSizeBytes
  ) where

import qualified Network.AWS.Firehose.Types.NonEmptyStringWithoutWhitespace as Types
import qualified Network.AWS.Firehose.Types.OrcCompression as Types
import qualified Network.AWS.Firehose.Types.OrcFormatVersion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A serializer to use for converting data to the ORC format before storing it in Amazon S3. For more information, see <https://orc.apache.org/docs/ Apache ORC> .
--
-- /See:/ 'mkOrcSerDe' smart constructor.
data OrcSerDe = OrcSerDe'
  { blockSizeBytes :: Core.Maybe Core.Natural
    -- ^ The Hadoop Distributed File System (HDFS) block size. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses this value for padding calculations.
  , bloomFilterColumns :: Core.Maybe [Types.NonEmptyStringWithoutWhitespace]
    -- ^ The column names for which you want Kinesis Data Firehose to create bloom filters. The default is @null@ .
  , bloomFilterFalsePositiveProbability :: Core.Maybe Core.Double
    -- ^ The Bloom filter false positive probability (FPP). The lower the FPP, the bigger the Bloom filter. The default value is 0.05, the minimum is 0, and the maximum is 1.
  , compression :: Core.Maybe Types.OrcCompression
    -- ^ The compression code to use over data blocks. The default is @SNAPPY@ .
  , dictionaryKeyThreshold :: Core.Maybe Core.Double
    -- ^ Represents the fraction of the total number of non-null rows. To turn off dictionary encoding, set this fraction to a number that is less than the number of distinct keys in a dictionary. To always use dictionary encoding, set this threshold to 1.
  , enablePadding :: Core.Maybe Core.Bool
    -- ^ Set this to @true@ to indicate that you want stripes to be padded to the HDFS block boundaries. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is @false@ .
  , formatVersion :: Core.Maybe Types.OrcFormatVersion
    -- ^ The version of the file to write. The possible values are @V0_11@ and @V0_12@ . The default is @V0_12@ .
  , paddingTolerance :: Core.Maybe Core.Double
    -- ^ A number between 0 and 1 that defines the tolerance for block padding as a decimal fraction of stripe size. The default value is 0.05, which means 5 percent of stripe size.
--
-- For the default values of 64 MiB ORC stripes and 256 MiB HDFS blocks, the default block padding tolerance of 5 percent reserves a maximum of 3.2 MiB for padding within the 256 MiB block. In such a case, if the available size within the block is more than 3.2 MiB, a new, smaller stripe is inserted to fit within that space. This ensures that no stripe crosses block boundaries and causes remote reads within a node-local task.
-- Kinesis Data Firehose ignores this parameter when 'OrcSerDe$EnablePadding' is @false@ .
  , rowIndexStride :: Core.Maybe Core.Natural
    -- ^ The number of rows between index entries. The default is 10,000 and the minimum is 1,000.
  , stripeSizeBytes :: Core.Maybe Core.Natural
    -- ^ The number of bytes in each stripe. The default is 64 MiB and the minimum is 8 MiB.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrcSerDe' value with any optional fields omitted.
mkOrcSerDe
    :: OrcSerDe
mkOrcSerDe
  = OrcSerDe'{blockSizeBytes = Core.Nothing,
              bloomFilterColumns = Core.Nothing,
              bloomFilterFalsePositiveProbability = Core.Nothing,
              compression = Core.Nothing, dictionaryKeyThreshold = Core.Nothing,
              enablePadding = Core.Nothing, formatVersion = Core.Nothing,
              paddingTolerance = Core.Nothing, rowIndexStride = Core.Nothing,
              stripeSizeBytes = Core.Nothing}

-- | The Hadoop Distributed File System (HDFS) block size. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses this value for padding calculations.
--
-- /Note:/ Consider using 'blockSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdBlockSizeBytes :: Lens.Lens' OrcSerDe (Core.Maybe Core.Natural)
osdBlockSizeBytes = Lens.field @"blockSizeBytes"
{-# INLINEABLE osdBlockSizeBytes #-}
{-# DEPRECATED blockSizeBytes "Use generic-lens or generic-optics with 'blockSizeBytes' instead"  #-}

-- | The column names for which you want Kinesis Data Firehose to create bloom filters. The default is @null@ .
--
-- /Note:/ Consider using 'bloomFilterColumns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdBloomFilterColumns :: Lens.Lens' OrcSerDe (Core.Maybe [Types.NonEmptyStringWithoutWhitespace])
osdBloomFilterColumns = Lens.field @"bloomFilterColumns"
{-# INLINEABLE osdBloomFilterColumns #-}
{-# DEPRECATED bloomFilterColumns "Use generic-lens or generic-optics with 'bloomFilterColumns' instead"  #-}

-- | The Bloom filter false positive probability (FPP). The lower the FPP, the bigger the Bloom filter. The default value is 0.05, the minimum is 0, and the maximum is 1.
--
-- /Note:/ Consider using 'bloomFilterFalsePositiveProbability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdBloomFilterFalsePositiveProbability :: Lens.Lens' OrcSerDe (Core.Maybe Core.Double)
osdBloomFilterFalsePositiveProbability = Lens.field @"bloomFilterFalsePositiveProbability"
{-# INLINEABLE osdBloomFilterFalsePositiveProbability #-}
{-# DEPRECATED bloomFilterFalsePositiveProbability "Use generic-lens or generic-optics with 'bloomFilterFalsePositiveProbability' instead"  #-}

-- | The compression code to use over data blocks. The default is @SNAPPY@ .
--
-- /Note:/ Consider using 'compression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdCompression :: Lens.Lens' OrcSerDe (Core.Maybe Types.OrcCompression)
osdCompression = Lens.field @"compression"
{-# INLINEABLE osdCompression #-}
{-# DEPRECATED compression "Use generic-lens or generic-optics with 'compression' instead"  #-}

-- | Represents the fraction of the total number of non-null rows. To turn off dictionary encoding, set this fraction to a number that is less than the number of distinct keys in a dictionary. To always use dictionary encoding, set this threshold to 1.
--
-- /Note:/ Consider using 'dictionaryKeyThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdDictionaryKeyThreshold :: Lens.Lens' OrcSerDe (Core.Maybe Core.Double)
osdDictionaryKeyThreshold = Lens.field @"dictionaryKeyThreshold"
{-# INLINEABLE osdDictionaryKeyThreshold #-}
{-# DEPRECATED dictionaryKeyThreshold "Use generic-lens or generic-optics with 'dictionaryKeyThreshold' instead"  #-}

-- | Set this to @true@ to indicate that you want stripes to be padded to the HDFS block boundaries. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is @false@ .
--
-- /Note:/ Consider using 'enablePadding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdEnablePadding :: Lens.Lens' OrcSerDe (Core.Maybe Core.Bool)
osdEnablePadding = Lens.field @"enablePadding"
{-# INLINEABLE osdEnablePadding #-}
{-# DEPRECATED enablePadding "Use generic-lens or generic-optics with 'enablePadding' instead"  #-}

-- | The version of the file to write. The possible values are @V0_11@ and @V0_12@ . The default is @V0_12@ .
--
-- /Note:/ Consider using 'formatVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdFormatVersion :: Lens.Lens' OrcSerDe (Core.Maybe Types.OrcFormatVersion)
osdFormatVersion = Lens.field @"formatVersion"
{-# INLINEABLE osdFormatVersion #-}
{-# DEPRECATED formatVersion "Use generic-lens or generic-optics with 'formatVersion' instead"  #-}

-- | A number between 0 and 1 that defines the tolerance for block padding as a decimal fraction of stripe size. The default value is 0.05, which means 5 percent of stripe size.
--
-- For the default values of 64 MiB ORC stripes and 256 MiB HDFS blocks, the default block padding tolerance of 5 percent reserves a maximum of 3.2 MiB for padding within the 256 MiB block. In such a case, if the available size within the block is more than 3.2 MiB, a new, smaller stripe is inserted to fit within that space. This ensures that no stripe crosses block boundaries and causes remote reads within a node-local task.
-- Kinesis Data Firehose ignores this parameter when 'OrcSerDe$EnablePadding' is @false@ .
--
-- /Note:/ Consider using 'paddingTolerance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdPaddingTolerance :: Lens.Lens' OrcSerDe (Core.Maybe Core.Double)
osdPaddingTolerance = Lens.field @"paddingTolerance"
{-# INLINEABLE osdPaddingTolerance #-}
{-# DEPRECATED paddingTolerance "Use generic-lens or generic-optics with 'paddingTolerance' instead"  #-}

-- | The number of rows between index entries. The default is 10,000 and the minimum is 1,000.
--
-- /Note:/ Consider using 'rowIndexStride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdRowIndexStride :: Lens.Lens' OrcSerDe (Core.Maybe Core.Natural)
osdRowIndexStride = Lens.field @"rowIndexStride"
{-# INLINEABLE osdRowIndexStride #-}
{-# DEPRECATED rowIndexStride "Use generic-lens or generic-optics with 'rowIndexStride' instead"  #-}

-- | The number of bytes in each stripe. The default is 64 MiB and the minimum is 8 MiB.
--
-- /Note:/ Consider using 'stripeSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osdStripeSizeBytes :: Lens.Lens' OrcSerDe (Core.Maybe Core.Natural)
osdStripeSizeBytes = Lens.field @"stripeSizeBytes"
{-# INLINEABLE osdStripeSizeBytes #-}
{-# DEPRECATED stripeSizeBytes "Use generic-lens or generic-optics with 'stripeSizeBytes' instead"  #-}

instance Core.FromJSON OrcSerDe where
        toJSON OrcSerDe{..}
          = Core.object
              (Core.catMaybes
                 [("BlockSizeBytes" Core..=) Core.<$> blockSizeBytes,
                  ("BloomFilterColumns" Core..=) Core.<$> bloomFilterColumns,
                  ("BloomFilterFalsePositiveProbability" Core..=) Core.<$>
                    bloomFilterFalsePositiveProbability,
                  ("Compression" Core..=) Core.<$> compression,
                  ("DictionaryKeyThreshold" Core..=) Core.<$> dictionaryKeyThreshold,
                  ("EnablePadding" Core..=) Core.<$> enablePadding,
                  ("FormatVersion" Core..=) Core.<$> formatVersion,
                  ("PaddingTolerance" Core..=) Core.<$> paddingTolerance,
                  ("RowIndexStride" Core..=) Core.<$> rowIndexStride,
                  ("StripeSizeBytes" Core..=) Core.<$> stripeSizeBytes])

instance Core.FromJSON OrcSerDe where
        parseJSON
          = Core.withObject "OrcSerDe" Core.$
              \ x ->
                OrcSerDe' Core.<$>
                  (x Core..:? "BlockSizeBytes") Core.<*>
                    x Core..:? "BloomFilterColumns"
                    Core.<*> x Core..:? "BloomFilterFalsePositiveProbability"
                    Core.<*> x Core..:? "Compression"
                    Core.<*> x Core..:? "DictionaryKeyThreshold"
                    Core.<*> x Core..:? "EnablePadding"
                    Core.<*> x Core..:? "FormatVersion"
                    Core.<*> x Core..:? "PaddingTolerance"
                    Core.<*> x Core..:? "RowIndexStride"
                    Core.<*> x Core..:? "StripeSizeBytes"
