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
-- Module      : Network.AWS.Firehose.Types.OrcSerDe
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.OrcSerDe where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.OrcCompression
import Network.AWS.Firehose.Types.OrcFormatVersion
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A serializer to use for converting data to the ORC format before storing
-- it in Amazon S3. For more information, see
-- <https://orc.apache.org/docs/ Apache ORC>.
--
-- /See:/ 'newOrcSerDe' smart constructor.
data OrcSerDe = OrcSerDe'
  { -- | The Bloom filter false positive probability (FPP). The lower the FPP,
    -- the bigger the Bloom filter. The default value is 0.05, the minimum is
    -- 0, and the maximum is 1.
    bloomFilterFalsePositiveProbability :: Prelude.Maybe Prelude.Double,
    -- | Represents the fraction of the total number of non-null rows. To turn
    -- off dictionary encoding, set this fraction to a number that is less than
    -- the number of distinct keys in a dictionary. To always use dictionary
    -- encoding, set this threshold to 1.
    dictionaryKeyThreshold :: Prelude.Maybe Prelude.Double,
    -- | Set this to @true@ to indicate that you want stripes to be padded to the
    -- HDFS block boundaries. This is useful if you intend to copy the data
    -- from Amazon S3 to HDFS before querying. The default is @false@.
    enablePadding :: Prelude.Maybe Prelude.Bool,
    -- | The compression code to use over data blocks. The default is @SNAPPY@.
    compression :: Prelude.Maybe OrcCompression,
    -- | The column names for which you want Kinesis Data Firehose to create
    -- bloom filters. The default is @null@.
    bloomFilterColumns :: Prelude.Maybe [Prelude.Text],
    -- | The number of rows between index entries. The default is 10,000 and the
    -- minimum is 1,000.
    rowIndexStride :: Prelude.Maybe Prelude.Natural,
    -- | The version of the file to write. The possible values are @V0_11@ and
    -- @V0_12@. The default is @V0_12@.
    formatVersion :: Prelude.Maybe OrcFormatVersion,
    -- | The Hadoop Distributed File System (HDFS) block size. This is useful if
    -- you intend to copy the data from Amazon S3 to HDFS before querying. The
    -- default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses
    -- this value for padding calculations.
    blockSizeBytes :: Prelude.Maybe Prelude.Natural,
    -- | The number of bytes in each stripe. The default is 64 MiB and the
    -- minimum is 8 MiB.
    stripeSizeBytes :: Prelude.Maybe Prelude.Natural,
    -- | A number between 0 and 1 that defines the tolerance for block padding as
    -- a decimal fraction of stripe size. The default value is 0.05, which
    -- means 5 percent of stripe size.
    --
    -- For the default values of 64 MiB ORC stripes and 256 MiB HDFS blocks,
    -- the default block padding tolerance of 5 percent reserves a maximum of
    -- 3.2 MiB for padding within the 256 MiB block. In such a case, if the
    -- available size within the block is more than 3.2 MiB, a new, smaller
    -- stripe is inserted to fit within that space. This ensures that no stripe
    -- crosses block boundaries and causes remote reads within a node-local
    -- task.
    --
    -- Kinesis Data Firehose ignores this parameter when OrcSerDe$EnablePadding
    -- is @false@.
    paddingTolerance :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrcSerDe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bloomFilterFalsePositiveProbability', 'orcSerDe_bloomFilterFalsePositiveProbability' - The Bloom filter false positive probability (FPP). The lower the FPP,
-- the bigger the Bloom filter. The default value is 0.05, the minimum is
-- 0, and the maximum is 1.
--
-- 'dictionaryKeyThreshold', 'orcSerDe_dictionaryKeyThreshold' - Represents the fraction of the total number of non-null rows. To turn
-- off dictionary encoding, set this fraction to a number that is less than
-- the number of distinct keys in a dictionary. To always use dictionary
-- encoding, set this threshold to 1.
--
-- 'enablePadding', 'orcSerDe_enablePadding' - Set this to @true@ to indicate that you want stripes to be padded to the
-- HDFS block boundaries. This is useful if you intend to copy the data
-- from Amazon S3 to HDFS before querying. The default is @false@.
--
-- 'compression', 'orcSerDe_compression' - The compression code to use over data blocks. The default is @SNAPPY@.
--
-- 'bloomFilterColumns', 'orcSerDe_bloomFilterColumns' - The column names for which you want Kinesis Data Firehose to create
-- bloom filters. The default is @null@.
--
-- 'rowIndexStride', 'orcSerDe_rowIndexStride' - The number of rows between index entries. The default is 10,000 and the
-- minimum is 1,000.
--
-- 'formatVersion', 'orcSerDe_formatVersion' - The version of the file to write. The possible values are @V0_11@ and
-- @V0_12@. The default is @V0_12@.
--
-- 'blockSizeBytes', 'orcSerDe_blockSizeBytes' - The Hadoop Distributed File System (HDFS) block size. This is useful if
-- you intend to copy the data from Amazon S3 to HDFS before querying. The
-- default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses
-- this value for padding calculations.
--
-- 'stripeSizeBytes', 'orcSerDe_stripeSizeBytes' - The number of bytes in each stripe. The default is 64 MiB and the
-- minimum is 8 MiB.
--
-- 'paddingTolerance', 'orcSerDe_paddingTolerance' - A number between 0 and 1 that defines the tolerance for block padding as
-- a decimal fraction of stripe size. The default value is 0.05, which
-- means 5 percent of stripe size.
--
-- For the default values of 64 MiB ORC stripes and 256 MiB HDFS blocks,
-- the default block padding tolerance of 5 percent reserves a maximum of
-- 3.2 MiB for padding within the 256 MiB block. In such a case, if the
-- available size within the block is more than 3.2 MiB, a new, smaller
-- stripe is inserted to fit within that space. This ensures that no stripe
-- crosses block boundaries and causes remote reads within a node-local
-- task.
--
-- Kinesis Data Firehose ignores this parameter when OrcSerDe$EnablePadding
-- is @false@.
newOrcSerDe ::
  OrcSerDe
newOrcSerDe =
  OrcSerDe'
    { bloomFilterFalsePositiveProbability =
        Prelude.Nothing,
      dictionaryKeyThreshold = Prelude.Nothing,
      enablePadding = Prelude.Nothing,
      compression = Prelude.Nothing,
      bloomFilterColumns = Prelude.Nothing,
      rowIndexStride = Prelude.Nothing,
      formatVersion = Prelude.Nothing,
      blockSizeBytes = Prelude.Nothing,
      stripeSizeBytes = Prelude.Nothing,
      paddingTolerance = Prelude.Nothing
    }

-- | The Bloom filter false positive probability (FPP). The lower the FPP,
-- the bigger the Bloom filter. The default value is 0.05, the minimum is
-- 0, and the maximum is 1.
orcSerDe_bloomFilterFalsePositiveProbability :: Lens.Lens' OrcSerDe (Prelude.Maybe Prelude.Double)
orcSerDe_bloomFilterFalsePositiveProbability = Lens.lens (\OrcSerDe' {bloomFilterFalsePositiveProbability} -> bloomFilterFalsePositiveProbability) (\s@OrcSerDe' {} a -> s {bloomFilterFalsePositiveProbability = a} :: OrcSerDe)

-- | Represents the fraction of the total number of non-null rows. To turn
-- off dictionary encoding, set this fraction to a number that is less than
-- the number of distinct keys in a dictionary. To always use dictionary
-- encoding, set this threshold to 1.
orcSerDe_dictionaryKeyThreshold :: Lens.Lens' OrcSerDe (Prelude.Maybe Prelude.Double)
orcSerDe_dictionaryKeyThreshold = Lens.lens (\OrcSerDe' {dictionaryKeyThreshold} -> dictionaryKeyThreshold) (\s@OrcSerDe' {} a -> s {dictionaryKeyThreshold = a} :: OrcSerDe)

-- | Set this to @true@ to indicate that you want stripes to be padded to the
-- HDFS block boundaries. This is useful if you intend to copy the data
-- from Amazon S3 to HDFS before querying. The default is @false@.
orcSerDe_enablePadding :: Lens.Lens' OrcSerDe (Prelude.Maybe Prelude.Bool)
orcSerDe_enablePadding = Lens.lens (\OrcSerDe' {enablePadding} -> enablePadding) (\s@OrcSerDe' {} a -> s {enablePadding = a} :: OrcSerDe)

-- | The compression code to use over data blocks. The default is @SNAPPY@.
orcSerDe_compression :: Lens.Lens' OrcSerDe (Prelude.Maybe OrcCompression)
orcSerDe_compression = Lens.lens (\OrcSerDe' {compression} -> compression) (\s@OrcSerDe' {} a -> s {compression = a} :: OrcSerDe)

-- | The column names for which you want Kinesis Data Firehose to create
-- bloom filters. The default is @null@.
orcSerDe_bloomFilterColumns :: Lens.Lens' OrcSerDe (Prelude.Maybe [Prelude.Text])
orcSerDe_bloomFilterColumns = Lens.lens (\OrcSerDe' {bloomFilterColumns} -> bloomFilterColumns) (\s@OrcSerDe' {} a -> s {bloomFilterColumns = a} :: OrcSerDe) Prelude.. Lens.mapping Lens.coerced

-- | The number of rows between index entries. The default is 10,000 and the
-- minimum is 1,000.
orcSerDe_rowIndexStride :: Lens.Lens' OrcSerDe (Prelude.Maybe Prelude.Natural)
orcSerDe_rowIndexStride = Lens.lens (\OrcSerDe' {rowIndexStride} -> rowIndexStride) (\s@OrcSerDe' {} a -> s {rowIndexStride = a} :: OrcSerDe)

-- | The version of the file to write. The possible values are @V0_11@ and
-- @V0_12@. The default is @V0_12@.
orcSerDe_formatVersion :: Lens.Lens' OrcSerDe (Prelude.Maybe OrcFormatVersion)
orcSerDe_formatVersion = Lens.lens (\OrcSerDe' {formatVersion} -> formatVersion) (\s@OrcSerDe' {} a -> s {formatVersion = a} :: OrcSerDe)

-- | The Hadoop Distributed File System (HDFS) block size. This is useful if
-- you intend to copy the data from Amazon S3 to HDFS before querying. The
-- default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses
-- this value for padding calculations.
orcSerDe_blockSizeBytes :: Lens.Lens' OrcSerDe (Prelude.Maybe Prelude.Natural)
orcSerDe_blockSizeBytes = Lens.lens (\OrcSerDe' {blockSizeBytes} -> blockSizeBytes) (\s@OrcSerDe' {} a -> s {blockSizeBytes = a} :: OrcSerDe)

-- | The number of bytes in each stripe. The default is 64 MiB and the
-- minimum is 8 MiB.
orcSerDe_stripeSizeBytes :: Lens.Lens' OrcSerDe (Prelude.Maybe Prelude.Natural)
orcSerDe_stripeSizeBytes = Lens.lens (\OrcSerDe' {stripeSizeBytes} -> stripeSizeBytes) (\s@OrcSerDe' {} a -> s {stripeSizeBytes = a} :: OrcSerDe)

-- | A number between 0 and 1 that defines the tolerance for block padding as
-- a decimal fraction of stripe size. The default value is 0.05, which
-- means 5 percent of stripe size.
--
-- For the default values of 64 MiB ORC stripes and 256 MiB HDFS blocks,
-- the default block padding tolerance of 5 percent reserves a maximum of
-- 3.2 MiB for padding within the 256 MiB block. In such a case, if the
-- available size within the block is more than 3.2 MiB, a new, smaller
-- stripe is inserted to fit within that space. This ensures that no stripe
-- crosses block boundaries and causes remote reads within a node-local
-- task.
--
-- Kinesis Data Firehose ignores this parameter when OrcSerDe$EnablePadding
-- is @false@.
orcSerDe_paddingTolerance :: Lens.Lens' OrcSerDe (Prelude.Maybe Prelude.Double)
orcSerDe_paddingTolerance = Lens.lens (\OrcSerDe' {paddingTolerance} -> paddingTolerance) (\s@OrcSerDe' {} a -> s {paddingTolerance = a} :: OrcSerDe)

instance Core.FromJSON OrcSerDe where
  parseJSON =
    Core.withObject
      "OrcSerDe"
      ( \x ->
          OrcSerDe'
            Prelude.<$> (x Core..:? "BloomFilterFalsePositiveProbability")
            Prelude.<*> (x Core..:? "DictionaryKeyThreshold")
            Prelude.<*> (x Core..:? "EnablePadding")
            Prelude.<*> (x Core..:? "Compression")
            Prelude.<*> ( x Core..:? "BloomFilterColumns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "RowIndexStride")
            Prelude.<*> (x Core..:? "FormatVersion")
            Prelude.<*> (x Core..:? "BlockSizeBytes")
            Prelude.<*> (x Core..:? "StripeSizeBytes")
            Prelude.<*> (x Core..:? "PaddingTolerance")
      )

instance Prelude.Hashable OrcSerDe

instance Prelude.NFData OrcSerDe

instance Core.ToJSON OrcSerDe where
  toJSON OrcSerDe' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BloomFilterFalsePositiveProbability" Core..=)
              Prelude.<$> bloomFilterFalsePositiveProbability,
            ("DictionaryKeyThreshold" Core..=)
              Prelude.<$> dictionaryKeyThreshold,
            ("EnablePadding" Core..=) Prelude.<$> enablePadding,
            ("Compression" Core..=) Prelude.<$> compression,
            ("BloomFilterColumns" Core..=)
              Prelude.<$> bloomFilterColumns,
            ("RowIndexStride" Core..=)
              Prelude.<$> rowIndexStride,
            ("FormatVersion" Core..=) Prelude.<$> formatVersion,
            ("BlockSizeBytes" Core..=)
              Prelude.<$> blockSizeBytes,
            ("StripeSizeBytes" Core..=)
              Prelude.<$> stripeSizeBytes,
            ("PaddingTolerance" Core..=)
              Prelude.<$> paddingTolerance
          ]
      )
