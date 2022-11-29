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
-- Module      : Amazonka.Glue.Types.S3CsvSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3CsvSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.CompressionType
import Amazonka.Glue.Types.GlueSchema
import Amazonka.Glue.Types.QuoteChar
import Amazonka.Glue.Types.S3DirectSourceAdditionalOptions
import Amazonka.Glue.Types.Separator
import qualified Amazonka.Prelude as Prelude

-- | Specifies a command-separated value (CSV) data store stored in Amazon
-- S3.
--
-- /See:/ 'newS3CsvSource' smart constructor.
data S3CsvSource = S3CsvSource'
  { -- | Specifies the data schema for the S3 CSV source.
    outputSchemas :: Prelude.Maybe [GlueSchema],
    -- | Grouping files is turned on by default when the input contains more than
    -- 50,000 files. To turn on grouping with fewer than 50,000 files, set this
    -- parameter to \"inPartition\". To disable grouping when there are more
    -- than 50,000 files, set this parameter to @\"none\"@.
    groupFiles :: Prelude.Maybe Prelude.Text,
    -- | This option controls the duration in milliseconds after which the s3
    -- listing is likely to be consistent. Files with modification timestamps
    -- falling within the last maxBand milliseconds are tracked specially when
    -- using JobBookmarks to account for Amazon S3 eventual consistency. Most
    -- users don\'t need to set this option. The default is 900000
    -- milliseconds, or 15 minutes.
    maxBand :: Prelude.Maybe Prelude.Natural,
    -- | A Boolean value that specifies whether to write the header to output.
    -- The default value is @True@.
    writeHeader :: Prelude.Maybe Prelude.Bool,
    -- | This option specifies the maximum number of files to save from the last
    -- maxBand seconds. If this number is exceeded, extra files are skipped and
    -- only processed in the next job run.
    maxFilesInBand :: Prelude.Maybe Prelude.Natural,
    -- | A Boolean value that specifies whether to use the advanced SIMD CSV
    -- reader along with Apache Arrow based columnar memory formats. Only
    -- available in Glue version 3.0.
    optimizePerformance :: Prelude.Maybe Prelude.Bool,
    -- | If set to true, recursively reads files in all subdirectories under the
    -- specified paths.
    recurse :: Prelude.Maybe Prelude.Bool,
    -- | Specifies additional connection options.
    additionalOptions :: Prelude.Maybe S3DirectSourceAdditionalOptions,
    -- | Specifies how the data is compressed. This is generally not necessary if
    -- the data has a standard file extension. Possible values are @\"gzip\"@
    -- and @\"bzip\"@).
    compressionType :: Prelude.Maybe CompressionType,
    -- | A Boolean value that specifies whether to skip the first data line. The
    -- default value is @False@.
    skipFirst :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value that specifies whether to treat the first line as a
    -- header. The default value is @False@.
    withHeader :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value that specifies whether a single record can span multiple
    -- lines. This can occur when a field contains a quoted new-line character.
    -- You must set this option to True if any record spans multiple lines. The
    -- default value is @False@, which allows for more aggressive
    -- file-splitting during parsing.
    multiline :: Prelude.Maybe Prelude.Bool,
    -- | A string containing a JSON list of Unix-style glob patterns to exclude.
    -- For example, \"[\\\"**.pdf\\\"]\" excludes all PDF files.
    exclusions :: Prelude.Maybe [Prelude.Text],
    -- | The target group size in bytes. The default is computed based on the
    -- input data size and the size of your cluster. When there are fewer than
    -- 50,000 input files, @\"groupFiles\"@ must be set to @\"inPartition\"@
    -- for this to take effect.
    groupSize :: Prelude.Maybe Prelude.Text,
    -- | Specifies a character to use for escaping. This option is used only when
    -- reading CSV files. The default value is @none@. If enabled, the
    -- character which immediately follows is used as-is, except for a small
    -- set of well-known escapes (@\\n@, @\\r@, @\\t@, and @\\0@).
    escaper :: Prelude.Maybe Prelude.Text,
    -- | The name of the data store.
    name :: Prelude.Text,
    -- | A list of the Amazon S3 paths to read from.
    paths :: [Prelude.Text],
    -- | Specifies the delimiter character. The default is a comma: \",\", but
    -- any other character can be specified.
    separator :: Separator,
    -- | Specifies the character to use for quoting. The default is a double
    -- quote: @\'\"\'@. Set this to @-1@ to turn off quoting entirely.
    quoteChar :: QuoteChar
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3CsvSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputSchemas', 's3CsvSource_outputSchemas' - Specifies the data schema for the S3 CSV source.
--
-- 'groupFiles', 's3CsvSource_groupFiles' - Grouping files is turned on by default when the input contains more than
-- 50,000 files. To turn on grouping with fewer than 50,000 files, set this
-- parameter to \"inPartition\". To disable grouping when there are more
-- than 50,000 files, set this parameter to @\"none\"@.
--
-- 'maxBand', 's3CsvSource_maxBand' - This option controls the duration in milliseconds after which the s3
-- listing is likely to be consistent. Files with modification timestamps
-- falling within the last maxBand milliseconds are tracked specially when
-- using JobBookmarks to account for Amazon S3 eventual consistency. Most
-- users don\'t need to set this option. The default is 900000
-- milliseconds, or 15 minutes.
--
-- 'writeHeader', 's3CsvSource_writeHeader' - A Boolean value that specifies whether to write the header to output.
-- The default value is @True@.
--
-- 'maxFilesInBand', 's3CsvSource_maxFilesInBand' - This option specifies the maximum number of files to save from the last
-- maxBand seconds. If this number is exceeded, extra files are skipped and
-- only processed in the next job run.
--
-- 'optimizePerformance', 's3CsvSource_optimizePerformance' - A Boolean value that specifies whether to use the advanced SIMD CSV
-- reader along with Apache Arrow based columnar memory formats. Only
-- available in Glue version 3.0.
--
-- 'recurse', 's3CsvSource_recurse' - If set to true, recursively reads files in all subdirectories under the
-- specified paths.
--
-- 'additionalOptions', 's3CsvSource_additionalOptions' - Specifies additional connection options.
--
-- 'compressionType', 's3CsvSource_compressionType' - Specifies how the data is compressed. This is generally not necessary if
-- the data has a standard file extension. Possible values are @\"gzip\"@
-- and @\"bzip\"@).
--
-- 'skipFirst', 's3CsvSource_skipFirst' - A Boolean value that specifies whether to skip the first data line. The
-- default value is @False@.
--
-- 'withHeader', 's3CsvSource_withHeader' - A Boolean value that specifies whether to treat the first line as a
-- header. The default value is @False@.
--
-- 'multiline', 's3CsvSource_multiline' - A Boolean value that specifies whether a single record can span multiple
-- lines. This can occur when a field contains a quoted new-line character.
-- You must set this option to True if any record spans multiple lines. The
-- default value is @False@, which allows for more aggressive
-- file-splitting during parsing.
--
-- 'exclusions', 's3CsvSource_exclusions' - A string containing a JSON list of Unix-style glob patterns to exclude.
-- For example, \"[\\\"**.pdf\\\"]\" excludes all PDF files.
--
-- 'groupSize', 's3CsvSource_groupSize' - The target group size in bytes. The default is computed based on the
-- input data size and the size of your cluster. When there are fewer than
-- 50,000 input files, @\"groupFiles\"@ must be set to @\"inPartition\"@
-- for this to take effect.
--
-- 'escaper', 's3CsvSource_escaper' - Specifies a character to use for escaping. This option is used only when
-- reading CSV files. The default value is @none@. If enabled, the
-- character which immediately follows is used as-is, except for a small
-- set of well-known escapes (@\\n@, @\\r@, @\\t@, and @\\0@).
--
-- 'name', 's3CsvSource_name' - The name of the data store.
--
-- 'paths', 's3CsvSource_paths' - A list of the Amazon S3 paths to read from.
--
-- 'separator', 's3CsvSource_separator' - Specifies the delimiter character. The default is a comma: \",\", but
-- any other character can be specified.
--
-- 'quoteChar', 's3CsvSource_quoteChar' - Specifies the character to use for quoting. The default is a double
-- quote: @\'\"\'@. Set this to @-1@ to turn off quoting entirely.
newS3CsvSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'separator'
  Separator ->
  -- | 'quoteChar'
  QuoteChar ->
  S3CsvSource
newS3CsvSource pName_ pSeparator_ pQuoteChar_ =
  S3CsvSource'
    { outputSchemas = Prelude.Nothing,
      groupFiles = Prelude.Nothing,
      maxBand = Prelude.Nothing,
      writeHeader = Prelude.Nothing,
      maxFilesInBand = Prelude.Nothing,
      optimizePerformance = Prelude.Nothing,
      recurse = Prelude.Nothing,
      additionalOptions = Prelude.Nothing,
      compressionType = Prelude.Nothing,
      skipFirst = Prelude.Nothing,
      withHeader = Prelude.Nothing,
      multiline = Prelude.Nothing,
      exclusions = Prelude.Nothing,
      groupSize = Prelude.Nothing,
      escaper = Prelude.Nothing,
      name = pName_,
      paths = Prelude.mempty,
      separator = pSeparator_,
      quoteChar = pQuoteChar_
    }

-- | Specifies the data schema for the S3 CSV source.
s3CsvSource_outputSchemas :: Lens.Lens' S3CsvSource (Prelude.Maybe [GlueSchema])
s3CsvSource_outputSchemas = Lens.lens (\S3CsvSource' {outputSchemas} -> outputSchemas) (\s@S3CsvSource' {} a -> s {outputSchemas = a} :: S3CsvSource) Prelude.. Lens.mapping Lens.coerced

-- | Grouping files is turned on by default when the input contains more than
-- 50,000 files. To turn on grouping with fewer than 50,000 files, set this
-- parameter to \"inPartition\". To disable grouping when there are more
-- than 50,000 files, set this parameter to @\"none\"@.
s3CsvSource_groupFiles :: Lens.Lens' S3CsvSource (Prelude.Maybe Prelude.Text)
s3CsvSource_groupFiles = Lens.lens (\S3CsvSource' {groupFiles} -> groupFiles) (\s@S3CsvSource' {} a -> s {groupFiles = a} :: S3CsvSource)

-- | This option controls the duration in milliseconds after which the s3
-- listing is likely to be consistent. Files with modification timestamps
-- falling within the last maxBand milliseconds are tracked specially when
-- using JobBookmarks to account for Amazon S3 eventual consistency. Most
-- users don\'t need to set this option. The default is 900000
-- milliseconds, or 15 minutes.
s3CsvSource_maxBand :: Lens.Lens' S3CsvSource (Prelude.Maybe Prelude.Natural)
s3CsvSource_maxBand = Lens.lens (\S3CsvSource' {maxBand} -> maxBand) (\s@S3CsvSource' {} a -> s {maxBand = a} :: S3CsvSource)

-- | A Boolean value that specifies whether to write the header to output.
-- The default value is @True@.
s3CsvSource_writeHeader :: Lens.Lens' S3CsvSource (Prelude.Maybe Prelude.Bool)
s3CsvSource_writeHeader = Lens.lens (\S3CsvSource' {writeHeader} -> writeHeader) (\s@S3CsvSource' {} a -> s {writeHeader = a} :: S3CsvSource)

-- | This option specifies the maximum number of files to save from the last
-- maxBand seconds. If this number is exceeded, extra files are skipped and
-- only processed in the next job run.
s3CsvSource_maxFilesInBand :: Lens.Lens' S3CsvSource (Prelude.Maybe Prelude.Natural)
s3CsvSource_maxFilesInBand = Lens.lens (\S3CsvSource' {maxFilesInBand} -> maxFilesInBand) (\s@S3CsvSource' {} a -> s {maxFilesInBand = a} :: S3CsvSource)

-- | A Boolean value that specifies whether to use the advanced SIMD CSV
-- reader along with Apache Arrow based columnar memory formats. Only
-- available in Glue version 3.0.
s3CsvSource_optimizePerformance :: Lens.Lens' S3CsvSource (Prelude.Maybe Prelude.Bool)
s3CsvSource_optimizePerformance = Lens.lens (\S3CsvSource' {optimizePerformance} -> optimizePerformance) (\s@S3CsvSource' {} a -> s {optimizePerformance = a} :: S3CsvSource)

-- | If set to true, recursively reads files in all subdirectories under the
-- specified paths.
s3CsvSource_recurse :: Lens.Lens' S3CsvSource (Prelude.Maybe Prelude.Bool)
s3CsvSource_recurse = Lens.lens (\S3CsvSource' {recurse} -> recurse) (\s@S3CsvSource' {} a -> s {recurse = a} :: S3CsvSource)

-- | Specifies additional connection options.
s3CsvSource_additionalOptions :: Lens.Lens' S3CsvSource (Prelude.Maybe S3DirectSourceAdditionalOptions)
s3CsvSource_additionalOptions = Lens.lens (\S3CsvSource' {additionalOptions} -> additionalOptions) (\s@S3CsvSource' {} a -> s {additionalOptions = a} :: S3CsvSource)

-- | Specifies how the data is compressed. This is generally not necessary if
-- the data has a standard file extension. Possible values are @\"gzip\"@
-- and @\"bzip\"@).
s3CsvSource_compressionType :: Lens.Lens' S3CsvSource (Prelude.Maybe CompressionType)
s3CsvSource_compressionType = Lens.lens (\S3CsvSource' {compressionType} -> compressionType) (\s@S3CsvSource' {} a -> s {compressionType = a} :: S3CsvSource)

-- | A Boolean value that specifies whether to skip the first data line. The
-- default value is @False@.
s3CsvSource_skipFirst :: Lens.Lens' S3CsvSource (Prelude.Maybe Prelude.Bool)
s3CsvSource_skipFirst = Lens.lens (\S3CsvSource' {skipFirst} -> skipFirst) (\s@S3CsvSource' {} a -> s {skipFirst = a} :: S3CsvSource)

-- | A Boolean value that specifies whether to treat the first line as a
-- header. The default value is @False@.
s3CsvSource_withHeader :: Lens.Lens' S3CsvSource (Prelude.Maybe Prelude.Bool)
s3CsvSource_withHeader = Lens.lens (\S3CsvSource' {withHeader} -> withHeader) (\s@S3CsvSource' {} a -> s {withHeader = a} :: S3CsvSource)

-- | A Boolean value that specifies whether a single record can span multiple
-- lines. This can occur when a field contains a quoted new-line character.
-- You must set this option to True if any record spans multiple lines. The
-- default value is @False@, which allows for more aggressive
-- file-splitting during parsing.
s3CsvSource_multiline :: Lens.Lens' S3CsvSource (Prelude.Maybe Prelude.Bool)
s3CsvSource_multiline = Lens.lens (\S3CsvSource' {multiline} -> multiline) (\s@S3CsvSource' {} a -> s {multiline = a} :: S3CsvSource)

-- | A string containing a JSON list of Unix-style glob patterns to exclude.
-- For example, \"[\\\"**.pdf\\\"]\" excludes all PDF files.
s3CsvSource_exclusions :: Lens.Lens' S3CsvSource (Prelude.Maybe [Prelude.Text])
s3CsvSource_exclusions = Lens.lens (\S3CsvSource' {exclusions} -> exclusions) (\s@S3CsvSource' {} a -> s {exclusions = a} :: S3CsvSource) Prelude.. Lens.mapping Lens.coerced

-- | The target group size in bytes. The default is computed based on the
-- input data size and the size of your cluster. When there are fewer than
-- 50,000 input files, @\"groupFiles\"@ must be set to @\"inPartition\"@
-- for this to take effect.
s3CsvSource_groupSize :: Lens.Lens' S3CsvSource (Prelude.Maybe Prelude.Text)
s3CsvSource_groupSize = Lens.lens (\S3CsvSource' {groupSize} -> groupSize) (\s@S3CsvSource' {} a -> s {groupSize = a} :: S3CsvSource)

-- | Specifies a character to use for escaping. This option is used only when
-- reading CSV files. The default value is @none@. If enabled, the
-- character which immediately follows is used as-is, except for a small
-- set of well-known escapes (@\\n@, @\\r@, @\\t@, and @\\0@).
s3CsvSource_escaper :: Lens.Lens' S3CsvSource (Prelude.Maybe Prelude.Text)
s3CsvSource_escaper = Lens.lens (\S3CsvSource' {escaper} -> escaper) (\s@S3CsvSource' {} a -> s {escaper = a} :: S3CsvSource)

-- | The name of the data store.
s3CsvSource_name :: Lens.Lens' S3CsvSource Prelude.Text
s3CsvSource_name = Lens.lens (\S3CsvSource' {name} -> name) (\s@S3CsvSource' {} a -> s {name = a} :: S3CsvSource)

-- | A list of the Amazon S3 paths to read from.
s3CsvSource_paths :: Lens.Lens' S3CsvSource [Prelude.Text]
s3CsvSource_paths = Lens.lens (\S3CsvSource' {paths} -> paths) (\s@S3CsvSource' {} a -> s {paths = a} :: S3CsvSource) Prelude.. Lens.coerced

-- | Specifies the delimiter character. The default is a comma: \",\", but
-- any other character can be specified.
s3CsvSource_separator :: Lens.Lens' S3CsvSource Separator
s3CsvSource_separator = Lens.lens (\S3CsvSource' {separator} -> separator) (\s@S3CsvSource' {} a -> s {separator = a} :: S3CsvSource)

-- | Specifies the character to use for quoting. The default is a double
-- quote: @\'\"\'@. Set this to @-1@ to turn off quoting entirely.
s3CsvSource_quoteChar :: Lens.Lens' S3CsvSource QuoteChar
s3CsvSource_quoteChar = Lens.lens (\S3CsvSource' {quoteChar} -> quoteChar) (\s@S3CsvSource' {} a -> s {quoteChar = a} :: S3CsvSource)

instance Core.FromJSON S3CsvSource where
  parseJSON =
    Core.withObject
      "S3CsvSource"
      ( \x ->
          S3CsvSource'
            Prelude.<$> (x Core..:? "OutputSchemas" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "GroupFiles")
            Prelude.<*> (x Core..:? "MaxBand")
            Prelude.<*> (x Core..:? "WriteHeader")
            Prelude.<*> (x Core..:? "MaxFilesInBand")
            Prelude.<*> (x Core..:? "OptimizePerformance")
            Prelude.<*> (x Core..:? "Recurse")
            Prelude.<*> (x Core..:? "AdditionalOptions")
            Prelude.<*> (x Core..:? "CompressionType")
            Prelude.<*> (x Core..:? "SkipFirst")
            Prelude.<*> (x Core..:? "WithHeader")
            Prelude.<*> (x Core..:? "Multiline")
            Prelude.<*> (x Core..:? "Exclusions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "GroupSize")
            Prelude.<*> (x Core..:? "Escaper")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..:? "Paths" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "Separator")
            Prelude.<*> (x Core..: "QuoteChar")
      )

instance Prelude.Hashable S3CsvSource where
  hashWithSalt _salt S3CsvSource' {..} =
    _salt `Prelude.hashWithSalt` outputSchemas
      `Prelude.hashWithSalt` groupFiles
      `Prelude.hashWithSalt` maxBand
      `Prelude.hashWithSalt` writeHeader
      `Prelude.hashWithSalt` maxFilesInBand
      `Prelude.hashWithSalt` optimizePerformance
      `Prelude.hashWithSalt` recurse
      `Prelude.hashWithSalt` additionalOptions
      `Prelude.hashWithSalt` compressionType
      `Prelude.hashWithSalt` skipFirst
      `Prelude.hashWithSalt` withHeader
      `Prelude.hashWithSalt` multiline
      `Prelude.hashWithSalt` exclusions
      `Prelude.hashWithSalt` groupSize
      `Prelude.hashWithSalt` escaper
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` paths
      `Prelude.hashWithSalt` separator
      `Prelude.hashWithSalt` quoteChar

instance Prelude.NFData S3CsvSource where
  rnf S3CsvSource' {..} =
    Prelude.rnf outputSchemas
      `Prelude.seq` Prelude.rnf groupFiles
      `Prelude.seq` Prelude.rnf maxBand
      `Prelude.seq` Prelude.rnf writeHeader
      `Prelude.seq` Prelude.rnf maxFilesInBand
      `Prelude.seq` Prelude.rnf optimizePerformance
      `Prelude.seq` Prelude.rnf recurse
      `Prelude.seq` Prelude.rnf additionalOptions
      `Prelude.seq` Prelude.rnf compressionType
      `Prelude.seq` Prelude.rnf skipFirst
      `Prelude.seq` Prelude.rnf withHeader
      `Prelude.seq` Prelude.rnf multiline
      `Prelude.seq` Prelude.rnf exclusions
      `Prelude.seq` Prelude.rnf groupSize
      `Prelude.seq` Prelude.rnf escaper
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf paths
      `Prelude.seq` Prelude.rnf separator
      `Prelude.seq` Prelude.rnf quoteChar

instance Core.ToJSON S3CsvSource where
  toJSON S3CsvSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OutputSchemas" Core..=) Prelude.<$> outputSchemas,
            ("GroupFiles" Core..=) Prelude.<$> groupFiles,
            ("MaxBand" Core..=) Prelude.<$> maxBand,
            ("WriteHeader" Core..=) Prelude.<$> writeHeader,
            ("MaxFilesInBand" Core..=)
              Prelude.<$> maxFilesInBand,
            ("OptimizePerformance" Core..=)
              Prelude.<$> optimizePerformance,
            ("Recurse" Core..=) Prelude.<$> recurse,
            ("AdditionalOptions" Core..=)
              Prelude.<$> additionalOptions,
            ("CompressionType" Core..=)
              Prelude.<$> compressionType,
            ("SkipFirst" Core..=) Prelude.<$> skipFirst,
            ("WithHeader" Core..=) Prelude.<$> withHeader,
            ("Multiline" Core..=) Prelude.<$> multiline,
            ("Exclusions" Core..=) Prelude.<$> exclusions,
            ("GroupSize" Core..=) Prelude.<$> groupSize,
            ("Escaper" Core..=) Prelude.<$> escaper,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Paths" Core..= paths),
            Prelude.Just ("Separator" Core..= separator),
            Prelude.Just ("QuoteChar" Core..= quoteChar)
          ]
      )
