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
-- Module      : Amazonka.Glue.Types.S3ParquetSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3ParquetSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.GlueSchema
import Amazonka.Glue.Types.ParquetCompressionType
import Amazonka.Glue.Types.S3DirectSourceAdditionalOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies an Apache Parquet data store stored in Amazon S3.
--
-- /See:/ 'newS3ParquetSource' smart constructor.
data S3ParquetSource = S3ParquetSource'
  { -- | Specifies the data schema for the S3 Parquet source.
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
    -- | This option specifies the maximum number of files to save from the last
    -- maxBand seconds. If this number is exceeded, extra files are skipped and
    -- only processed in the next job run.
    maxFilesInBand :: Prelude.Maybe Prelude.Natural,
    -- | If set to true, recursively reads files in all subdirectories under the
    -- specified paths.
    recurse :: Prelude.Maybe Prelude.Bool,
    -- | Specifies additional connection options.
    additionalOptions :: Prelude.Maybe S3DirectSourceAdditionalOptions,
    -- | Specifies how the data is compressed. This is generally not necessary if
    -- the data has a standard file extension. Possible values are @\"gzip\"@
    -- and @\"bzip\"@).
    compressionType :: Prelude.Maybe ParquetCompressionType,
    -- | A string containing a JSON list of Unix-style glob patterns to exclude.
    -- For example, \"[\\\"**.pdf\\\"]\" excludes all PDF files.
    exclusions :: Prelude.Maybe [Prelude.Text],
    -- | The target group size in bytes. The default is computed based on the
    -- input data size and the size of your cluster. When there are fewer than
    -- 50,000 input files, @\"groupFiles\"@ must be set to @\"inPartition\"@
    -- for this to take effect.
    groupSize :: Prelude.Maybe Prelude.Text,
    -- | The name of the data store.
    name :: Prelude.Text,
    -- | A list of the Amazon S3 paths to read from.
    paths :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ParquetSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputSchemas', 's3ParquetSource_outputSchemas' - Specifies the data schema for the S3 Parquet source.
--
-- 'groupFiles', 's3ParquetSource_groupFiles' - Grouping files is turned on by default when the input contains more than
-- 50,000 files. To turn on grouping with fewer than 50,000 files, set this
-- parameter to \"inPartition\". To disable grouping when there are more
-- than 50,000 files, set this parameter to @\"none\"@.
--
-- 'maxBand', 's3ParquetSource_maxBand' - This option controls the duration in milliseconds after which the s3
-- listing is likely to be consistent. Files with modification timestamps
-- falling within the last maxBand milliseconds are tracked specially when
-- using JobBookmarks to account for Amazon S3 eventual consistency. Most
-- users don\'t need to set this option. The default is 900000
-- milliseconds, or 15 minutes.
--
-- 'maxFilesInBand', 's3ParquetSource_maxFilesInBand' - This option specifies the maximum number of files to save from the last
-- maxBand seconds. If this number is exceeded, extra files are skipped and
-- only processed in the next job run.
--
-- 'recurse', 's3ParquetSource_recurse' - If set to true, recursively reads files in all subdirectories under the
-- specified paths.
--
-- 'additionalOptions', 's3ParquetSource_additionalOptions' - Specifies additional connection options.
--
-- 'compressionType', 's3ParquetSource_compressionType' - Specifies how the data is compressed. This is generally not necessary if
-- the data has a standard file extension. Possible values are @\"gzip\"@
-- and @\"bzip\"@).
--
-- 'exclusions', 's3ParquetSource_exclusions' - A string containing a JSON list of Unix-style glob patterns to exclude.
-- For example, \"[\\\"**.pdf\\\"]\" excludes all PDF files.
--
-- 'groupSize', 's3ParquetSource_groupSize' - The target group size in bytes. The default is computed based on the
-- input data size and the size of your cluster. When there are fewer than
-- 50,000 input files, @\"groupFiles\"@ must be set to @\"inPartition\"@
-- for this to take effect.
--
-- 'name', 's3ParquetSource_name' - The name of the data store.
--
-- 'paths', 's3ParquetSource_paths' - A list of the Amazon S3 paths to read from.
newS3ParquetSource ::
  -- | 'name'
  Prelude.Text ->
  S3ParquetSource
newS3ParquetSource pName_ =
  S3ParquetSource'
    { outputSchemas = Prelude.Nothing,
      groupFiles = Prelude.Nothing,
      maxBand = Prelude.Nothing,
      maxFilesInBand = Prelude.Nothing,
      recurse = Prelude.Nothing,
      additionalOptions = Prelude.Nothing,
      compressionType = Prelude.Nothing,
      exclusions = Prelude.Nothing,
      groupSize = Prelude.Nothing,
      name = pName_,
      paths = Prelude.mempty
    }

-- | Specifies the data schema for the S3 Parquet source.
s3ParquetSource_outputSchemas :: Lens.Lens' S3ParquetSource (Prelude.Maybe [GlueSchema])
s3ParquetSource_outputSchemas = Lens.lens (\S3ParquetSource' {outputSchemas} -> outputSchemas) (\s@S3ParquetSource' {} a -> s {outputSchemas = a} :: S3ParquetSource) Prelude.. Lens.mapping Lens.coerced

-- | Grouping files is turned on by default when the input contains more than
-- 50,000 files. To turn on grouping with fewer than 50,000 files, set this
-- parameter to \"inPartition\". To disable grouping when there are more
-- than 50,000 files, set this parameter to @\"none\"@.
s3ParquetSource_groupFiles :: Lens.Lens' S3ParquetSource (Prelude.Maybe Prelude.Text)
s3ParquetSource_groupFiles = Lens.lens (\S3ParquetSource' {groupFiles} -> groupFiles) (\s@S3ParquetSource' {} a -> s {groupFiles = a} :: S3ParquetSource)

-- | This option controls the duration in milliseconds after which the s3
-- listing is likely to be consistent. Files with modification timestamps
-- falling within the last maxBand milliseconds are tracked specially when
-- using JobBookmarks to account for Amazon S3 eventual consistency. Most
-- users don\'t need to set this option. The default is 900000
-- milliseconds, or 15 minutes.
s3ParquetSource_maxBand :: Lens.Lens' S3ParquetSource (Prelude.Maybe Prelude.Natural)
s3ParquetSource_maxBand = Lens.lens (\S3ParquetSource' {maxBand} -> maxBand) (\s@S3ParquetSource' {} a -> s {maxBand = a} :: S3ParquetSource)

-- | This option specifies the maximum number of files to save from the last
-- maxBand seconds. If this number is exceeded, extra files are skipped and
-- only processed in the next job run.
s3ParquetSource_maxFilesInBand :: Lens.Lens' S3ParquetSource (Prelude.Maybe Prelude.Natural)
s3ParquetSource_maxFilesInBand = Lens.lens (\S3ParquetSource' {maxFilesInBand} -> maxFilesInBand) (\s@S3ParquetSource' {} a -> s {maxFilesInBand = a} :: S3ParquetSource)

-- | If set to true, recursively reads files in all subdirectories under the
-- specified paths.
s3ParquetSource_recurse :: Lens.Lens' S3ParquetSource (Prelude.Maybe Prelude.Bool)
s3ParquetSource_recurse = Lens.lens (\S3ParquetSource' {recurse} -> recurse) (\s@S3ParquetSource' {} a -> s {recurse = a} :: S3ParquetSource)

-- | Specifies additional connection options.
s3ParquetSource_additionalOptions :: Lens.Lens' S3ParquetSource (Prelude.Maybe S3DirectSourceAdditionalOptions)
s3ParquetSource_additionalOptions = Lens.lens (\S3ParquetSource' {additionalOptions} -> additionalOptions) (\s@S3ParquetSource' {} a -> s {additionalOptions = a} :: S3ParquetSource)

-- | Specifies how the data is compressed. This is generally not necessary if
-- the data has a standard file extension. Possible values are @\"gzip\"@
-- and @\"bzip\"@).
s3ParquetSource_compressionType :: Lens.Lens' S3ParquetSource (Prelude.Maybe ParquetCompressionType)
s3ParquetSource_compressionType = Lens.lens (\S3ParquetSource' {compressionType} -> compressionType) (\s@S3ParquetSource' {} a -> s {compressionType = a} :: S3ParquetSource)

-- | A string containing a JSON list of Unix-style glob patterns to exclude.
-- For example, \"[\\\"**.pdf\\\"]\" excludes all PDF files.
s3ParquetSource_exclusions :: Lens.Lens' S3ParquetSource (Prelude.Maybe [Prelude.Text])
s3ParquetSource_exclusions = Lens.lens (\S3ParquetSource' {exclusions} -> exclusions) (\s@S3ParquetSource' {} a -> s {exclusions = a} :: S3ParquetSource) Prelude.. Lens.mapping Lens.coerced

-- | The target group size in bytes. The default is computed based on the
-- input data size and the size of your cluster. When there are fewer than
-- 50,000 input files, @\"groupFiles\"@ must be set to @\"inPartition\"@
-- for this to take effect.
s3ParquetSource_groupSize :: Lens.Lens' S3ParquetSource (Prelude.Maybe Prelude.Text)
s3ParquetSource_groupSize = Lens.lens (\S3ParquetSource' {groupSize} -> groupSize) (\s@S3ParquetSource' {} a -> s {groupSize = a} :: S3ParquetSource)

-- | The name of the data store.
s3ParquetSource_name :: Lens.Lens' S3ParquetSource Prelude.Text
s3ParquetSource_name = Lens.lens (\S3ParquetSource' {name} -> name) (\s@S3ParquetSource' {} a -> s {name = a} :: S3ParquetSource)

-- | A list of the Amazon S3 paths to read from.
s3ParquetSource_paths :: Lens.Lens' S3ParquetSource [Prelude.Text]
s3ParquetSource_paths = Lens.lens (\S3ParquetSource' {paths} -> paths) (\s@S3ParquetSource' {} a -> s {paths = a} :: S3ParquetSource) Prelude.. Lens.coerced

instance Data.FromJSON S3ParquetSource where
  parseJSON =
    Data.withObject
      "S3ParquetSource"
      ( \x ->
          S3ParquetSource'
            Prelude.<$> (x Data..:? "OutputSchemas" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "GroupFiles")
            Prelude.<*> (x Data..:? "MaxBand")
            Prelude.<*> (x Data..:? "MaxFilesInBand")
            Prelude.<*> (x Data..:? "Recurse")
            Prelude.<*> (x Data..:? "AdditionalOptions")
            Prelude.<*> (x Data..:? "CompressionType")
            Prelude.<*> (x Data..:? "Exclusions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "GroupSize")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..:? "Paths" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable S3ParquetSource where
  hashWithSalt _salt S3ParquetSource' {..} =
    _salt `Prelude.hashWithSalt` outputSchemas
      `Prelude.hashWithSalt` groupFiles
      `Prelude.hashWithSalt` maxBand
      `Prelude.hashWithSalt` maxFilesInBand
      `Prelude.hashWithSalt` recurse
      `Prelude.hashWithSalt` additionalOptions
      `Prelude.hashWithSalt` compressionType
      `Prelude.hashWithSalt` exclusions
      `Prelude.hashWithSalt` groupSize
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` paths

instance Prelude.NFData S3ParquetSource where
  rnf S3ParquetSource' {..} =
    Prelude.rnf outputSchemas
      `Prelude.seq` Prelude.rnf groupFiles
      `Prelude.seq` Prelude.rnf maxBand
      `Prelude.seq` Prelude.rnf maxFilesInBand
      `Prelude.seq` Prelude.rnf recurse
      `Prelude.seq` Prelude.rnf additionalOptions
      `Prelude.seq` Prelude.rnf compressionType
      `Prelude.seq` Prelude.rnf exclusions
      `Prelude.seq` Prelude.rnf groupSize
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf paths

instance Data.ToJSON S3ParquetSource where
  toJSON S3ParquetSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OutputSchemas" Data..=) Prelude.<$> outputSchemas,
            ("GroupFiles" Data..=) Prelude.<$> groupFiles,
            ("MaxBand" Data..=) Prelude.<$> maxBand,
            ("MaxFilesInBand" Data..=)
              Prelude.<$> maxFilesInBand,
            ("Recurse" Data..=) Prelude.<$> recurse,
            ("AdditionalOptions" Data..=)
              Prelude.<$> additionalOptions,
            ("CompressionType" Data..=)
              Prelude.<$> compressionType,
            ("Exclusions" Data..=) Prelude.<$> exclusions,
            ("GroupSize" Data..=) Prelude.<$> groupSize,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Paths" Data..= paths)
          ]
      )
