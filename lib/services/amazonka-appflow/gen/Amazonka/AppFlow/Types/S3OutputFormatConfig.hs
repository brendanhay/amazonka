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
-- Module      : Amazonka.AppFlow.Types.S3OutputFormatConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.S3OutputFormatConfig where

import Amazonka.AppFlow.Types.AggregationConfig
import Amazonka.AppFlow.Types.FileType
import Amazonka.AppFlow.Types.PrefixConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration that determines how Amazon AppFlow should format the
-- flow output data when Amazon S3 is used as the destination.
--
-- /See:/ 'newS3OutputFormatConfig' smart constructor.
data S3OutputFormatConfig = S3OutputFormatConfig'
  { aggregationConfig :: Prelude.Maybe AggregationConfig,
    -- | Indicates the file type that Amazon AppFlow places in the Amazon S3
    -- bucket.
    fileType :: Prelude.Maybe FileType,
    -- | Determines the prefix that Amazon AppFlow applies to the folder name in
    -- the Amazon S3 bucket. You can name folders according to the flow
    -- frequency and date.
    prefixConfig :: Prelude.Maybe PrefixConfig,
    -- | If your file output format is Parquet, use this parameter to set whether
    -- Amazon AppFlow preserves the data types in your source data when it
    -- writes the output to Amazon S3.
    --
    -- -   @true@: Amazon AppFlow preserves the data types when it writes to
    --     Amazon S3. For example, an integer or @1@ in your source data is
    --     still an integer in your output.
    --
    -- -   @false@: Amazon AppFlow converts all of the source data into strings
    --     when it writes to Amazon S3. For example, an integer of @1@ in your
    --     source data becomes the string @\"1\"@ in the output.
    preserveSourceDataTyping :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3OutputFormatConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationConfig', 's3OutputFormatConfig_aggregationConfig' - Undocumented member.
--
-- 'fileType', 's3OutputFormatConfig_fileType' - Indicates the file type that Amazon AppFlow places in the Amazon S3
-- bucket.
--
-- 'prefixConfig', 's3OutputFormatConfig_prefixConfig' - Determines the prefix that Amazon AppFlow applies to the folder name in
-- the Amazon S3 bucket. You can name folders according to the flow
-- frequency and date.
--
-- 'preserveSourceDataTyping', 's3OutputFormatConfig_preserveSourceDataTyping' - If your file output format is Parquet, use this parameter to set whether
-- Amazon AppFlow preserves the data types in your source data when it
-- writes the output to Amazon S3.
--
-- -   @true@: Amazon AppFlow preserves the data types when it writes to
--     Amazon S3. For example, an integer or @1@ in your source data is
--     still an integer in your output.
--
-- -   @false@: Amazon AppFlow converts all of the source data into strings
--     when it writes to Amazon S3. For example, an integer of @1@ in your
--     source data becomes the string @\"1\"@ in the output.
newS3OutputFormatConfig ::
  S3OutputFormatConfig
newS3OutputFormatConfig =
  S3OutputFormatConfig'
    { aggregationConfig =
        Prelude.Nothing,
      fileType = Prelude.Nothing,
      prefixConfig = Prelude.Nothing,
      preserveSourceDataTyping = Prelude.Nothing
    }

-- | Undocumented member.
s3OutputFormatConfig_aggregationConfig :: Lens.Lens' S3OutputFormatConfig (Prelude.Maybe AggregationConfig)
s3OutputFormatConfig_aggregationConfig = Lens.lens (\S3OutputFormatConfig' {aggregationConfig} -> aggregationConfig) (\s@S3OutputFormatConfig' {} a -> s {aggregationConfig = a} :: S3OutputFormatConfig)

-- | Indicates the file type that Amazon AppFlow places in the Amazon S3
-- bucket.
s3OutputFormatConfig_fileType :: Lens.Lens' S3OutputFormatConfig (Prelude.Maybe FileType)
s3OutputFormatConfig_fileType = Lens.lens (\S3OutputFormatConfig' {fileType} -> fileType) (\s@S3OutputFormatConfig' {} a -> s {fileType = a} :: S3OutputFormatConfig)

-- | Determines the prefix that Amazon AppFlow applies to the folder name in
-- the Amazon S3 bucket. You can name folders according to the flow
-- frequency and date.
s3OutputFormatConfig_prefixConfig :: Lens.Lens' S3OutputFormatConfig (Prelude.Maybe PrefixConfig)
s3OutputFormatConfig_prefixConfig = Lens.lens (\S3OutputFormatConfig' {prefixConfig} -> prefixConfig) (\s@S3OutputFormatConfig' {} a -> s {prefixConfig = a} :: S3OutputFormatConfig)

-- | If your file output format is Parquet, use this parameter to set whether
-- Amazon AppFlow preserves the data types in your source data when it
-- writes the output to Amazon S3.
--
-- -   @true@: Amazon AppFlow preserves the data types when it writes to
--     Amazon S3. For example, an integer or @1@ in your source data is
--     still an integer in your output.
--
-- -   @false@: Amazon AppFlow converts all of the source data into strings
--     when it writes to Amazon S3. For example, an integer of @1@ in your
--     source data becomes the string @\"1\"@ in the output.
s3OutputFormatConfig_preserveSourceDataTyping :: Lens.Lens' S3OutputFormatConfig (Prelude.Maybe Prelude.Bool)
s3OutputFormatConfig_preserveSourceDataTyping = Lens.lens (\S3OutputFormatConfig' {preserveSourceDataTyping} -> preserveSourceDataTyping) (\s@S3OutputFormatConfig' {} a -> s {preserveSourceDataTyping = a} :: S3OutputFormatConfig)

instance Data.FromJSON S3OutputFormatConfig where
  parseJSON =
    Data.withObject
      "S3OutputFormatConfig"
      ( \x ->
          S3OutputFormatConfig'
            Prelude.<$> (x Data..:? "aggregationConfig")
            Prelude.<*> (x Data..:? "fileType")
            Prelude.<*> (x Data..:? "prefixConfig")
            Prelude.<*> (x Data..:? "preserveSourceDataTyping")
      )

instance Prelude.Hashable S3OutputFormatConfig where
  hashWithSalt _salt S3OutputFormatConfig' {..} =
    _salt
      `Prelude.hashWithSalt` aggregationConfig
      `Prelude.hashWithSalt` fileType
      `Prelude.hashWithSalt` prefixConfig
      `Prelude.hashWithSalt` preserveSourceDataTyping

instance Prelude.NFData S3OutputFormatConfig where
  rnf S3OutputFormatConfig' {..} =
    Prelude.rnf aggregationConfig `Prelude.seq`
      Prelude.rnf fileType `Prelude.seq`
        Prelude.rnf prefixConfig `Prelude.seq`
          Prelude.rnf preserveSourceDataTyping

instance Data.ToJSON S3OutputFormatConfig where
  toJSON S3OutputFormatConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("aggregationConfig" Data..=)
              Prelude.<$> aggregationConfig,
            ("fileType" Data..=) Prelude.<$> fileType,
            ("prefixConfig" Data..=) Prelude.<$> prefixConfig,
            ("preserveSourceDataTyping" Data..=)
              Prelude.<$> preserveSourceDataTyping
          ]
      )
