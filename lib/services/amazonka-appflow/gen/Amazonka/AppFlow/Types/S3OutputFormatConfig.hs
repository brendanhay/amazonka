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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.S3OutputFormatConfig where

import Amazonka.AppFlow.Types.AggregationConfig
import Amazonka.AppFlow.Types.FileType
import Amazonka.AppFlow.Types.PrefixConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration that determines how Amazon AppFlow should format the
-- flow output data when Amazon S3 is used as the destination.
--
-- /See:/ 'newS3OutputFormatConfig' smart constructor.
data S3OutputFormatConfig = S3OutputFormatConfig'
  { -- | Determines the prefix that Amazon AppFlow applies to the folder name in
    -- the Amazon S3 bucket. You can name folders according to the flow
    -- frequency and date.
    prefixConfig :: Prelude.Maybe PrefixConfig,
    -- | Indicates the file type that Amazon AppFlow places in the Amazon S3
    -- bucket.
    fileType :: Prelude.Maybe FileType,
    aggregationConfig :: Prelude.Maybe AggregationConfig
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
-- 'prefixConfig', 's3OutputFormatConfig_prefixConfig' - Determines the prefix that Amazon AppFlow applies to the folder name in
-- the Amazon S3 bucket. You can name folders according to the flow
-- frequency and date.
--
-- 'fileType', 's3OutputFormatConfig_fileType' - Indicates the file type that Amazon AppFlow places in the Amazon S3
-- bucket.
--
-- 'aggregationConfig', 's3OutputFormatConfig_aggregationConfig' - Undocumented member.
newS3OutputFormatConfig ::
  S3OutputFormatConfig
newS3OutputFormatConfig =
  S3OutputFormatConfig'
    { prefixConfig =
        Prelude.Nothing,
      fileType = Prelude.Nothing,
      aggregationConfig = Prelude.Nothing
    }

-- | Determines the prefix that Amazon AppFlow applies to the folder name in
-- the Amazon S3 bucket. You can name folders according to the flow
-- frequency and date.
s3OutputFormatConfig_prefixConfig :: Lens.Lens' S3OutputFormatConfig (Prelude.Maybe PrefixConfig)
s3OutputFormatConfig_prefixConfig = Lens.lens (\S3OutputFormatConfig' {prefixConfig} -> prefixConfig) (\s@S3OutputFormatConfig' {} a -> s {prefixConfig = a} :: S3OutputFormatConfig)

-- | Indicates the file type that Amazon AppFlow places in the Amazon S3
-- bucket.
s3OutputFormatConfig_fileType :: Lens.Lens' S3OutputFormatConfig (Prelude.Maybe FileType)
s3OutputFormatConfig_fileType = Lens.lens (\S3OutputFormatConfig' {fileType} -> fileType) (\s@S3OutputFormatConfig' {} a -> s {fileType = a} :: S3OutputFormatConfig)

-- | Undocumented member.
s3OutputFormatConfig_aggregationConfig :: Lens.Lens' S3OutputFormatConfig (Prelude.Maybe AggregationConfig)
s3OutputFormatConfig_aggregationConfig = Lens.lens (\S3OutputFormatConfig' {aggregationConfig} -> aggregationConfig) (\s@S3OutputFormatConfig' {} a -> s {aggregationConfig = a} :: S3OutputFormatConfig)

instance Core.FromJSON S3OutputFormatConfig where
  parseJSON =
    Core.withObject
      "S3OutputFormatConfig"
      ( \x ->
          S3OutputFormatConfig'
            Prelude.<$> (x Core..:? "prefixConfig")
            Prelude.<*> (x Core..:? "fileType")
            Prelude.<*> (x Core..:? "aggregationConfig")
      )

instance Prelude.Hashable S3OutputFormatConfig

instance Prelude.NFData S3OutputFormatConfig

instance Core.ToJSON S3OutputFormatConfig where
  toJSON S3OutputFormatConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("prefixConfig" Core..=) Prelude.<$> prefixConfig,
            ("fileType" Core..=) Prelude.<$> fileType,
            ("aggregationConfig" Core..=)
              Prelude.<$> aggregationConfig
          ]
      )
