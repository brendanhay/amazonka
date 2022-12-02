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
-- Module      : Amazonka.AppFlow.Types.UpsolverS3OutputFormatConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.UpsolverS3OutputFormatConfig where

import Amazonka.AppFlow.Types.AggregationConfig
import Amazonka.AppFlow.Types.FileType
import Amazonka.AppFlow.Types.PrefixConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration that determines how Amazon AppFlow formats the flow
-- output data when Upsolver is used as the destination.
--
-- /See:/ 'newUpsolverS3OutputFormatConfig' smart constructor.
data UpsolverS3OutputFormatConfig = UpsolverS3OutputFormatConfig'
  { aggregationConfig :: Prelude.Maybe AggregationConfig,
    -- | Indicates the file type that Amazon AppFlow places in the Upsolver
    -- Amazon S3 bucket.
    fileType :: Prelude.Maybe FileType,
    prefixConfig :: PrefixConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpsolverS3OutputFormatConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationConfig', 'upsolverS3OutputFormatConfig_aggregationConfig' - Undocumented member.
--
-- 'fileType', 'upsolverS3OutputFormatConfig_fileType' - Indicates the file type that Amazon AppFlow places in the Upsolver
-- Amazon S3 bucket.
--
-- 'prefixConfig', 'upsolverS3OutputFormatConfig_prefixConfig' - Undocumented member.
newUpsolverS3OutputFormatConfig ::
  -- | 'prefixConfig'
  PrefixConfig ->
  UpsolverS3OutputFormatConfig
newUpsolverS3OutputFormatConfig pPrefixConfig_ =
  UpsolverS3OutputFormatConfig'
    { aggregationConfig =
        Prelude.Nothing,
      fileType = Prelude.Nothing,
      prefixConfig = pPrefixConfig_
    }

-- | Undocumented member.
upsolverS3OutputFormatConfig_aggregationConfig :: Lens.Lens' UpsolverS3OutputFormatConfig (Prelude.Maybe AggregationConfig)
upsolverS3OutputFormatConfig_aggregationConfig = Lens.lens (\UpsolverS3OutputFormatConfig' {aggregationConfig} -> aggregationConfig) (\s@UpsolverS3OutputFormatConfig' {} a -> s {aggregationConfig = a} :: UpsolverS3OutputFormatConfig)

-- | Indicates the file type that Amazon AppFlow places in the Upsolver
-- Amazon S3 bucket.
upsolverS3OutputFormatConfig_fileType :: Lens.Lens' UpsolverS3OutputFormatConfig (Prelude.Maybe FileType)
upsolverS3OutputFormatConfig_fileType = Lens.lens (\UpsolverS3OutputFormatConfig' {fileType} -> fileType) (\s@UpsolverS3OutputFormatConfig' {} a -> s {fileType = a} :: UpsolverS3OutputFormatConfig)

-- | Undocumented member.
upsolverS3OutputFormatConfig_prefixConfig :: Lens.Lens' UpsolverS3OutputFormatConfig PrefixConfig
upsolverS3OutputFormatConfig_prefixConfig = Lens.lens (\UpsolverS3OutputFormatConfig' {prefixConfig} -> prefixConfig) (\s@UpsolverS3OutputFormatConfig' {} a -> s {prefixConfig = a} :: UpsolverS3OutputFormatConfig)

instance Data.FromJSON UpsolverS3OutputFormatConfig where
  parseJSON =
    Data.withObject
      "UpsolverS3OutputFormatConfig"
      ( \x ->
          UpsolverS3OutputFormatConfig'
            Prelude.<$> (x Data..:? "aggregationConfig")
            Prelude.<*> (x Data..:? "fileType")
            Prelude.<*> (x Data..: "prefixConfig")
      )

instance
  Prelude.Hashable
    UpsolverS3OutputFormatConfig
  where
  hashWithSalt _salt UpsolverS3OutputFormatConfig' {..} =
    _salt `Prelude.hashWithSalt` aggregationConfig
      `Prelude.hashWithSalt` fileType
      `Prelude.hashWithSalt` prefixConfig

instance Prelude.NFData UpsolverS3OutputFormatConfig where
  rnf UpsolverS3OutputFormatConfig' {..} =
    Prelude.rnf aggregationConfig
      `Prelude.seq` Prelude.rnf fileType
      `Prelude.seq` Prelude.rnf prefixConfig

instance Data.ToJSON UpsolverS3OutputFormatConfig where
  toJSON UpsolverS3OutputFormatConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("aggregationConfig" Data..=)
              Prelude.<$> aggregationConfig,
            ("fileType" Data..=) Prelude.<$> fileType,
            Prelude.Just ("prefixConfig" Data..= prefixConfig)
          ]
      )
