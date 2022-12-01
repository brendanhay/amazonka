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
-- Module      : Amazonka.LookoutMetrics.Types.AutoDetectionMetricSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AutoDetectionMetricSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types.AutoDetectionS3SourceConfig
import qualified Amazonka.Prelude as Prelude

-- | An auto detection metric source.
--
-- /See:/ 'newAutoDetectionMetricSource' smart constructor.
data AutoDetectionMetricSource = AutoDetectionMetricSource'
  { -- | The source\'s source config.
    s3SourceConfig :: Prelude.Maybe AutoDetectionS3SourceConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoDetectionMetricSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3SourceConfig', 'autoDetectionMetricSource_s3SourceConfig' - The source\'s source config.
newAutoDetectionMetricSource ::
  AutoDetectionMetricSource
newAutoDetectionMetricSource =
  AutoDetectionMetricSource'
    { s3SourceConfig =
        Prelude.Nothing
    }

-- | The source\'s source config.
autoDetectionMetricSource_s3SourceConfig :: Lens.Lens' AutoDetectionMetricSource (Prelude.Maybe AutoDetectionS3SourceConfig)
autoDetectionMetricSource_s3SourceConfig = Lens.lens (\AutoDetectionMetricSource' {s3SourceConfig} -> s3SourceConfig) (\s@AutoDetectionMetricSource' {} a -> s {s3SourceConfig = a} :: AutoDetectionMetricSource)

instance Prelude.Hashable AutoDetectionMetricSource where
  hashWithSalt _salt AutoDetectionMetricSource' {..} =
    _salt `Prelude.hashWithSalt` s3SourceConfig

instance Prelude.NFData AutoDetectionMetricSource where
  rnf AutoDetectionMetricSource' {..} =
    Prelude.rnf s3SourceConfig

instance Core.ToJSON AutoDetectionMetricSource where
  toJSON AutoDetectionMetricSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("S3SourceConfig" Core..=)
              Prelude.<$> s3SourceConfig
          ]
      )
