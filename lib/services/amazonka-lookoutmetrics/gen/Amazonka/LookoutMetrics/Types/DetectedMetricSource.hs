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
-- Module      : Amazonka.LookoutMetrics.Types.DetectedMetricSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.DetectedMetricSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.DetectedS3SourceConfig
import qualified Amazonka.Prelude as Prelude

-- | An inferred data source.
--
-- /See:/ 'newDetectedMetricSource' smart constructor.
data DetectedMetricSource = DetectedMetricSource'
  { -- | The data source\'s source configuration.
    s3SourceConfig :: Prelude.Maybe DetectedS3SourceConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectedMetricSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3SourceConfig', 'detectedMetricSource_s3SourceConfig' - The data source\'s source configuration.
newDetectedMetricSource ::
  DetectedMetricSource
newDetectedMetricSource =
  DetectedMetricSource'
    { s3SourceConfig =
        Prelude.Nothing
    }

-- | The data source\'s source configuration.
detectedMetricSource_s3SourceConfig :: Lens.Lens' DetectedMetricSource (Prelude.Maybe DetectedS3SourceConfig)
detectedMetricSource_s3SourceConfig = Lens.lens (\DetectedMetricSource' {s3SourceConfig} -> s3SourceConfig) (\s@DetectedMetricSource' {} a -> s {s3SourceConfig = a} :: DetectedMetricSource)

instance Data.FromJSON DetectedMetricSource where
  parseJSON =
    Data.withObject
      "DetectedMetricSource"
      ( \x ->
          DetectedMetricSource'
            Prelude.<$> (x Data..:? "S3SourceConfig")
      )

instance Prelude.Hashable DetectedMetricSource where
  hashWithSalt _salt DetectedMetricSource' {..} =
    _salt `Prelude.hashWithSalt` s3SourceConfig

instance Prelude.NFData DetectedMetricSource where
  rnf DetectedMetricSource' {..} =
    Prelude.rnf s3SourceConfig
