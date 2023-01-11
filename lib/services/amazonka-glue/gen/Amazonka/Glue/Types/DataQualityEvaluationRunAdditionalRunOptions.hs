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
-- Module      : Amazonka.Glue.Types.DataQualityEvaluationRunAdditionalRunOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DataQualityEvaluationRunAdditionalRunOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Additional run options you can specify for an evaluation run.
--
-- /See:/ 'newDataQualityEvaluationRunAdditionalRunOptions' smart constructor.
data DataQualityEvaluationRunAdditionalRunOptions = DataQualityEvaluationRunAdditionalRunOptions'
  { -- | Whether or not to enable CloudWatch metrics.
    cloudWatchMetricsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Prefix for Amazon S3 to store results.
    resultsS3Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityEvaluationRunAdditionalRunOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchMetricsEnabled', 'dataQualityEvaluationRunAdditionalRunOptions_cloudWatchMetricsEnabled' - Whether or not to enable CloudWatch metrics.
--
-- 'resultsS3Prefix', 'dataQualityEvaluationRunAdditionalRunOptions_resultsS3Prefix' - Prefix for Amazon S3 to store results.
newDataQualityEvaluationRunAdditionalRunOptions ::
  DataQualityEvaluationRunAdditionalRunOptions
newDataQualityEvaluationRunAdditionalRunOptions =
  DataQualityEvaluationRunAdditionalRunOptions'
    { cloudWatchMetricsEnabled =
        Prelude.Nothing,
      resultsS3Prefix =
        Prelude.Nothing
    }

-- | Whether or not to enable CloudWatch metrics.
dataQualityEvaluationRunAdditionalRunOptions_cloudWatchMetricsEnabled :: Lens.Lens' DataQualityEvaluationRunAdditionalRunOptions (Prelude.Maybe Prelude.Bool)
dataQualityEvaluationRunAdditionalRunOptions_cloudWatchMetricsEnabled = Lens.lens (\DataQualityEvaluationRunAdditionalRunOptions' {cloudWatchMetricsEnabled} -> cloudWatchMetricsEnabled) (\s@DataQualityEvaluationRunAdditionalRunOptions' {} a -> s {cloudWatchMetricsEnabled = a} :: DataQualityEvaluationRunAdditionalRunOptions)

-- | Prefix for Amazon S3 to store results.
dataQualityEvaluationRunAdditionalRunOptions_resultsS3Prefix :: Lens.Lens' DataQualityEvaluationRunAdditionalRunOptions (Prelude.Maybe Prelude.Text)
dataQualityEvaluationRunAdditionalRunOptions_resultsS3Prefix = Lens.lens (\DataQualityEvaluationRunAdditionalRunOptions' {resultsS3Prefix} -> resultsS3Prefix) (\s@DataQualityEvaluationRunAdditionalRunOptions' {} a -> s {resultsS3Prefix = a} :: DataQualityEvaluationRunAdditionalRunOptions)

instance
  Data.FromJSON
    DataQualityEvaluationRunAdditionalRunOptions
  where
  parseJSON =
    Data.withObject
      "DataQualityEvaluationRunAdditionalRunOptions"
      ( \x ->
          DataQualityEvaluationRunAdditionalRunOptions'
            Prelude.<$> (x Data..:? "CloudWatchMetricsEnabled")
              Prelude.<*> (x Data..:? "ResultsS3Prefix")
      )

instance
  Prelude.Hashable
    DataQualityEvaluationRunAdditionalRunOptions
  where
  hashWithSalt
    _salt
    DataQualityEvaluationRunAdditionalRunOptions' {..} =
      _salt
        `Prelude.hashWithSalt` cloudWatchMetricsEnabled
        `Prelude.hashWithSalt` resultsS3Prefix

instance
  Prelude.NFData
    DataQualityEvaluationRunAdditionalRunOptions
  where
  rnf DataQualityEvaluationRunAdditionalRunOptions' {..} =
    Prelude.rnf cloudWatchMetricsEnabled
      `Prelude.seq` Prelude.rnf resultsS3Prefix

instance
  Data.ToJSON
    DataQualityEvaluationRunAdditionalRunOptions
  where
  toJSON
    DataQualityEvaluationRunAdditionalRunOptions' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("CloudWatchMetricsEnabled" Data..=)
                Prelude.<$> cloudWatchMetricsEnabled,
              ("ResultsS3Prefix" Data..=)
                Prelude.<$> resultsS3Prefix
            ]
        )
