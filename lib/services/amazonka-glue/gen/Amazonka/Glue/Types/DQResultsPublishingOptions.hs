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
-- Module      : Amazonka.Glue.Types.DQResultsPublishingOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DQResultsPublishingOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Options to configure how your data quality evaluation results are
-- published.
--
-- /See:/ 'newDQResultsPublishingOptions' smart constructor.
data DQResultsPublishingOptions = DQResultsPublishingOptions'
  { -- | Enable metrics for your data quality results.
    cloudWatchMetricsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The context of the evaluation.
    evaluationContext :: Prelude.Maybe Prelude.Text,
    -- | Enable publishing for your data quality results.
    resultsPublishingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon S3 prefix prepended to the results.
    resultsS3Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DQResultsPublishingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchMetricsEnabled', 'dQResultsPublishingOptions_cloudWatchMetricsEnabled' - Enable metrics for your data quality results.
--
-- 'evaluationContext', 'dQResultsPublishingOptions_evaluationContext' - The context of the evaluation.
--
-- 'resultsPublishingEnabled', 'dQResultsPublishingOptions_resultsPublishingEnabled' - Enable publishing for your data quality results.
--
-- 'resultsS3Prefix', 'dQResultsPublishingOptions_resultsS3Prefix' - The Amazon S3 prefix prepended to the results.
newDQResultsPublishingOptions ::
  DQResultsPublishingOptions
newDQResultsPublishingOptions =
  DQResultsPublishingOptions'
    { cloudWatchMetricsEnabled =
        Prelude.Nothing,
      evaluationContext = Prelude.Nothing,
      resultsPublishingEnabled = Prelude.Nothing,
      resultsS3Prefix = Prelude.Nothing
    }

-- | Enable metrics for your data quality results.
dQResultsPublishingOptions_cloudWatchMetricsEnabled :: Lens.Lens' DQResultsPublishingOptions (Prelude.Maybe Prelude.Bool)
dQResultsPublishingOptions_cloudWatchMetricsEnabled = Lens.lens (\DQResultsPublishingOptions' {cloudWatchMetricsEnabled} -> cloudWatchMetricsEnabled) (\s@DQResultsPublishingOptions' {} a -> s {cloudWatchMetricsEnabled = a} :: DQResultsPublishingOptions)

-- | The context of the evaluation.
dQResultsPublishingOptions_evaluationContext :: Lens.Lens' DQResultsPublishingOptions (Prelude.Maybe Prelude.Text)
dQResultsPublishingOptions_evaluationContext = Lens.lens (\DQResultsPublishingOptions' {evaluationContext} -> evaluationContext) (\s@DQResultsPublishingOptions' {} a -> s {evaluationContext = a} :: DQResultsPublishingOptions)

-- | Enable publishing for your data quality results.
dQResultsPublishingOptions_resultsPublishingEnabled :: Lens.Lens' DQResultsPublishingOptions (Prelude.Maybe Prelude.Bool)
dQResultsPublishingOptions_resultsPublishingEnabled = Lens.lens (\DQResultsPublishingOptions' {resultsPublishingEnabled} -> resultsPublishingEnabled) (\s@DQResultsPublishingOptions' {} a -> s {resultsPublishingEnabled = a} :: DQResultsPublishingOptions)

-- | The Amazon S3 prefix prepended to the results.
dQResultsPublishingOptions_resultsS3Prefix :: Lens.Lens' DQResultsPublishingOptions (Prelude.Maybe Prelude.Text)
dQResultsPublishingOptions_resultsS3Prefix = Lens.lens (\DQResultsPublishingOptions' {resultsS3Prefix} -> resultsS3Prefix) (\s@DQResultsPublishingOptions' {} a -> s {resultsS3Prefix = a} :: DQResultsPublishingOptions)

instance Data.FromJSON DQResultsPublishingOptions where
  parseJSON =
    Data.withObject
      "DQResultsPublishingOptions"
      ( \x ->
          DQResultsPublishingOptions'
            Prelude.<$> (x Data..:? "CloudWatchMetricsEnabled")
            Prelude.<*> (x Data..:? "EvaluationContext")
            Prelude.<*> (x Data..:? "ResultsPublishingEnabled")
            Prelude.<*> (x Data..:? "ResultsS3Prefix")
      )

instance Prelude.Hashable DQResultsPublishingOptions where
  hashWithSalt _salt DQResultsPublishingOptions' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchMetricsEnabled
      `Prelude.hashWithSalt` evaluationContext
      `Prelude.hashWithSalt` resultsPublishingEnabled
      `Prelude.hashWithSalt` resultsS3Prefix

instance Prelude.NFData DQResultsPublishingOptions where
  rnf DQResultsPublishingOptions' {..} =
    Prelude.rnf cloudWatchMetricsEnabled
      `Prelude.seq` Prelude.rnf evaluationContext
      `Prelude.seq` Prelude.rnf resultsPublishingEnabled
      `Prelude.seq` Prelude.rnf resultsS3Prefix

instance Data.ToJSON DQResultsPublishingOptions where
  toJSON DQResultsPublishingOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchMetricsEnabled" Data..=)
              Prelude.<$> cloudWatchMetricsEnabled,
            ("EvaluationContext" Data..=)
              Prelude.<$> evaluationContext,
            ("ResultsPublishingEnabled" Data..=)
              Prelude.<$> resultsPublishingEnabled,
            ("ResultsS3Prefix" Data..=)
              Prelude.<$> resultsS3Prefix
          ]
      )
