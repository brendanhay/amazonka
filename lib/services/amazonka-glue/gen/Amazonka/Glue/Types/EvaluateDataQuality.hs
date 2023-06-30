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
-- Module      : Amazonka.Glue.Types.EvaluateDataQuality
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.EvaluateDataQuality where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DQResultsPublishingOptions
import Amazonka.Glue.Types.DQStopJobOnFailureOptions
import Amazonka.Glue.Types.DQTransformOutput
import qualified Amazonka.Prelude as Prelude

-- | Specifies your data quality evaluation criteria.
--
-- /See:/ 'newEvaluateDataQuality' smart constructor.
data EvaluateDataQuality = EvaluateDataQuality'
  { -- | The output of your data quality evaluation.
    output :: Prelude.Maybe DQTransformOutput,
    -- | Options to configure how your results are published.
    publishingOptions :: Prelude.Maybe DQResultsPublishingOptions,
    -- | Options to configure how your job will stop if your data quality
    -- evaluation fails.
    stopJobOnFailureOptions :: Prelude.Maybe DQStopJobOnFailureOptions,
    -- | The name of the data quality evaluation.
    name :: Prelude.Text,
    -- | The inputs of your data quality evaluation.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | The ruleset for your data quality evaluation.
    ruleset :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluateDataQuality' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'output', 'evaluateDataQuality_output' - The output of your data quality evaluation.
--
-- 'publishingOptions', 'evaluateDataQuality_publishingOptions' - Options to configure how your results are published.
--
-- 'stopJobOnFailureOptions', 'evaluateDataQuality_stopJobOnFailureOptions' - Options to configure how your job will stop if your data quality
-- evaluation fails.
--
-- 'name', 'evaluateDataQuality_name' - The name of the data quality evaluation.
--
-- 'inputs', 'evaluateDataQuality_inputs' - The inputs of your data quality evaluation.
--
-- 'ruleset', 'evaluateDataQuality_ruleset' - The ruleset for your data quality evaluation.
newEvaluateDataQuality ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'ruleset'
  Prelude.Text ->
  EvaluateDataQuality
newEvaluateDataQuality pName_ pInputs_ pRuleset_ =
  EvaluateDataQuality'
    { output = Prelude.Nothing,
      publishingOptions = Prelude.Nothing,
      stopJobOnFailureOptions = Prelude.Nothing,
      name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      ruleset = pRuleset_
    }

-- | The output of your data quality evaluation.
evaluateDataQuality_output :: Lens.Lens' EvaluateDataQuality (Prelude.Maybe DQTransformOutput)
evaluateDataQuality_output = Lens.lens (\EvaluateDataQuality' {output} -> output) (\s@EvaluateDataQuality' {} a -> s {output = a} :: EvaluateDataQuality)

-- | Options to configure how your results are published.
evaluateDataQuality_publishingOptions :: Lens.Lens' EvaluateDataQuality (Prelude.Maybe DQResultsPublishingOptions)
evaluateDataQuality_publishingOptions = Lens.lens (\EvaluateDataQuality' {publishingOptions} -> publishingOptions) (\s@EvaluateDataQuality' {} a -> s {publishingOptions = a} :: EvaluateDataQuality)

-- | Options to configure how your job will stop if your data quality
-- evaluation fails.
evaluateDataQuality_stopJobOnFailureOptions :: Lens.Lens' EvaluateDataQuality (Prelude.Maybe DQStopJobOnFailureOptions)
evaluateDataQuality_stopJobOnFailureOptions = Lens.lens (\EvaluateDataQuality' {stopJobOnFailureOptions} -> stopJobOnFailureOptions) (\s@EvaluateDataQuality' {} a -> s {stopJobOnFailureOptions = a} :: EvaluateDataQuality)

-- | The name of the data quality evaluation.
evaluateDataQuality_name :: Lens.Lens' EvaluateDataQuality Prelude.Text
evaluateDataQuality_name = Lens.lens (\EvaluateDataQuality' {name} -> name) (\s@EvaluateDataQuality' {} a -> s {name = a} :: EvaluateDataQuality)

-- | The inputs of your data quality evaluation.
evaluateDataQuality_inputs :: Lens.Lens' EvaluateDataQuality (Prelude.NonEmpty Prelude.Text)
evaluateDataQuality_inputs = Lens.lens (\EvaluateDataQuality' {inputs} -> inputs) (\s@EvaluateDataQuality' {} a -> s {inputs = a} :: EvaluateDataQuality) Prelude.. Lens.coerced

-- | The ruleset for your data quality evaluation.
evaluateDataQuality_ruleset :: Lens.Lens' EvaluateDataQuality Prelude.Text
evaluateDataQuality_ruleset = Lens.lens (\EvaluateDataQuality' {ruleset} -> ruleset) (\s@EvaluateDataQuality' {} a -> s {ruleset = a} :: EvaluateDataQuality)

instance Data.FromJSON EvaluateDataQuality where
  parseJSON =
    Data.withObject
      "EvaluateDataQuality"
      ( \x ->
          EvaluateDataQuality'
            Prelude.<$> (x Data..:? "Output")
            Prelude.<*> (x Data..:? "PublishingOptions")
            Prelude.<*> (x Data..:? "StopJobOnFailureOptions")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Ruleset")
      )

instance Prelude.Hashable EvaluateDataQuality where
  hashWithSalt _salt EvaluateDataQuality' {..} =
    _salt
      `Prelude.hashWithSalt` output
      `Prelude.hashWithSalt` publishingOptions
      `Prelude.hashWithSalt` stopJobOnFailureOptions
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` ruleset

instance Prelude.NFData EvaluateDataQuality where
  rnf EvaluateDataQuality' {..} =
    Prelude.rnf output
      `Prelude.seq` Prelude.rnf publishingOptions
      `Prelude.seq` Prelude.rnf stopJobOnFailureOptions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf ruleset

instance Data.ToJSON EvaluateDataQuality where
  toJSON EvaluateDataQuality' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Output" Data..=) Prelude.<$> output,
            ("PublishingOptions" Data..=)
              Prelude.<$> publishingOptions,
            ("StopJobOnFailureOptions" Data..=)
              Prelude.<$> stopJobOnFailureOptions,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Ruleset" Data..= ruleset)
          ]
      )
