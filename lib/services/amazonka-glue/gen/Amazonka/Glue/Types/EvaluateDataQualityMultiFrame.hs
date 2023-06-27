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
-- Module      : Amazonka.Glue.Types.EvaluateDataQualityMultiFrame
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.EvaluateDataQualityMultiFrame where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.AdditionalOptionKeys
import Amazonka.Glue.Types.DQResultsPublishingOptions
import Amazonka.Glue.Types.DQStopJobOnFailureOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies your data quality evaluation criteria.
--
-- /See:/ 'newEvaluateDataQualityMultiFrame' smart constructor.
data EvaluateDataQualityMultiFrame = EvaluateDataQualityMultiFrame'
  { -- | The aliases of all data sources except primary.
    additionalDataSources :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Options to configure runtime behavior of the transform.
    additionalOptions :: Prelude.Maybe (Prelude.HashMap AdditionalOptionKeys Prelude.Text),
    -- | Options to configure how your results are published.
    publishingOptions :: Prelude.Maybe DQResultsPublishingOptions,
    -- | Options to configure how your job will stop if your data quality
    -- evaluation fails.
    stopJobOnFailureOptions :: Prelude.Maybe DQStopJobOnFailureOptions,
    -- | The name of the data quality evaluation.
    name :: Prelude.Text,
    -- | The inputs of your data quality evaluation. The first input in this list
    -- is the primary data source.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | The ruleset for your data quality evaluation.
    ruleset :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluateDataQualityMultiFrame' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalDataSources', 'evaluateDataQualityMultiFrame_additionalDataSources' - The aliases of all data sources except primary.
--
-- 'additionalOptions', 'evaluateDataQualityMultiFrame_additionalOptions' - Options to configure runtime behavior of the transform.
--
-- 'publishingOptions', 'evaluateDataQualityMultiFrame_publishingOptions' - Options to configure how your results are published.
--
-- 'stopJobOnFailureOptions', 'evaluateDataQualityMultiFrame_stopJobOnFailureOptions' - Options to configure how your job will stop if your data quality
-- evaluation fails.
--
-- 'name', 'evaluateDataQualityMultiFrame_name' - The name of the data quality evaluation.
--
-- 'inputs', 'evaluateDataQualityMultiFrame_inputs' - The inputs of your data quality evaluation. The first input in this list
-- is the primary data source.
--
-- 'ruleset', 'evaluateDataQualityMultiFrame_ruleset' - The ruleset for your data quality evaluation.
newEvaluateDataQualityMultiFrame ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'ruleset'
  Prelude.Text ->
  EvaluateDataQualityMultiFrame
newEvaluateDataQualityMultiFrame
  pName_
  pInputs_
  pRuleset_ =
    EvaluateDataQualityMultiFrame'
      { additionalDataSources =
          Prelude.Nothing,
        additionalOptions = Prelude.Nothing,
        publishingOptions = Prelude.Nothing,
        stopJobOnFailureOptions = Prelude.Nothing,
        name = pName_,
        inputs = Lens.coerced Lens.# pInputs_,
        ruleset = pRuleset_
      }

-- | The aliases of all data sources except primary.
evaluateDataQualityMultiFrame_additionalDataSources :: Lens.Lens' EvaluateDataQualityMultiFrame (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
evaluateDataQualityMultiFrame_additionalDataSources = Lens.lens (\EvaluateDataQualityMultiFrame' {additionalDataSources} -> additionalDataSources) (\s@EvaluateDataQualityMultiFrame' {} a -> s {additionalDataSources = a} :: EvaluateDataQualityMultiFrame) Prelude.. Lens.mapping Lens.coerced

-- | Options to configure runtime behavior of the transform.
evaluateDataQualityMultiFrame_additionalOptions :: Lens.Lens' EvaluateDataQualityMultiFrame (Prelude.Maybe (Prelude.HashMap AdditionalOptionKeys Prelude.Text))
evaluateDataQualityMultiFrame_additionalOptions = Lens.lens (\EvaluateDataQualityMultiFrame' {additionalOptions} -> additionalOptions) (\s@EvaluateDataQualityMultiFrame' {} a -> s {additionalOptions = a} :: EvaluateDataQualityMultiFrame) Prelude.. Lens.mapping Lens.coerced

-- | Options to configure how your results are published.
evaluateDataQualityMultiFrame_publishingOptions :: Lens.Lens' EvaluateDataQualityMultiFrame (Prelude.Maybe DQResultsPublishingOptions)
evaluateDataQualityMultiFrame_publishingOptions = Lens.lens (\EvaluateDataQualityMultiFrame' {publishingOptions} -> publishingOptions) (\s@EvaluateDataQualityMultiFrame' {} a -> s {publishingOptions = a} :: EvaluateDataQualityMultiFrame)

-- | Options to configure how your job will stop if your data quality
-- evaluation fails.
evaluateDataQualityMultiFrame_stopJobOnFailureOptions :: Lens.Lens' EvaluateDataQualityMultiFrame (Prelude.Maybe DQStopJobOnFailureOptions)
evaluateDataQualityMultiFrame_stopJobOnFailureOptions = Lens.lens (\EvaluateDataQualityMultiFrame' {stopJobOnFailureOptions} -> stopJobOnFailureOptions) (\s@EvaluateDataQualityMultiFrame' {} a -> s {stopJobOnFailureOptions = a} :: EvaluateDataQualityMultiFrame)

-- | The name of the data quality evaluation.
evaluateDataQualityMultiFrame_name :: Lens.Lens' EvaluateDataQualityMultiFrame Prelude.Text
evaluateDataQualityMultiFrame_name = Lens.lens (\EvaluateDataQualityMultiFrame' {name} -> name) (\s@EvaluateDataQualityMultiFrame' {} a -> s {name = a} :: EvaluateDataQualityMultiFrame)

-- | The inputs of your data quality evaluation. The first input in this list
-- is the primary data source.
evaluateDataQualityMultiFrame_inputs :: Lens.Lens' EvaluateDataQualityMultiFrame (Prelude.NonEmpty Prelude.Text)
evaluateDataQualityMultiFrame_inputs = Lens.lens (\EvaluateDataQualityMultiFrame' {inputs} -> inputs) (\s@EvaluateDataQualityMultiFrame' {} a -> s {inputs = a} :: EvaluateDataQualityMultiFrame) Prelude.. Lens.coerced

-- | The ruleset for your data quality evaluation.
evaluateDataQualityMultiFrame_ruleset :: Lens.Lens' EvaluateDataQualityMultiFrame Prelude.Text
evaluateDataQualityMultiFrame_ruleset = Lens.lens (\EvaluateDataQualityMultiFrame' {ruleset} -> ruleset) (\s@EvaluateDataQualityMultiFrame' {} a -> s {ruleset = a} :: EvaluateDataQualityMultiFrame)

instance Data.FromJSON EvaluateDataQualityMultiFrame where
  parseJSON =
    Data.withObject
      "EvaluateDataQualityMultiFrame"
      ( \x ->
          EvaluateDataQualityMultiFrame'
            Prelude.<$> ( x
                            Data..:? "AdditionalDataSources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "AdditionalOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PublishingOptions")
            Prelude.<*> (x Data..:? "StopJobOnFailureOptions")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Ruleset")
      )

instance
  Prelude.Hashable
    EvaluateDataQualityMultiFrame
  where
  hashWithSalt _salt EvaluateDataQualityMultiFrame' {..} =
    _salt
      `Prelude.hashWithSalt` additionalDataSources
      `Prelude.hashWithSalt` additionalOptions
      `Prelude.hashWithSalt` publishingOptions
      `Prelude.hashWithSalt` stopJobOnFailureOptions
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` ruleset

instance Prelude.NFData EvaluateDataQualityMultiFrame where
  rnf EvaluateDataQualityMultiFrame' {..} =
    Prelude.rnf additionalDataSources
      `Prelude.seq` Prelude.rnf additionalOptions
      `Prelude.seq` Prelude.rnf publishingOptions
      `Prelude.seq` Prelude.rnf stopJobOnFailureOptions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf ruleset

instance Data.ToJSON EvaluateDataQualityMultiFrame where
  toJSON EvaluateDataQualityMultiFrame' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalDataSources" Data..=)
              Prelude.<$> additionalDataSources,
            ("AdditionalOptions" Data..=)
              Prelude.<$> additionalOptions,
            ("PublishingOptions" Data..=)
              Prelude.<$> publishingOptions,
            ("StopJobOnFailureOptions" Data..=)
              Prelude.<$> stopJobOnFailureOptions,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Ruleset" Data..= ruleset)
          ]
      )
