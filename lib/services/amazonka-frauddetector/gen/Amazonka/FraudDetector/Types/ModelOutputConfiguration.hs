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
-- Module      : Amazonka.FraudDetector.Types.ModelOutputConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ModelOutputConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.ModelOutputDataFormat
import qualified Amazonka.Prelude as Prelude

-- | Provides the Amazon Sagemaker model output configuration.
--
-- /See:/ 'newModelOutputConfiguration' smart constructor.
data ModelOutputConfiguration = ModelOutputConfiguration'
  { -- | A map of JSON keys in response from SageMaker to the Amazon Fraud
    -- Detector variables.
    jsonKeyToVariableMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A map of CSV index values in the SageMaker response to the Amazon Fraud
    -- Detector variables.
    csvIndexToVariableMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The format of the model output configuration.
    format :: ModelOutputDataFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelOutputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jsonKeyToVariableMap', 'modelOutputConfiguration_jsonKeyToVariableMap' - A map of JSON keys in response from SageMaker to the Amazon Fraud
-- Detector variables.
--
-- 'csvIndexToVariableMap', 'modelOutputConfiguration_csvIndexToVariableMap' - A map of CSV index values in the SageMaker response to the Amazon Fraud
-- Detector variables.
--
-- 'format', 'modelOutputConfiguration_format' - The format of the model output configuration.
newModelOutputConfiguration ::
  -- | 'format'
  ModelOutputDataFormat ->
  ModelOutputConfiguration
newModelOutputConfiguration pFormat_ =
  ModelOutputConfiguration'
    { jsonKeyToVariableMap =
        Prelude.Nothing,
      csvIndexToVariableMap = Prelude.Nothing,
      format = pFormat_
    }

-- | A map of JSON keys in response from SageMaker to the Amazon Fraud
-- Detector variables.
modelOutputConfiguration_jsonKeyToVariableMap :: Lens.Lens' ModelOutputConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
modelOutputConfiguration_jsonKeyToVariableMap = Lens.lens (\ModelOutputConfiguration' {jsonKeyToVariableMap} -> jsonKeyToVariableMap) (\s@ModelOutputConfiguration' {} a -> s {jsonKeyToVariableMap = a} :: ModelOutputConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A map of CSV index values in the SageMaker response to the Amazon Fraud
-- Detector variables.
modelOutputConfiguration_csvIndexToVariableMap :: Lens.Lens' ModelOutputConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
modelOutputConfiguration_csvIndexToVariableMap = Lens.lens (\ModelOutputConfiguration' {csvIndexToVariableMap} -> csvIndexToVariableMap) (\s@ModelOutputConfiguration' {} a -> s {csvIndexToVariableMap = a} :: ModelOutputConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The format of the model output configuration.
modelOutputConfiguration_format :: Lens.Lens' ModelOutputConfiguration ModelOutputDataFormat
modelOutputConfiguration_format = Lens.lens (\ModelOutputConfiguration' {format} -> format) (\s@ModelOutputConfiguration' {} a -> s {format = a} :: ModelOutputConfiguration)

instance Data.FromJSON ModelOutputConfiguration where
  parseJSON =
    Data.withObject
      "ModelOutputConfiguration"
      ( \x ->
          ModelOutputConfiguration'
            Prelude.<$> ( x Data..:? "jsonKeyToVariableMap"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "csvIndexToVariableMap"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "format")
      )

instance Prelude.Hashable ModelOutputConfiguration where
  hashWithSalt _salt ModelOutputConfiguration' {..} =
    _salt `Prelude.hashWithSalt` jsonKeyToVariableMap
      `Prelude.hashWithSalt` csvIndexToVariableMap
      `Prelude.hashWithSalt` format

instance Prelude.NFData ModelOutputConfiguration where
  rnf ModelOutputConfiguration' {..} =
    Prelude.rnf jsonKeyToVariableMap
      `Prelude.seq` Prelude.rnf csvIndexToVariableMap
      `Prelude.seq` Prelude.rnf format

instance Data.ToJSON ModelOutputConfiguration where
  toJSON ModelOutputConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("jsonKeyToVariableMap" Data..=)
              Prelude.<$> jsonKeyToVariableMap,
            ("csvIndexToVariableMap" Data..=)
              Prelude.<$> csvIndexToVariableMap,
            Prelude.Just ("format" Data..= format)
          ]
      )
