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
-- Module      : Amazonka.SageMaker.Types.ClarifyExplainerConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ClarifyExplainerConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ClarifyInferenceConfig
import Amazonka.SageMaker.Types.ClarifyShapConfig

-- | The configuration parameters for the SageMaker Clarify explainer.
--
-- /See:/ 'newClarifyExplainerConfig' smart constructor.
data ClarifyExplainerConfig = ClarifyExplainerConfig'
  { -- | A JMESPath boolean expression used to filter which records to explain.
    -- Explanations are activated by default. See
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-create-endpoint.html#clarify-online-explainability-create-endpoint-enable EnableExplanations>
    -- for additional information.
    enableExplanations :: Prelude.Maybe Prelude.Text,
    -- | The inference configuration parameter for the model container.
    inferenceConfig :: Prelude.Maybe ClarifyInferenceConfig,
    -- | The configuration for SHAP analysis.
    shapConfig :: ClarifyShapConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClarifyExplainerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableExplanations', 'clarifyExplainerConfig_enableExplanations' - A JMESPath boolean expression used to filter which records to explain.
-- Explanations are activated by default. See
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-create-endpoint.html#clarify-online-explainability-create-endpoint-enable EnableExplanations>
-- for additional information.
--
-- 'inferenceConfig', 'clarifyExplainerConfig_inferenceConfig' - The inference configuration parameter for the model container.
--
-- 'shapConfig', 'clarifyExplainerConfig_shapConfig' - The configuration for SHAP analysis.
newClarifyExplainerConfig ::
  -- | 'shapConfig'
  ClarifyShapConfig ->
  ClarifyExplainerConfig
newClarifyExplainerConfig pShapConfig_ =
  ClarifyExplainerConfig'
    { enableExplanations =
        Prelude.Nothing,
      inferenceConfig = Prelude.Nothing,
      shapConfig = pShapConfig_
    }

-- | A JMESPath boolean expression used to filter which records to explain.
-- Explanations are activated by default. See
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-create-endpoint.html#clarify-online-explainability-create-endpoint-enable EnableExplanations>
-- for additional information.
clarifyExplainerConfig_enableExplanations :: Lens.Lens' ClarifyExplainerConfig (Prelude.Maybe Prelude.Text)
clarifyExplainerConfig_enableExplanations = Lens.lens (\ClarifyExplainerConfig' {enableExplanations} -> enableExplanations) (\s@ClarifyExplainerConfig' {} a -> s {enableExplanations = a} :: ClarifyExplainerConfig)

-- | The inference configuration parameter for the model container.
clarifyExplainerConfig_inferenceConfig :: Lens.Lens' ClarifyExplainerConfig (Prelude.Maybe ClarifyInferenceConfig)
clarifyExplainerConfig_inferenceConfig = Lens.lens (\ClarifyExplainerConfig' {inferenceConfig} -> inferenceConfig) (\s@ClarifyExplainerConfig' {} a -> s {inferenceConfig = a} :: ClarifyExplainerConfig)

-- | The configuration for SHAP analysis.
clarifyExplainerConfig_shapConfig :: Lens.Lens' ClarifyExplainerConfig ClarifyShapConfig
clarifyExplainerConfig_shapConfig = Lens.lens (\ClarifyExplainerConfig' {shapConfig} -> shapConfig) (\s@ClarifyExplainerConfig' {} a -> s {shapConfig = a} :: ClarifyExplainerConfig)

instance Data.FromJSON ClarifyExplainerConfig where
  parseJSON =
    Data.withObject
      "ClarifyExplainerConfig"
      ( \x ->
          ClarifyExplainerConfig'
            Prelude.<$> (x Data..:? "EnableExplanations")
            Prelude.<*> (x Data..:? "InferenceConfig")
            Prelude.<*> (x Data..: "ShapConfig")
      )

instance Prelude.Hashable ClarifyExplainerConfig where
  hashWithSalt _salt ClarifyExplainerConfig' {..} =
    _salt `Prelude.hashWithSalt` enableExplanations
      `Prelude.hashWithSalt` inferenceConfig
      `Prelude.hashWithSalt` shapConfig

instance Prelude.NFData ClarifyExplainerConfig where
  rnf ClarifyExplainerConfig' {..} =
    Prelude.rnf enableExplanations
      `Prelude.seq` Prelude.rnf inferenceConfig
      `Prelude.seq` Prelude.rnf shapConfig

instance Data.ToJSON ClarifyExplainerConfig where
  toJSON ClarifyExplainerConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EnableExplanations" Data..=)
              Prelude.<$> enableExplanations,
            ("InferenceConfig" Data..=)
              Prelude.<$> inferenceConfig,
            Prelude.Just ("ShapConfig" Data..= shapConfig)
          ]
      )
