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
-- Module      : Amazonka.FraudDetector.Types.EvaluatedModelVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.EvaluatedModelVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FraudDetector.Types.ModelVersionEvaluation
import qualified Amazonka.Prelude as Prelude

-- | The model version evaluated for generating prediction.
--
-- /See:/ 'newEvaluatedModelVersion' smart constructor.
data EvaluatedModelVersion = EvaluatedModelVersion'
  { -- | Evaluations generated for the model version.
    evaluations :: Prelude.Maybe [ModelVersionEvaluation],
    -- | The model version.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | The model type.
    --
    -- Valid values: @ONLINE_FRAUD_INSIGHTS@ | @TRANSACTION_FRAUD_INSIGHTS@
    modelType :: Prelude.Maybe Prelude.Text,
    -- | The model ID.
    modelId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluatedModelVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluations', 'evaluatedModelVersion_evaluations' - Evaluations generated for the model version.
--
-- 'modelVersion', 'evaluatedModelVersion_modelVersion' - The model version.
--
-- 'modelType', 'evaluatedModelVersion_modelType' - The model type.
--
-- Valid values: @ONLINE_FRAUD_INSIGHTS@ | @TRANSACTION_FRAUD_INSIGHTS@
--
-- 'modelId', 'evaluatedModelVersion_modelId' - The model ID.
newEvaluatedModelVersion ::
  EvaluatedModelVersion
newEvaluatedModelVersion =
  EvaluatedModelVersion'
    { evaluations =
        Prelude.Nothing,
      modelVersion = Prelude.Nothing,
      modelType = Prelude.Nothing,
      modelId = Prelude.Nothing
    }

-- | Evaluations generated for the model version.
evaluatedModelVersion_evaluations :: Lens.Lens' EvaluatedModelVersion (Prelude.Maybe [ModelVersionEvaluation])
evaluatedModelVersion_evaluations = Lens.lens (\EvaluatedModelVersion' {evaluations} -> evaluations) (\s@EvaluatedModelVersion' {} a -> s {evaluations = a} :: EvaluatedModelVersion) Prelude.. Lens.mapping Lens.coerced

-- | The model version.
evaluatedModelVersion_modelVersion :: Lens.Lens' EvaluatedModelVersion (Prelude.Maybe Prelude.Text)
evaluatedModelVersion_modelVersion = Lens.lens (\EvaluatedModelVersion' {modelVersion} -> modelVersion) (\s@EvaluatedModelVersion' {} a -> s {modelVersion = a} :: EvaluatedModelVersion)

-- | The model type.
--
-- Valid values: @ONLINE_FRAUD_INSIGHTS@ | @TRANSACTION_FRAUD_INSIGHTS@
evaluatedModelVersion_modelType :: Lens.Lens' EvaluatedModelVersion (Prelude.Maybe Prelude.Text)
evaluatedModelVersion_modelType = Lens.lens (\EvaluatedModelVersion' {modelType} -> modelType) (\s@EvaluatedModelVersion' {} a -> s {modelType = a} :: EvaluatedModelVersion)

-- | The model ID.
evaluatedModelVersion_modelId :: Lens.Lens' EvaluatedModelVersion (Prelude.Maybe Prelude.Text)
evaluatedModelVersion_modelId = Lens.lens (\EvaluatedModelVersion' {modelId} -> modelId) (\s@EvaluatedModelVersion' {} a -> s {modelId = a} :: EvaluatedModelVersion)

instance Core.FromJSON EvaluatedModelVersion where
  parseJSON =
    Core.withObject
      "EvaluatedModelVersion"
      ( \x ->
          EvaluatedModelVersion'
            Prelude.<$> (x Core..:? "evaluations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "modelVersion")
            Prelude.<*> (x Core..:? "modelType")
            Prelude.<*> (x Core..:? "modelId")
      )

instance Prelude.Hashable EvaluatedModelVersion where
  hashWithSalt _salt EvaluatedModelVersion' {..} =
    _salt `Prelude.hashWithSalt` evaluations
      `Prelude.hashWithSalt` modelVersion
      `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` modelId

instance Prelude.NFData EvaluatedModelVersion where
  rnf EvaluatedModelVersion' {..} =
    Prelude.rnf evaluations
      `Prelude.seq` Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf modelId
