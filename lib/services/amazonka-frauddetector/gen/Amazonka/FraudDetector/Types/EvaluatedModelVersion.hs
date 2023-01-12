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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.EvaluatedModelVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.ModelVersionEvaluation
import qualified Amazonka.Prelude as Prelude

-- | The model version evaluated for generating prediction.
--
-- /See:/ 'newEvaluatedModelVersion' smart constructor.
data EvaluatedModelVersion = EvaluatedModelVersion'
  { -- | Evaluations generated for the model version.
    evaluations :: Prelude.Maybe [ModelVersionEvaluation],
    -- | The model ID.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | The model type.
    --
    -- Valid values: @ONLINE_FRAUD_INSIGHTS@ | @TRANSACTION_FRAUD_INSIGHTS@
    modelType :: Prelude.Maybe Prelude.Text,
    -- | The model version.
    modelVersion :: Prelude.Maybe Prelude.Text
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
-- 'modelId', 'evaluatedModelVersion_modelId' - The model ID.
--
-- 'modelType', 'evaluatedModelVersion_modelType' - The model type.
--
-- Valid values: @ONLINE_FRAUD_INSIGHTS@ | @TRANSACTION_FRAUD_INSIGHTS@
--
-- 'modelVersion', 'evaluatedModelVersion_modelVersion' - The model version.
newEvaluatedModelVersion ::
  EvaluatedModelVersion
newEvaluatedModelVersion =
  EvaluatedModelVersion'
    { evaluations =
        Prelude.Nothing,
      modelId = Prelude.Nothing,
      modelType = Prelude.Nothing,
      modelVersion = Prelude.Nothing
    }

-- | Evaluations generated for the model version.
evaluatedModelVersion_evaluations :: Lens.Lens' EvaluatedModelVersion (Prelude.Maybe [ModelVersionEvaluation])
evaluatedModelVersion_evaluations = Lens.lens (\EvaluatedModelVersion' {evaluations} -> evaluations) (\s@EvaluatedModelVersion' {} a -> s {evaluations = a} :: EvaluatedModelVersion) Prelude.. Lens.mapping Lens.coerced

-- | The model ID.
evaluatedModelVersion_modelId :: Lens.Lens' EvaluatedModelVersion (Prelude.Maybe Prelude.Text)
evaluatedModelVersion_modelId = Lens.lens (\EvaluatedModelVersion' {modelId} -> modelId) (\s@EvaluatedModelVersion' {} a -> s {modelId = a} :: EvaluatedModelVersion)

-- | The model type.
--
-- Valid values: @ONLINE_FRAUD_INSIGHTS@ | @TRANSACTION_FRAUD_INSIGHTS@
evaluatedModelVersion_modelType :: Lens.Lens' EvaluatedModelVersion (Prelude.Maybe Prelude.Text)
evaluatedModelVersion_modelType = Lens.lens (\EvaluatedModelVersion' {modelType} -> modelType) (\s@EvaluatedModelVersion' {} a -> s {modelType = a} :: EvaluatedModelVersion)

-- | The model version.
evaluatedModelVersion_modelVersion :: Lens.Lens' EvaluatedModelVersion (Prelude.Maybe Prelude.Text)
evaluatedModelVersion_modelVersion = Lens.lens (\EvaluatedModelVersion' {modelVersion} -> modelVersion) (\s@EvaluatedModelVersion' {} a -> s {modelVersion = a} :: EvaluatedModelVersion)

instance Data.FromJSON EvaluatedModelVersion where
  parseJSON =
    Data.withObject
      "EvaluatedModelVersion"
      ( \x ->
          EvaluatedModelVersion'
            Prelude.<$> (x Data..:? "evaluations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "modelId")
            Prelude.<*> (x Data..:? "modelType")
            Prelude.<*> (x Data..:? "modelVersion")
      )

instance Prelude.Hashable EvaluatedModelVersion where
  hashWithSalt _salt EvaluatedModelVersion' {..} =
    _salt `Prelude.hashWithSalt` evaluations
      `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` modelVersion

instance Prelude.NFData EvaluatedModelVersion where
  rnf EvaluatedModelVersion' {..} =
    Prelude.rnf evaluations
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf modelVersion
