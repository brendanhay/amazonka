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
-- Module      : Amazonka.FraudDetector.Types.EvaluatedExternalModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.EvaluatedExternalModel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of the external (Amazon Sagemaker) model evaluated for
-- generating predictions.
--
-- /See:/ 'newEvaluatedExternalModel' smart constructor.
data EvaluatedExternalModel = EvaluatedExternalModel'
  { -- | Input variables use for generating predictions.
    inputVariables :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The endpoint of the external (Amazon Sagemaker) model.
    modelEndpoint :: Prelude.Maybe Prelude.Text,
    -- | Output variables.
    outputVariables :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Indicates whether event variables were used to generate predictions.
    useEventVariables :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluatedExternalModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputVariables', 'evaluatedExternalModel_inputVariables' - Input variables use for generating predictions.
--
-- 'modelEndpoint', 'evaluatedExternalModel_modelEndpoint' - The endpoint of the external (Amazon Sagemaker) model.
--
-- 'outputVariables', 'evaluatedExternalModel_outputVariables' - Output variables.
--
-- 'useEventVariables', 'evaluatedExternalModel_useEventVariables' - Indicates whether event variables were used to generate predictions.
newEvaluatedExternalModel ::
  EvaluatedExternalModel
newEvaluatedExternalModel =
  EvaluatedExternalModel'
    { inputVariables =
        Prelude.Nothing,
      modelEndpoint = Prelude.Nothing,
      outputVariables = Prelude.Nothing,
      useEventVariables = Prelude.Nothing
    }

-- | Input variables use for generating predictions.
evaluatedExternalModel_inputVariables :: Lens.Lens' EvaluatedExternalModel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
evaluatedExternalModel_inputVariables = Lens.lens (\EvaluatedExternalModel' {inputVariables} -> inputVariables) (\s@EvaluatedExternalModel' {} a -> s {inputVariables = a} :: EvaluatedExternalModel) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The endpoint of the external (Amazon Sagemaker) model.
evaluatedExternalModel_modelEndpoint :: Lens.Lens' EvaluatedExternalModel (Prelude.Maybe Prelude.Text)
evaluatedExternalModel_modelEndpoint = Lens.lens (\EvaluatedExternalModel' {modelEndpoint} -> modelEndpoint) (\s@EvaluatedExternalModel' {} a -> s {modelEndpoint = a} :: EvaluatedExternalModel)

-- | Output variables.
evaluatedExternalModel_outputVariables :: Lens.Lens' EvaluatedExternalModel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
evaluatedExternalModel_outputVariables = Lens.lens (\EvaluatedExternalModel' {outputVariables} -> outputVariables) (\s@EvaluatedExternalModel' {} a -> s {outputVariables = a} :: EvaluatedExternalModel) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Indicates whether event variables were used to generate predictions.
evaluatedExternalModel_useEventVariables :: Lens.Lens' EvaluatedExternalModel (Prelude.Maybe Prelude.Bool)
evaluatedExternalModel_useEventVariables = Lens.lens (\EvaluatedExternalModel' {useEventVariables} -> useEventVariables) (\s@EvaluatedExternalModel' {} a -> s {useEventVariables = a} :: EvaluatedExternalModel)

instance Data.FromJSON EvaluatedExternalModel where
  parseJSON =
    Data.withObject
      "EvaluatedExternalModel"
      ( \x ->
          EvaluatedExternalModel'
            Prelude.<$> (x Data..:? "inputVariables" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "modelEndpoint")
            Prelude.<*> ( x Data..:? "outputVariables"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "useEventVariables")
      )

instance Prelude.Hashable EvaluatedExternalModel where
  hashWithSalt _salt EvaluatedExternalModel' {..} =
    _salt `Prelude.hashWithSalt` inputVariables
      `Prelude.hashWithSalt` modelEndpoint
      `Prelude.hashWithSalt` outputVariables
      `Prelude.hashWithSalt` useEventVariables

instance Prelude.NFData EvaluatedExternalModel where
  rnf EvaluatedExternalModel' {..} =
    Prelude.rnf inputVariables
      `Prelude.seq` Prelude.rnf modelEndpoint
      `Prelude.seq` Prelude.rnf outputVariables
      `Prelude.seq` Prelude.rnf useEventVariables
