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
-- Module      : Amazonka.FraudDetector.Types.ExternalModelOutputs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ExternalModelOutputs where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.ExternalModelSummary
import qualified Amazonka.Prelude as Prelude

-- | The fraud prediction scores from Amazon SageMaker model.
--
-- /See:/ 'newExternalModelOutputs' smart constructor.
data ExternalModelOutputs = ExternalModelOutputs'
  { -- | The Amazon SageMaker model.
    externalModel :: Prelude.Maybe ExternalModelSummary,
    -- | The fraud prediction scores from Amazon SageMaker model.
    outputs :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExternalModelOutputs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalModel', 'externalModelOutputs_externalModel' - The Amazon SageMaker model.
--
-- 'outputs', 'externalModelOutputs_outputs' - The fraud prediction scores from Amazon SageMaker model.
newExternalModelOutputs ::
  ExternalModelOutputs
newExternalModelOutputs =
  ExternalModelOutputs'
    { externalModel =
        Prelude.Nothing,
      outputs = Prelude.Nothing
    }

-- | The Amazon SageMaker model.
externalModelOutputs_externalModel :: Lens.Lens' ExternalModelOutputs (Prelude.Maybe ExternalModelSummary)
externalModelOutputs_externalModel = Lens.lens (\ExternalModelOutputs' {externalModel} -> externalModel) (\s@ExternalModelOutputs' {} a -> s {externalModel = a} :: ExternalModelOutputs)

-- | The fraud prediction scores from Amazon SageMaker model.
externalModelOutputs_outputs :: Lens.Lens' ExternalModelOutputs (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
externalModelOutputs_outputs = Lens.lens (\ExternalModelOutputs' {outputs} -> outputs) (\s@ExternalModelOutputs' {} a -> s {outputs = a} :: ExternalModelOutputs) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ExternalModelOutputs where
  parseJSON =
    Data.withObject
      "ExternalModelOutputs"
      ( \x ->
          ExternalModelOutputs'
            Prelude.<$> (x Data..:? "externalModel")
            Prelude.<*> (x Data..:? "outputs" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ExternalModelOutputs where
  hashWithSalt _salt ExternalModelOutputs' {..} =
    _salt
      `Prelude.hashWithSalt` externalModel
      `Prelude.hashWithSalt` outputs

instance Prelude.NFData ExternalModelOutputs where
  rnf ExternalModelOutputs' {..} =
    Prelude.rnf externalModel
      `Prelude.seq` Prelude.rnf outputs
