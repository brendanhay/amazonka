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
-- Module      : Amazonka.SageMaker.Types.ConditionStepMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ConditionStepMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ConditionOutcome

-- | Metadata for a Condition step.
--
-- /See:/ 'newConditionStepMetadata' smart constructor.
data ConditionStepMetadata = ConditionStepMetadata'
  { -- | The outcome of the Condition step evaluation.
    outcome :: Prelude.Maybe ConditionOutcome
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConditionStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outcome', 'conditionStepMetadata_outcome' - The outcome of the Condition step evaluation.
newConditionStepMetadata ::
  ConditionStepMetadata
newConditionStepMetadata =
  ConditionStepMetadata' {outcome = Prelude.Nothing}

-- | The outcome of the Condition step evaluation.
conditionStepMetadata_outcome :: Lens.Lens' ConditionStepMetadata (Prelude.Maybe ConditionOutcome)
conditionStepMetadata_outcome = Lens.lens (\ConditionStepMetadata' {outcome} -> outcome) (\s@ConditionStepMetadata' {} a -> s {outcome = a} :: ConditionStepMetadata)

instance Data.FromJSON ConditionStepMetadata where
  parseJSON =
    Data.withObject
      "ConditionStepMetadata"
      ( \x ->
          ConditionStepMetadata'
            Prelude.<$> (x Data..:? "Outcome")
      )

instance Prelude.Hashable ConditionStepMetadata where
  hashWithSalt _salt ConditionStepMetadata' {..} =
    _salt `Prelude.hashWithSalt` outcome

instance Prelude.NFData ConditionStepMetadata where
  rnf ConditionStepMetadata' {..} = Prelude.rnf outcome
