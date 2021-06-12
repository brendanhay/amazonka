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
-- Module      : Network.AWS.SageMaker.Types.ConditionStepMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ConditionStepMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ConditionOutcome

-- | Metadata for a Condition step.
--
-- /See:/ 'newConditionStepMetadata' smart constructor.
data ConditionStepMetadata = ConditionStepMetadata'
  { -- | The outcome of the Condition step evaluation.
    outcome :: Core.Maybe ConditionOutcome
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  ConditionStepMetadata' {outcome = Core.Nothing}

-- | The outcome of the Condition step evaluation.
conditionStepMetadata_outcome :: Lens.Lens' ConditionStepMetadata (Core.Maybe ConditionOutcome)
conditionStepMetadata_outcome = Lens.lens (\ConditionStepMetadata' {outcome} -> outcome) (\s@ConditionStepMetadata' {} a -> s {outcome = a} :: ConditionStepMetadata)

instance Core.FromJSON ConditionStepMetadata where
  parseJSON =
    Core.withObject
      "ConditionStepMetadata"
      ( \x ->
          ConditionStepMetadata'
            Core.<$> (x Core..:? "Outcome")
      )

instance Core.Hashable ConditionStepMetadata

instance Core.NFData ConditionStepMetadata
