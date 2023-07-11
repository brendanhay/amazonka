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
-- Module      : Amazonka.SageMaker.Types.TuningJobCompletionCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TuningJobCompletionCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The job completion criteria.
--
-- /See:/ 'newTuningJobCompletionCriteria' smart constructor.
data TuningJobCompletionCriteria = TuningJobCompletionCriteria'
  { -- | The value of the objective metric.
    targetObjectiveMetricValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TuningJobCompletionCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetObjectiveMetricValue', 'tuningJobCompletionCriteria_targetObjectiveMetricValue' - The value of the objective metric.
newTuningJobCompletionCriteria ::
  -- | 'targetObjectiveMetricValue'
  Prelude.Double ->
  TuningJobCompletionCriteria
newTuningJobCompletionCriteria
  pTargetObjectiveMetricValue_ =
    TuningJobCompletionCriteria'
      { targetObjectiveMetricValue =
          pTargetObjectiveMetricValue_
      }

-- | The value of the objective metric.
tuningJobCompletionCriteria_targetObjectiveMetricValue :: Lens.Lens' TuningJobCompletionCriteria Prelude.Double
tuningJobCompletionCriteria_targetObjectiveMetricValue = Lens.lens (\TuningJobCompletionCriteria' {targetObjectiveMetricValue} -> targetObjectiveMetricValue) (\s@TuningJobCompletionCriteria' {} a -> s {targetObjectiveMetricValue = a} :: TuningJobCompletionCriteria)

instance Data.FromJSON TuningJobCompletionCriteria where
  parseJSON =
    Data.withObject
      "TuningJobCompletionCriteria"
      ( \x ->
          TuningJobCompletionCriteria'
            Prelude.<$> (x Data..: "TargetObjectiveMetricValue")
      )

instance Prelude.Hashable TuningJobCompletionCriteria where
  hashWithSalt _salt TuningJobCompletionCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` targetObjectiveMetricValue

instance Prelude.NFData TuningJobCompletionCriteria where
  rnf TuningJobCompletionCriteria' {..} =
    Prelude.rnf targetObjectiveMetricValue

instance Data.ToJSON TuningJobCompletionCriteria where
  toJSON TuningJobCompletionCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TargetObjectiveMetricValue"
                  Data..= targetObjectiveMetricValue
              )
          ]
      )
