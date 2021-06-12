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
-- Module      : Network.AWS.SageMaker.Types.TuningJobCompletionCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TuningJobCompletionCriteria where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The job completion criteria.
--
-- /See:/ 'newTuningJobCompletionCriteria' smart constructor.
data TuningJobCompletionCriteria = TuningJobCompletionCriteria'
  { -- | The value of the objective metric.
    targetObjectiveMetricValue :: Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Double ->
  TuningJobCompletionCriteria
newTuningJobCompletionCriteria
  pTargetObjectiveMetricValue_ =
    TuningJobCompletionCriteria'
      { targetObjectiveMetricValue =
          pTargetObjectiveMetricValue_
      }

-- | The value of the objective metric.
tuningJobCompletionCriteria_targetObjectiveMetricValue :: Lens.Lens' TuningJobCompletionCriteria Core.Double
tuningJobCompletionCriteria_targetObjectiveMetricValue = Lens.lens (\TuningJobCompletionCriteria' {targetObjectiveMetricValue} -> targetObjectiveMetricValue) (\s@TuningJobCompletionCriteria' {} a -> s {targetObjectiveMetricValue = a} :: TuningJobCompletionCriteria)

instance Core.FromJSON TuningJobCompletionCriteria where
  parseJSON =
    Core.withObject
      "TuningJobCompletionCriteria"
      ( \x ->
          TuningJobCompletionCriteria'
            Core.<$> (x Core..: "TargetObjectiveMetricValue")
      )

instance Core.Hashable TuningJobCompletionCriteria

instance Core.NFData TuningJobCompletionCriteria

instance Core.ToJSON TuningJobCompletionCriteria where
  toJSON TuningJobCompletionCriteria' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "TargetObjectiveMetricValue"
                  Core..= targetObjectiveMetricValue
              )
          ]
      )
