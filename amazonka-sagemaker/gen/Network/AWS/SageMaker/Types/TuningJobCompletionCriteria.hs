{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The job completion criteria.
--
-- /See:/ 'newTuningJobCompletionCriteria' smart constructor.
data TuningJobCompletionCriteria = TuningJobCompletionCriteria'
  { -- | The value of the objective metric.
    targetObjectiveMetricValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON TuningJobCompletionCriteria where
  parseJSON =
    Prelude.withObject
      "TuningJobCompletionCriteria"
      ( \x ->
          TuningJobCompletionCriteria'
            Prelude.<$> (x Prelude..: "TargetObjectiveMetricValue")
      )

instance Prelude.Hashable TuningJobCompletionCriteria

instance Prelude.NFData TuningJobCompletionCriteria

instance Prelude.ToJSON TuningJobCompletionCriteria where
  toJSON TuningJobCompletionCriteria' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TargetObjectiveMetricValue"
                  Prelude..= targetObjectiveMetricValue
              )
          ]
      )
