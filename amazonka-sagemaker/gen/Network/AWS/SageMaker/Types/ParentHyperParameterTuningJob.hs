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
-- Module      : Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A previously completed or stopped hyperparameter tuning job to be used
-- as a starting point for a new hyperparameter tuning job.
--
-- /See:/ 'newParentHyperParameterTuningJob' smart constructor.
data ParentHyperParameterTuningJob = ParentHyperParameterTuningJob'
  { -- | The name of the hyperparameter tuning job to be used as a starting point
    -- for a new hyperparameter tuning job.
    hyperParameterTuningJobName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ParentHyperParameterTuningJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hyperParameterTuningJobName', 'parentHyperParameterTuningJob_hyperParameterTuningJobName' - The name of the hyperparameter tuning job to be used as a starting point
-- for a new hyperparameter tuning job.
newParentHyperParameterTuningJob ::
  ParentHyperParameterTuningJob
newParentHyperParameterTuningJob =
  ParentHyperParameterTuningJob'
    { hyperParameterTuningJobName =
        Core.Nothing
    }

-- | The name of the hyperparameter tuning job to be used as a starting point
-- for a new hyperparameter tuning job.
parentHyperParameterTuningJob_hyperParameterTuningJobName :: Lens.Lens' ParentHyperParameterTuningJob (Core.Maybe Core.Text)
parentHyperParameterTuningJob_hyperParameterTuningJobName = Lens.lens (\ParentHyperParameterTuningJob' {hyperParameterTuningJobName} -> hyperParameterTuningJobName) (\s@ParentHyperParameterTuningJob' {} a -> s {hyperParameterTuningJobName = a} :: ParentHyperParameterTuningJob)

instance Core.FromJSON ParentHyperParameterTuningJob where
  parseJSON =
    Core.withObject
      "ParentHyperParameterTuningJob"
      ( \x ->
          ParentHyperParameterTuningJob'
            Core.<$> (x Core..:? "HyperParameterTuningJobName")
      )

instance Core.Hashable ParentHyperParameterTuningJob

instance Core.NFData ParentHyperParameterTuningJob

instance Core.ToJSON ParentHyperParameterTuningJob where
  toJSON ParentHyperParameterTuningJob' {..} =
    Core.object
      ( Core.catMaybes
          [ ("HyperParameterTuningJobName" Core..=)
              Core.<$> hyperParameterTuningJobName
          ]
      )
