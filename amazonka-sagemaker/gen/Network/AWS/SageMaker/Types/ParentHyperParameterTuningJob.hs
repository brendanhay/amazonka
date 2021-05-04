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
-- Module      : Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A previously completed or stopped hyperparameter tuning job to be used
-- as a starting point for a new hyperparameter tuning job.
--
-- /See:/ 'newParentHyperParameterTuningJob' smart constructor.
data ParentHyperParameterTuningJob = ParentHyperParameterTuningJob'
  { -- | The name of the hyperparameter tuning job to be used as a starting point
    -- for a new hyperparameter tuning job.
    hyperParameterTuningJobName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The name of the hyperparameter tuning job to be used as a starting point
-- for a new hyperparameter tuning job.
parentHyperParameterTuningJob_hyperParameterTuningJobName :: Lens.Lens' ParentHyperParameterTuningJob (Prelude.Maybe Prelude.Text)
parentHyperParameterTuningJob_hyperParameterTuningJobName = Lens.lens (\ParentHyperParameterTuningJob' {hyperParameterTuningJobName} -> hyperParameterTuningJobName) (\s@ParentHyperParameterTuningJob' {} a -> s {hyperParameterTuningJobName = a} :: ParentHyperParameterTuningJob)

instance
  Prelude.FromJSON
    ParentHyperParameterTuningJob
  where
  parseJSON =
    Prelude.withObject
      "ParentHyperParameterTuningJob"
      ( \x ->
          ParentHyperParameterTuningJob'
            Prelude.<$> (x Prelude..:? "HyperParameterTuningJobName")
      )

instance
  Prelude.Hashable
    ParentHyperParameterTuningJob

instance Prelude.NFData ParentHyperParameterTuningJob

instance Prelude.ToJSON ParentHyperParameterTuningJob where
  toJSON ParentHyperParameterTuningJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("HyperParameterTuningJobName" Prelude..=)
              Prelude.<$> hyperParameterTuningJobName
          ]
      )
