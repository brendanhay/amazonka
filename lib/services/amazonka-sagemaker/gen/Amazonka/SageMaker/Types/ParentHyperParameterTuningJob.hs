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
-- Module      : Amazonka.SageMaker.Types.ParentHyperParameterTuningJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ParentHyperParameterTuningJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A previously completed or stopped hyperparameter tuning job to be used
-- as a starting point for a new hyperparameter tuning job.
--
-- /See:/ 'newParentHyperParameterTuningJob' smart constructor.
data ParentHyperParameterTuningJob = ParentHyperParameterTuningJob'
  { -- | The name of the hyperparameter tuning job to be used as a starting point
    -- for a new hyperparameter tuning job.
    hyperParameterTuningJobName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.FromJSON ParentHyperParameterTuningJob where
  parseJSON =
    Core.withObject
      "ParentHyperParameterTuningJob"
      ( \x ->
          ParentHyperParameterTuningJob'
            Prelude.<$> (x Core..:? "HyperParameterTuningJobName")
      )

instance
  Prelude.Hashable
    ParentHyperParameterTuningJob
  where
  hashWithSalt _salt ParentHyperParameterTuningJob' {..} =
    _salt
      `Prelude.hashWithSalt` hyperParameterTuningJobName

instance Prelude.NFData ParentHyperParameterTuningJob where
  rnf ParentHyperParameterTuningJob' {..} =
    Prelude.rnf hyperParameterTuningJobName

instance Core.ToJSON ParentHyperParameterTuningJob where
  toJSON ParentHyperParameterTuningJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("HyperParameterTuningJobName" Core..=)
              Prelude.<$> hyperParameterTuningJobName
          ]
      )
