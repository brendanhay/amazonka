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
-- Module      : Amazonka.SageMaker.Types.ConvergenceDetected
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ConvergenceDetected where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CompleteOnConvergence

-- | A flag to indicating that automatic model tuning (AMT) has detected
-- model convergence, defined as a lack of significant improvement (1% or
-- less) against an objective metric.
--
-- /See:/ 'newConvergenceDetected' smart constructor.
data ConvergenceDetected = ConvergenceDetected'
  { -- | A flag to stop a tuning job once AMT has detected that the job has
    -- converged.
    completeOnConvergence :: Prelude.Maybe CompleteOnConvergence
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConvergenceDetected' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completeOnConvergence', 'convergenceDetected_completeOnConvergence' - A flag to stop a tuning job once AMT has detected that the job has
-- converged.
newConvergenceDetected ::
  ConvergenceDetected
newConvergenceDetected =
  ConvergenceDetected'
    { completeOnConvergence =
        Prelude.Nothing
    }

-- | A flag to stop a tuning job once AMT has detected that the job has
-- converged.
convergenceDetected_completeOnConvergence :: Lens.Lens' ConvergenceDetected (Prelude.Maybe CompleteOnConvergence)
convergenceDetected_completeOnConvergence = Lens.lens (\ConvergenceDetected' {completeOnConvergence} -> completeOnConvergence) (\s@ConvergenceDetected' {} a -> s {completeOnConvergence = a} :: ConvergenceDetected)

instance Data.FromJSON ConvergenceDetected where
  parseJSON =
    Data.withObject
      "ConvergenceDetected"
      ( \x ->
          ConvergenceDetected'
            Prelude.<$> (x Data..:? "CompleteOnConvergence")
      )

instance Prelude.Hashable ConvergenceDetected where
  hashWithSalt _salt ConvergenceDetected' {..} =
    _salt `Prelude.hashWithSalt` completeOnConvergence

instance Prelude.NFData ConvergenceDetected where
  rnf ConvergenceDetected' {..} =
    Prelude.rnf completeOnConvergence

instance Data.ToJSON ConvergenceDetected where
  toJSON ConvergenceDetected' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CompleteOnConvergence" Data..=)
              Prelude.<$> completeOnConvergence
          ]
      )
