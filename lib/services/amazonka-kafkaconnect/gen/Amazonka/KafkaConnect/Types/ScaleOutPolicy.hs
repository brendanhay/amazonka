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
-- Module      : Amazonka.KafkaConnect.Types.ScaleOutPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.ScaleOutPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The scale-out policy for the connector.
--
-- /See:/ 'newScaleOutPolicy' smart constructor.
data ScaleOutPolicy = ScaleOutPolicy'
  { -- | The CPU utilization percentage threshold at which you want connector
    -- scale out to be triggered.
    cpuUtilizationPercentage :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScaleOutPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuUtilizationPercentage', 'scaleOutPolicy_cpuUtilizationPercentage' - The CPU utilization percentage threshold at which you want connector
-- scale out to be triggered.
newScaleOutPolicy ::
  -- | 'cpuUtilizationPercentage'
  Prelude.Natural ->
  ScaleOutPolicy
newScaleOutPolicy pCpuUtilizationPercentage_ =
  ScaleOutPolicy'
    { cpuUtilizationPercentage =
        pCpuUtilizationPercentage_
    }

-- | The CPU utilization percentage threshold at which you want connector
-- scale out to be triggered.
scaleOutPolicy_cpuUtilizationPercentage :: Lens.Lens' ScaleOutPolicy Prelude.Natural
scaleOutPolicy_cpuUtilizationPercentage = Lens.lens (\ScaleOutPolicy' {cpuUtilizationPercentage} -> cpuUtilizationPercentage) (\s@ScaleOutPolicy' {} a -> s {cpuUtilizationPercentage = a} :: ScaleOutPolicy)

instance Prelude.Hashable ScaleOutPolicy where
  hashWithSalt _salt ScaleOutPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` cpuUtilizationPercentage

instance Prelude.NFData ScaleOutPolicy where
  rnf ScaleOutPolicy' {..} =
    Prelude.rnf cpuUtilizationPercentage

instance Core.ToJSON ScaleOutPolicy where
  toJSON ScaleOutPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "cpuUtilizationPercentage"
                  Core..= cpuUtilizationPercentage
              )
          ]
      )
