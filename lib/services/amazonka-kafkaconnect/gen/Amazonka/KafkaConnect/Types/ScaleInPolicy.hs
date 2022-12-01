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
-- Module      : Amazonka.KafkaConnect.Types.ScaleInPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.ScaleInPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The scale-in policy for the connector.
--
-- /See:/ 'newScaleInPolicy' smart constructor.
data ScaleInPolicy = ScaleInPolicy'
  { -- | Specifies the CPU utilization percentage threshold at which you want
    -- connector scale in to be triggered.
    cpuUtilizationPercentage :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScaleInPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuUtilizationPercentage', 'scaleInPolicy_cpuUtilizationPercentage' - Specifies the CPU utilization percentage threshold at which you want
-- connector scale in to be triggered.
newScaleInPolicy ::
  -- | 'cpuUtilizationPercentage'
  Prelude.Natural ->
  ScaleInPolicy
newScaleInPolicy pCpuUtilizationPercentage_ =
  ScaleInPolicy'
    { cpuUtilizationPercentage =
        pCpuUtilizationPercentage_
    }

-- | Specifies the CPU utilization percentage threshold at which you want
-- connector scale in to be triggered.
scaleInPolicy_cpuUtilizationPercentage :: Lens.Lens' ScaleInPolicy Prelude.Natural
scaleInPolicy_cpuUtilizationPercentage = Lens.lens (\ScaleInPolicy' {cpuUtilizationPercentage} -> cpuUtilizationPercentage) (\s@ScaleInPolicy' {} a -> s {cpuUtilizationPercentage = a} :: ScaleInPolicy)

instance Prelude.Hashable ScaleInPolicy where
  hashWithSalt _salt ScaleInPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` cpuUtilizationPercentage

instance Prelude.NFData ScaleInPolicy where
  rnf ScaleInPolicy' {..} =
    Prelude.rnf cpuUtilizationPercentage

instance Core.ToJSON ScaleInPolicy where
  toJSON ScaleInPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "cpuUtilizationPercentage"
                  Core..= cpuUtilizationPercentage
              )
          ]
      )
