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
-- Module      : Amazonka.KafkaConnect.Types.ScaleInPolicyUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.ScaleInPolicyUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An update to the connector\'s scale-in policy.
--
-- /See:/ 'newScaleInPolicyUpdate' smart constructor.
data ScaleInPolicyUpdate = ScaleInPolicyUpdate'
  { -- | The target CPU utilization percentage threshold at which you want
    -- connector scale in to be triggered.
    cpuUtilizationPercentage :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScaleInPolicyUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuUtilizationPercentage', 'scaleInPolicyUpdate_cpuUtilizationPercentage' - The target CPU utilization percentage threshold at which you want
-- connector scale in to be triggered.
newScaleInPolicyUpdate ::
  -- | 'cpuUtilizationPercentage'
  Prelude.Natural ->
  ScaleInPolicyUpdate
newScaleInPolicyUpdate pCpuUtilizationPercentage_ =
  ScaleInPolicyUpdate'
    { cpuUtilizationPercentage =
        pCpuUtilizationPercentage_
    }

-- | The target CPU utilization percentage threshold at which you want
-- connector scale in to be triggered.
scaleInPolicyUpdate_cpuUtilizationPercentage :: Lens.Lens' ScaleInPolicyUpdate Prelude.Natural
scaleInPolicyUpdate_cpuUtilizationPercentage = Lens.lens (\ScaleInPolicyUpdate' {cpuUtilizationPercentage} -> cpuUtilizationPercentage) (\s@ScaleInPolicyUpdate' {} a -> s {cpuUtilizationPercentage = a} :: ScaleInPolicyUpdate)

instance Prelude.Hashable ScaleInPolicyUpdate where
  hashWithSalt _salt ScaleInPolicyUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` cpuUtilizationPercentage

instance Prelude.NFData ScaleInPolicyUpdate where
  rnf ScaleInPolicyUpdate' {..} =
    Prelude.rnf cpuUtilizationPercentage

instance Data.ToJSON ScaleInPolicyUpdate where
  toJSON ScaleInPolicyUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "cpuUtilizationPercentage"
                  Data..= cpuUtilizationPercentage
              )
          ]
      )
