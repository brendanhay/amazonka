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
-- Module      : Amazonka.KafkaConnect.Types.ScaleOutPolicyDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.ScaleOutPolicyDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The description of the scale-out policy for the connector.
--
-- /See:/ 'newScaleOutPolicyDescription' smart constructor.
data ScaleOutPolicyDescription = ScaleOutPolicyDescription'
  { -- | The CPU utilization percentage threshold at which you want connector
    -- scale out to be triggered.
    cpuUtilizationPercentage :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScaleOutPolicyDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuUtilizationPercentage', 'scaleOutPolicyDescription_cpuUtilizationPercentage' - The CPU utilization percentage threshold at which you want connector
-- scale out to be triggered.
newScaleOutPolicyDescription ::
  ScaleOutPolicyDescription
newScaleOutPolicyDescription =
  ScaleOutPolicyDescription'
    { cpuUtilizationPercentage =
        Prelude.Nothing
    }

-- | The CPU utilization percentage threshold at which you want connector
-- scale out to be triggered.
scaleOutPolicyDescription_cpuUtilizationPercentage :: Lens.Lens' ScaleOutPolicyDescription (Prelude.Maybe Prelude.Int)
scaleOutPolicyDescription_cpuUtilizationPercentage = Lens.lens (\ScaleOutPolicyDescription' {cpuUtilizationPercentage} -> cpuUtilizationPercentage) (\s@ScaleOutPolicyDescription' {} a -> s {cpuUtilizationPercentage = a} :: ScaleOutPolicyDescription)

instance Core.FromJSON ScaleOutPolicyDescription where
  parseJSON =
    Core.withObject
      "ScaleOutPolicyDescription"
      ( \x ->
          ScaleOutPolicyDescription'
            Prelude.<$> (x Core..:? "cpuUtilizationPercentage")
      )

instance Prelude.Hashable ScaleOutPolicyDescription where
  hashWithSalt _salt ScaleOutPolicyDescription' {..} =
    _salt
      `Prelude.hashWithSalt` cpuUtilizationPercentage

instance Prelude.NFData ScaleOutPolicyDescription where
  rnf ScaleOutPolicyDescription' {..} =
    Prelude.rnf cpuUtilizationPercentage
