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
-- Module      : Amazonka.KafkaConnect.Types.ScaleInPolicyDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.ScaleInPolicyDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The description of the scale-in policy for the connector.
--
-- /See:/ 'newScaleInPolicyDescription' smart constructor.
data ScaleInPolicyDescription = ScaleInPolicyDescription'
  { -- | Specifies the CPU utilization percentage threshold at which you want
    -- connector scale in to be triggered.
    cpuUtilizationPercentage :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScaleInPolicyDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuUtilizationPercentage', 'scaleInPolicyDescription_cpuUtilizationPercentage' - Specifies the CPU utilization percentage threshold at which you want
-- connector scale in to be triggered.
newScaleInPolicyDescription ::
  ScaleInPolicyDescription
newScaleInPolicyDescription =
  ScaleInPolicyDescription'
    { cpuUtilizationPercentage =
        Prelude.Nothing
    }

-- | Specifies the CPU utilization percentage threshold at which you want
-- connector scale in to be triggered.
scaleInPolicyDescription_cpuUtilizationPercentage :: Lens.Lens' ScaleInPolicyDescription (Prelude.Maybe Prelude.Int)
scaleInPolicyDescription_cpuUtilizationPercentage = Lens.lens (\ScaleInPolicyDescription' {cpuUtilizationPercentage} -> cpuUtilizationPercentage) (\s@ScaleInPolicyDescription' {} a -> s {cpuUtilizationPercentage = a} :: ScaleInPolicyDescription)

instance Data.FromJSON ScaleInPolicyDescription where
  parseJSON =
    Data.withObject
      "ScaleInPolicyDescription"
      ( \x ->
          ScaleInPolicyDescription'
            Prelude.<$> (x Data..:? "cpuUtilizationPercentage")
      )

instance Prelude.Hashable ScaleInPolicyDescription where
  hashWithSalt _salt ScaleInPolicyDescription' {..} =
    _salt
      `Prelude.hashWithSalt` cpuUtilizationPercentage

instance Prelude.NFData ScaleInPolicyDescription where
  rnf ScaleInPolicyDescription' {..} =
    Prelude.rnf cpuUtilizationPercentage
