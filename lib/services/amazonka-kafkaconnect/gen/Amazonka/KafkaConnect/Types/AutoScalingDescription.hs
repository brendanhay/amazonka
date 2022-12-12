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
-- Module      : Amazonka.KafkaConnect.Types.AutoScalingDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.AutoScalingDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types.ScaleInPolicyDescription
import Amazonka.KafkaConnect.Types.ScaleOutPolicyDescription
import qualified Amazonka.Prelude as Prelude

-- | Information about the auto scaling parameters for the connector.
--
-- /See:/ 'newAutoScalingDescription' smart constructor.
data AutoScalingDescription = AutoScalingDescription'
  { -- | The maximum number of workers allocated to the connector.
    maxWorkerCount :: Prelude.Maybe Prelude.Int,
    -- | The number of microcontroller units (MCUs) allocated to each connector
    -- worker. The valid values are 1,2,4,8.
    mcuCount :: Prelude.Maybe Prelude.Int,
    -- | The minimum number of workers allocated to the connector.
    minWorkerCount :: Prelude.Maybe Prelude.Int,
    -- | The sacle-in policy for the connector.
    scaleInPolicy :: Prelude.Maybe ScaleInPolicyDescription,
    -- | The sacle-out policy for the connector.>
    scaleOutPolicy :: Prelude.Maybe ScaleOutPolicyDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxWorkerCount', 'autoScalingDescription_maxWorkerCount' - The maximum number of workers allocated to the connector.
--
-- 'mcuCount', 'autoScalingDescription_mcuCount' - The number of microcontroller units (MCUs) allocated to each connector
-- worker. The valid values are 1,2,4,8.
--
-- 'minWorkerCount', 'autoScalingDescription_minWorkerCount' - The minimum number of workers allocated to the connector.
--
-- 'scaleInPolicy', 'autoScalingDescription_scaleInPolicy' - The sacle-in policy for the connector.
--
-- 'scaleOutPolicy', 'autoScalingDescription_scaleOutPolicy' - The sacle-out policy for the connector.>
newAutoScalingDescription ::
  AutoScalingDescription
newAutoScalingDescription =
  AutoScalingDescription'
    { maxWorkerCount =
        Prelude.Nothing,
      mcuCount = Prelude.Nothing,
      minWorkerCount = Prelude.Nothing,
      scaleInPolicy = Prelude.Nothing,
      scaleOutPolicy = Prelude.Nothing
    }

-- | The maximum number of workers allocated to the connector.
autoScalingDescription_maxWorkerCount :: Lens.Lens' AutoScalingDescription (Prelude.Maybe Prelude.Int)
autoScalingDescription_maxWorkerCount = Lens.lens (\AutoScalingDescription' {maxWorkerCount} -> maxWorkerCount) (\s@AutoScalingDescription' {} a -> s {maxWorkerCount = a} :: AutoScalingDescription)

-- | The number of microcontroller units (MCUs) allocated to each connector
-- worker. The valid values are 1,2,4,8.
autoScalingDescription_mcuCount :: Lens.Lens' AutoScalingDescription (Prelude.Maybe Prelude.Int)
autoScalingDescription_mcuCount = Lens.lens (\AutoScalingDescription' {mcuCount} -> mcuCount) (\s@AutoScalingDescription' {} a -> s {mcuCount = a} :: AutoScalingDescription)

-- | The minimum number of workers allocated to the connector.
autoScalingDescription_minWorkerCount :: Lens.Lens' AutoScalingDescription (Prelude.Maybe Prelude.Int)
autoScalingDescription_minWorkerCount = Lens.lens (\AutoScalingDescription' {minWorkerCount} -> minWorkerCount) (\s@AutoScalingDescription' {} a -> s {minWorkerCount = a} :: AutoScalingDescription)

-- | The sacle-in policy for the connector.
autoScalingDescription_scaleInPolicy :: Lens.Lens' AutoScalingDescription (Prelude.Maybe ScaleInPolicyDescription)
autoScalingDescription_scaleInPolicy = Lens.lens (\AutoScalingDescription' {scaleInPolicy} -> scaleInPolicy) (\s@AutoScalingDescription' {} a -> s {scaleInPolicy = a} :: AutoScalingDescription)

-- | The sacle-out policy for the connector.>
autoScalingDescription_scaleOutPolicy :: Lens.Lens' AutoScalingDescription (Prelude.Maybe ScaleOutPolicyDescription)
autoScalingDescription_scaleOutPolicy = Lens.lens (\AutoScalingDescription' {scaleOutPolicy} -> scaleOutPolicy) (\s@AutoScalingDescription' {} a -> s {scaleOutPolicy = a} :: AutoScalingDescription)

instance Data.FromJSON AutoScalingDescription where
  parseJSON =
    Data.withObject
      "AutoScalingDescription"
      ( \x ->
          AutoScalingDescription'
            Prelude.<$> (x Data..:? "maxWorkerCount")
            Prelude.<*> (x Data..:? "mcuCount")
            Prelude.<*> (x Data..:? "minWorkerCount")
            Prelude.<*> (x Data..:? "scaleInPolicy")
            Prelude.<*> (x Data..:? "scaleOutPolicy")
      )

instance Prelude.Hashable AutoScalingDescription where
  hashWithSalt _salt AutoScalingDescription' {..} =
    _salt `Prelude.hashWithSalt` maxWorkerCount
      `Prelude.hashWithSalt` mcuCount
      `Prelude.hashWithSalt` minWorkerCount
      `Prelude.hashWithSalt` scaleInPolicy
      `Prelude.hashWithSalt` scaleOutPolicy

instance Prelude.NFData AutoScalingDescription where
  rnf AutoScalingDescription' {..} =
    Prelude.rnf maxWorkerCount
      `Prelude.seq` Prelude.rnf mcuCount
      `Prelude.seq` Prelude.rnf minWorkerCount
      `Prelude.seq` Prelude.rnf scaleInPolicy
      `Prelude.seq` Prelude.rnf scaleOutPolicy
