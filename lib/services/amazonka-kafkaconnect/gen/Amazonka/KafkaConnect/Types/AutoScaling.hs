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
-- Module      : Amazonka.KafkaConnect.Types.AutoScaling
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.AutoScaling where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KafkaConnect.Types.ScaleInPolicy
import Amazonka.KafkaConnect.Types.ScaleOutPolicy
import qualified Amazonka.Prelude as Prelude

-- | Specifies how the connector scales.
--
-- /See:/ 'newAutoScaling' smart constructor.
data AutoScaling = AutoScaling'
  { -- | The sacle-out policy for the connector.
    scaleOutPolicy :: Prelude.Maybe ScaleOutPolicy,
    -- | The sacle-in policy for the connector.
    scaleInPolicy :: Prelude.Maybe ScaleInPolicy,
    -- | The maximum number of workers allocated to the connector.
    maxWorkerCount :: Prelude.Natural,
    -- | The number of microcontroller units (MCUs) allocated to each connector
    -- worker. The valid values are 1,2,4,8.
    mcuCount :: Prelude.Natural,
    -- | The minimum number of workers allocated to the connector.
    minWorkerCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoScaling' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scaleOutPolicy', 'autoScaling_scaleOutPolicy' - The sacle-out policy for the connector.
--
-- 'scaleInPolicy', 'autoScaling_scaleInPolicy' - The sacle-in policy for the connector.
--
-- 'maxWorkerCount', 'autoScaling_maxWorkerCount' - The maximum number of workers allocated to the connector.
--
-- 'mcuCount', 'autoScaling_mcuCount' - The number of microcontroller units (MCUs) allocated to each connector
-- worker. The valid values are 1,2,4,8.
--
-- 'minWorkerCount', 'autoScaling_minWorkerCount' - The minimum number of workers allocated to the connector.
newAutoScaling ::
  -- | 'maxWorkerCount'
  Prelude.Natural ->
  -- | 'mcuCount'
  Prelude.Natural ->
  -- | 'minWorkerCount'
  Prelude.Natural ->
  AutoScaling
newAutoScaling
  pMaxWorkerCount_
  pMcuCount_
  pMinWorkerCount_ =
    AutoScaling'
      { scaleOutPolicy = Prelude.Nothing,
        scaleInPolicy = Prelude.Nothing,
        maxWorkerCount = pMaxWorkerCount_,
        mcuCount = pMcuCount_,
        minWorkerCount = pMinWorkerCount_
      }

-- | The sacle-out policy for the connector.
autoScaling_scaleOutPolicy :: Lens.Lens' AutoScaling (Prelude.Maybe ScaleOutPolicy)
autoScaling_scaleOutPolicy = Lens.lens (\AutoScaling' {scaleOutPolicy} -> scaleOutPolicy) (\s@AutoScaling' {} a -> s {scaleOutPolicy = a} :: AutoScaling)

-- | The sacle-in policy for the connector.
autoScaling_scaleInPolicy :: Lens.Lens' AutoScaling (Prelude.Maybe ScaleInPolicy)
autoScaling_scaleInPolicy = Lens.lens (\AutoScaling' {scaleInPolicy} -> scaleInPolicy) (\s@AutoScaling' {} a -> s {scaleInPolicy = a} :: AutoScaling)

-- | The maximum number of workers allocated to the connector.
autoScaling_maxWorkerCount :: Lens.Lens' AutoScaling Prelude.Natural
autoScaling_maxWorkerCount = Lens.lens (\AutoScaling' {maxWorkerCount} -> maxWorkerCount) (\s@AutoScaling' {} a -> s {maxWorkerCount = a} :: AutoScaling)

-- | The number of microcontroller units (MCUs) allocated to each connector
-- worker. The valid values are 1,2,4,8.
autoScaling_mcuCount :: Lens.Lens' AutoScaling Prelude.Natural
autoScaling_mcuCount = Lens.lens (\AutoScaling' {mcuCount} -> mcuCount) (\s@AutoScaling' {} a -> s {mcuCount = a} :: AutoScaling)

-- | The minimum number of workers allocated to the connector.
autoScaling_minWorkerCount :: Lens.Lens' AutoScaling Prelude.Natural
autoScaling_minWorkerCount = Lens.lens (\AutoScaling' {minWorkerCount} -> minWorkerCount) (\s@AutoScaling' {} a -> s {minWorkerCount = a} :: AutoScaling)

instance Prelude.Hashable AutoScaling where
  hashWithSalt _salt AutoScaling' {..} =
    _salt `Prelude.hashWithSalt` scaleOutPolicy
      `Prelude.hashWithSalt` scaleInPolicy
      `Prelude.hashWithSalt` maxWorkerCount
      `Prelude.hashWithSalt` mcuCount
      `Prelude.hashWithSalt` minWorkerCount

instance Prelude.NFData AutoScaling where
  rnf AutoScaling' {..} =
    Prelude.rnf scaleOutPolicy
      `Prelude.seq` Prelude.rnf scaleInPolicy
      `Prelude.seq` Prelude.rnf maxWorkerCount
      `Prelude.seq` Prelude.rnf mcuCount
      `Prelude.seq` Prelude.rnf minWorkerCount

instance Core.ToJSON AutoScaling where
  toJSON AutoScaling' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("scaleOutPolicy" Core..=)
              Prelude.<$> scaleOutPolicy,
            ("scaleInPolicy" Core..=) Prelude.<$> scaleInPolicy,
            Prelude.Just
              ("maxWorkerCount" Core..= maxWorkerCount),
            Prelude.Just ("mcuCount" Core..= mcuCount),
            Prelude.Just
              ("minWorkerCount" Core..= minWorkerCount)
          ]
      )
