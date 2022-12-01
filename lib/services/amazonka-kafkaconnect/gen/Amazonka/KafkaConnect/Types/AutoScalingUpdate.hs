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
-- Module      : Amazonka.KafkaConnect.Types.AutoScalingUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.AutoScalingUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KafkaConnect.Types.ScaleInPolicyUpdate
import Amazonka.KafkaConnect.Types.ScaleOutPolicyUpdate
import qualified Amazonka.Prelude as Prelude

-- | The updates to the auto scaling parameters for the connector.
--
-- /See:/ 'newAutoScalingUpdate' smart constructor.
data AutoScalingUpdate = AutoScalingUpdate'
  { -- | The target maximum number of workers allocated to the connector.
    maxWorkerCount :: Prelude.Natural,
    -- | The target number of microcontroller units (MCUs) allocated to each
    -- connector worker. The valid values are 1,2,4,8.
    mcuCount :: Prelude.Natural,
    -- | The target minimum number of workers allocated to the connector.
    minWorkerCount :: Prelude.Natural,
    -- | The target sacle-in policy for the connector.
    scaleInPolicy :: ScaleInPolicyUpdate,
    -- | The target sacle-out policy for the connector.
    scaleOutPolicy :: ScaleOutPolicyUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxWorkerCount', 'autoScalingUpdate_maxWorkerCount' - The target maximum number of workers allocated to the connector.
--
-- 'mcuCount', 'autoScalingUpdate_mcuCount' - The target number of microcontroller units (MCUs) allocated to each
-- connector worker. The valid values are 1,2,4,8.
--
-- 'minWorkerCount', 'autoScalingUpdate_minWorkerCount' - The target minimum number of workers allocated to the connector.
--
-- 'scaleInPolicy', 'autoScalingUpdate_scaleInPolicy' - The target sacle-in policy for the connector.
--
-- 'scaleOutPolicy', 'autoScalingUpdate_scaleOutPolicy' - The target sacle-out policy for the connector.
newAutoScalingUpdate ::
  -- | 'maxWorkerCount'
  Prelude.Natural ->
  -- | 'mcuCount'
  Prelude.Natural ->
  -- | 'minWorkerCount'
  Prelude.Natural ->
  -- | 'scaleInPolicy'
  ScaleInPolicyUpdate ->
  -- | 'scaleOutPolicy'
  ScaleOutPolicyUpdate ->
  AutoScalingUpdate
newAutoScalingUpdate
  pMaxWorkerCount_
  pMcuCount_
  pMinWorkerCount_
  pScaleInPolicy_
  pScaleOutPolicy_ =
    AutoScalingUpdate'
      { maxWorkerCount =
          pMaxWorkerCount_,
        mcuCount = pMcuCount_,
        minWorkerCount = pMinWorkerCount_,
        scaleInPolicy = pScaleInPolicy_,
        scaleOutPolicy = pScaleOutPolicy_
      }

-- | The target maximum number of workers allocated to the connector.
autoScalingUpdate_maxWorkerCount :: Lens.Lens' AutoScalingUpdate Prelude.Natural
autoScalingUpdate_maxWorkerCount = Lens.lens (\AutoScalingUpdate' {maxWorkerCount} -> maxWorkerCount) (\s@AutoScalingUpdate' {} a -> s {maxWorkerCount = a} :: AutoScalingUpdate)

-- | The target number of microcontroller units (MCUs) allocated to each
-- connector worker. The valid values are 1,2,4,8.
autoScalingUpdate_mcuCount :: Lens.Lens' AutoScalingUpdate Prelude.Natural
autoScalingUpdate_mcuCount = Lens.lens (\AutoScalingUpdate' {mcuCount} -> mcuCount) (\s@AutoScalingUpdate' {} a -> s {mcuCount = a} :: AutoScalingUpdate)

-- | The target minimum number of workers allocated to the connector.
autoScalingUpdate_minWorkerCount :: Lens.Lens' AutoScalingUpdate Prelude.Natural
autoScalingUpdate_minWorkerCount = Lens.lens (\AutoScalingUpdate' {minWorkerCount} -> minWorkerCount) (\s@AutoScalingUpdate' {} a -> s {minWorkerCount = a} :: AutoScalingUpdate)

-- | The target sacle-in policy for the connector.
autoScalingUpdate_scaleInPolicy :: Lens.Lens' AutoScalingUpdate ScaleInPolicyUpdate
autoScalingUpdate_scaleInPolicy = Lens.lens (\AutoScalingUpdate' {scaleInPolicy} -> scaleInPolicy) (\s@AutoScalingUpdate' {} a -> s {scaleInPolicy = a} :: AutoScalingUpdate)

-- | The target sacle-out policy for the connector.
autoScalingUpdate_scaleOutPolicy :: Lens.Lens' AutoScalingUpdate ScaleOutPolicyUpdate
autoScalingUpdate_scaleOutPolicy = Lens.lens (\AutoScalingUpdate' {scaleOutPolicy} -> scaleOutPolicy) (\s@AutoScalingUpdate' {} a -> s {scaleOutPolicy = a} :: AutoScalingUpdate)

instance Prelude.Hashable AutoScalingUpdate where
  hashWithSalt _salt AutoScalingUpdate' {..} =
    _salt `Prelude.hashWithSalt` maxWorkerCount
      `Prelude.hashWithSalt` mcuCount
      `Prelude.hashWithSalt` minWorkerCount
      `Prelude.hashWithSalt` scaleInPolicy
      `Prelude.hashWithSalt` scaleOutPolicy

instance Prelude.NFData AutoScalingUpdate where
  rnf AutoScalingUpdate' {..} =
    Prelude.rnf maxWorkerCount
      `Prelude.seq` Prelude.rnf mcuCount
      `Prelude.seq` Prelude.rnf minWorkerCount
      `Prelude.seq` Prelude.rnf scaleInPolicy
      `Prelude.seq` Prelude.rnf scaleOutPolicy

instance Core.ToJSON AutoScalingUpdate where
  toJSON AutoScalingUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("maxWorkerCount" Core..= maxWorkerCount),
            Prelude.Just ("mcuCount" Core..= mcuCount),
            Prelude.Just
              ("minWorkerCount" Core..= minWorkerCount),
            Prelude.Just ("scaleInPolicy" Core..= scaleInPolicy),
            Prelude.Just
              ("scaleOutPolicy" Core..= scaleOutPolicy)
          ]
      )
