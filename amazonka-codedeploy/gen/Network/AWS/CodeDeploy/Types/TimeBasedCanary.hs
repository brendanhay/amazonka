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
-- Module      : Network.AWS.CodeDeploy.Types.TimeBasedCanary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TimeBasedCanary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A configuration that shifts traffic from one version of a Lambda
-- function or ECS task set to another in two increments. The original and
-- target Lambda function versions or ECS task sets are specified in the
-- deployment\'s AppSpec file.
--
-- /See:/ 'newTimeBasedCanary' smart constructor.
data TimeBasedCanary = TimeBasedCanary'
  { -- | The number of minutes between the first and second traffic shifts of a
    -- @TimeBasedCanary@ deployment.
    canaryInterval :: Core.Maybe Core.Int,
    -- | The percentage of traffic to shift in the first increment of a
    -- @TimeBasedCanary@ deployment.
    canaryPercentage :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TimeBasedCanary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'canaryInterval', 'timeBasedCanary_canaryInterval' - The number of minutes between the first and second traffic shifts of a
-- @TimeBasedCanary@ deployment.
--
-- 'canaryPercentage', 'timeBasedCanary_canaryPercentage' - The percentage of traffic to shift in the first increment of a
-- @TimeBasedCanary@ deployment.
newTimeBasedCanary ::
  TimeBasedCanary
newTimeBasedCanary =
  TimeBasedCanary'
    { canaryInterval = Core.Nothing,
      canaryPercentage = Core.Nothing
    }

-- | The number of minutes between the first and second traffic shifts of a
-- @TimeBasedCanary@ deployment.
timeBasedCanary_canaryInterval :: Lens.Lens' TimeBasedCanary (Core.Maybe Core.Int)
timeBasedCanary_canaryInterval = Lens.lens (\TimeBasedCanary' {canaryInterval} -> canaryInterval) (\s@TimeBasedCanary' {} a -> s {canaryInterval = a} :: TimeBasedCanary)

-- | The percentage of traffic to shift in the first increment of a
-- @TimeBasedCanary@ deployment.
timeBasedCanary_canaryPercentage :: Lens.Lens' TimeBasedCanary (Core.Maybe Core.Int)
timeBasedCanary_canaryPercentage = Lens.lens (\TimeBasedCanary' {canaryPercentage} -> canaryPercentage) (\s@TimeBasedCanary' {} a -> s {canaryPercentage = a} :: TimeBasedCanary)

instance Core.FromJSON TimeBasedCanary where
  parseJSON =
    Core.withObject
      "TimeBasedCanary"
      ( \x ->
          TimeBasedCanary'
            Core.<$> (x Core..:? "canaryInterval")
            Core.<*> (x Core..:? "canaryPercentage")
      )

instance Core.Hashable TimeBasedCanary

instance Core.NFData TimeBasedCanary

instance Core.ToJSON TimeBasedCanary where
  toJSON TimeBasedCanary' {..} =
    Core.object
      ( Core.catMaybes
          [ ("canaryInterval" Core..=) Core.<$> canaryInterval,
            ("canaryPercentage" Core..=)
              Core.<$> canaryPercentage
          ]
      )
