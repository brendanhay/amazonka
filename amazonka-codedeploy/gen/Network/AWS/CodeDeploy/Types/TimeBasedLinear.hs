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
-- Module      : Network.AWS.CodeDeploy.Types.TimeBasedLinear
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TimeBasedLinear where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A configuration that shifts traffic from one version of a Lambda
-- function or ECS task set to another in equal increments, with an equal
-- number of minutes between each increment. The original and target Lambda
-- function versions or ECS task sets are specified in the deployment\'s
-- AppSpec file.
--
-- /See:/ 'newTimeBasedLinear' smart constructor.
data TimeBasedLinear = TimeBasedLinear'
  { -- | The number of minutes between each incremental traffic shift of a
    -- @TimeBasedLinear@ deployment.
    linearInterval :: Core.Maybe Core.Int,
    -- | The percentage of traffic that is shifted at the start of each increment
    -- of a @TimeBasedLinear@ deployment.
    linearPercentage :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TimeBasedLinear' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linearInterval', 'timeBasedLinear_linearInterval' - The number of minutes between each incremental traffic shift of a
-- @TimeBasedLinear@ deployment.
--
-- 'linearPercentage', 'timeBasedLinear_linearPercentage' - The percentage of traffic that is shifted at the start of each increment
-- of a @TimeBasedLinear@ deployment.
newTimeBasedLinear ::
  TimeBasedLinear
newTimeBasedLinear =
  TimeBasedLinear'
    { linearInterval = Core.Nothing,
      linearPercentage = Core.Nothing
    }

-- | The number of minutes between each incremental traffic shift of a
-- @TimeBasedLinear@ deployment.
timeBasedLinear_linearInterval :: Lens.Lens' TimeBasedLinear (Core.Maybe Core.Int)
timeBasedLinear_linearInterval = Lens.lens (\TimeBasedLinear' {linearInterval} -> linearInterval) (\s@TimeBasedLinear' {} a -> s {linearInterval = a} :: TimeBasedLinear)

-- | The percentage of traffic that is shifted at the start of each increment
-- of a @TimeBasedLinear@ deployment.
timeBasedLinear_linearPercentage :: Lens.Lens' TimeBasedLinear (Core.Maybe Core.Int)
timeBasedLinear_linearPercentage = Lens.lens (\TimeBasedLinear' {linearPercentage} -> linearPercentage) (\s@TimeBasedLinear' {} a -> s {linearPercentage = a} :: TimeBasedLinear)

instance Core.FromJSON TimeBasedLinear where
  parseJSON =
    Core.withObject
      "TimeBasedLinear"
      ( \x ->
          TimeBasedLinear'
            Core.<$> (x Core..:? "linearInterval")
            Core.<*> (x Core..:? "linearPercentage")
      )

instance Core.Hashable TimeBasedLinear

instance Core.NFData TimeBasedLinear

instance Core.ToJSON TimeBasedLinear where
  toJSON TimeBasedLinear' {..} =
    Core.object
      ( Core.catMaybes
          [ ("linearInterval" Core..=) Core.<$> linearInterval,
            ("linearPercentage" Core..=)
              Core.<$> linearPercentage
          ]
      )
