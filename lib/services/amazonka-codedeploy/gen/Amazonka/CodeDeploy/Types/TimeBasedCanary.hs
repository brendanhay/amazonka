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
-- Module      : Amazonka.CodeDeploy.Types.TimeBasedCanary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.TimeBasedCanary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A configuration that shifts traffic from one version of a Lambda
-- function or Amazon ECS task set to another in two increments. The
-- original and target Lambda function versions or ECS task sets are
-- specified in the deployment\'s AppSpec file.
--
-- /See:/ 'newTimeBasedCanary' smart constructor.
data TimeBasedCanary = TimeBasedCanary'
  { -- | The percentage of traffic to shift in the first increment of a
    -- @TimeBasedCanary@ deployment.
    canaryPercentage :: Prelude.Maybe Prelude.Int,
    -- | The number of minutes between the first and second traffic shifts of a
    -- @TimeBasedCanary@ deployment.
    canaryInterval :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeBasedCanary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'canaryPercentage', 'timeBasedCanary_canaryPercentage' - The percentage of traffic to shift in the first increment of a
-- @TimeBasedCanary@ deployment.
--
-- 'canaryInterval', 'timeBasedCanary_canaryInterval' - The number of minutes between the first and second traffic shifts of a
-- @TimeBasedCanary@ deployment.
newTimeBasedCanary ::
  TimeBasedCanary
newTimeBasedCanary =
  TimeBasedCanary'
    { canaryPercentage =
        Prelude.Nothing,
      canaryInterval = Prelude.Nothing
    }

-- | The percentage of traffic to shift in the first increment of a
-- @TimeBasedCanary@ deployment.
timeBasedCanary_canaryPercentage :: Lens.Lens' TimeBasedCanary (Prelude.Maybe Prelude.Int)
timeBasedCanary_canaryPercentage = Lens.lens (\TimeBasedCanary' {canaryPercentage} -> canaryPercentage) (\s@TimeBasedCanary' {} a -> s {canaryPercentage = a} :: TimeBasedCanary)

-- | The number of minutes between the first and second traffic shifts of a
-- @TimeBasedCanary@ deployment.
timeBasedCanary_canaryInterval :: Lens.Lens' TimeBasedCanary (Prelude.Maybe Prelude.Int)
timeBasedCanary_canaryInterval = Lens.lens (\TimeBasedCanary' {canaryInterval} -> canaryInterval) (\s@TimeBasedCanary' {} a -> s {canaryInterval = a} :: TimeBasedCanary)

instance Core.FromJSON TimeBasedCanary where
  parseJSON =
    Core.withObject
      "TimeBasedCanary"
      ( \x ->
          TimeBasedCanary'
            Prelude.<$> (x Core..:? "canaryPercentage")
            Prelude.<*> (x Core..:? "canaryInterval")
      )

instance Prelude.Hashable TimeBasedCanary where
  hashWithSalt _salt TimeBasedCanary' {..} =
    _salt `Prelude.hashWithSalt` canaryPercentage
      `Prelude.hashWithSalt` canaryInterval

instance Prelude.NFData TimeBasedCanary where
  rnf TimeBasedCanary' {..} =
    Prelude.rnf canaryPercentage
      `Prelude.seq` Prelude.rnf canaryInterval

instance Core.ToJSON TimeBasedCanary where
  toJSON TimeBasedCanary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("canaryPercentage" Core..=)
              Prelude.<$> canaryPercentage,
            ("canaryInterval" Core..=)
              Prelude.<$> canaryInterval
          ]
      )
