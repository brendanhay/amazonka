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
-- Module      : Amazonka.CodeDeploy.Types.TrafficRoutingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.TrafficRoutingConfig where

import Amazonka.CodeDeploy.Types.TimeBasedCanary
import Amazonka.CodeDeploy.Types.TimeBasedLinear
import Amazonka.CodeDeploy.Types.TrafficRoutingType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration that specifies how traffic is shifted from one version
-- of a Lambda function to another version during an Lambda deployment, or
-- from one Amazon ECS task set to another during an Amazon ECS deployment.
--
-- /See:/ 'newTrafficRoutingConfig' smart constructor.
data TrafficRoutingConfig = TrafficRoutingConfig'
  { -- | A configuration that shifts traffic from one version of a Lambda
    -- function or ECS task set to another in two increments. The original and
    -- target Lambda function versions or ECS task sets are specified in the
    -- deployment\'s AppSpec file.
    timeBasedCanary :: Prelude.Maybe TimeBasedCanary,
    -- | A configuration that shifts traffic from one version of a Lambda
    -- function or Amazon ECS task set to another in equal increments, with an
    -- equal number of minutes between each increment. The original and target
    -- Lambda function versions or Amazon ECS task sets are specified in the
    -- deployment\'s AppSpec file.
    timeBasedLinear :: Prelude.Maybe TimeBasedLinear,
    -- | The type of traffic shifting (@TimeBasedCanary@ or @TimeBasedLinear@)
    -- used by a deployment configuration.
    type' :: Prelude.Maybe TrafficRoutingType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficRoutingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeBasedCanary', 'trafficRoutingConfig_timeBasedCanary' - A configuration that shifts traffic from one version of a Lambda
-- function or ECS task set to another in two increments. The original and
-- target Lambda function versions or ECS task sets are specified in the
-- deployment\'s AppSpec file.
--
-- 'timeBasedLinear', 'trafficRoutingConfig_timeBasedLinear' - A configuration that shifts traffic from one version of a Lambda
-- function or Amazon ECS task set to another in equal increments, with an
-- equal number of minutes between each increment. The original and target
-- Lambda function versions or Amazon ECS task sets are specified in the
-- deployment\'s AppSpec file.
--
-- 'type'', 'trafficRoutingConfig_type' - The type of traffic shifting (@TimeBasedCanary@ or @TimeBasedLinear@)
-- used by a deployment configuration.
newTrafficRoutingConfig ::
  TrafficRoutingConfig
newTrafficRoutingConfig =
  TrafficRoutingConfig'
    { timeBasedCanary =
        Prelude.Nothing,
      timeBasedLinear = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A configuration that shifts traffic from one version of a Lambda
-- function or ECS task set to another in two increments. The original and
-- target Lambda function versions or ECS task sets are specified in the
-- deployment\'s AppSpec file.
trafficRoutingConfig_timeBasedCanary :: Lens.Lens' TrafficRoutingConfig (Prelude.Maybe TimeBasedCanary)
trafficRoutingConfig_timeBasedCanary = Lens.lens (\TrafficRoutingConfig' {timeBasedCanary} -> timeBasedCanary) (\s@TrafficRoutingConfig' {} a -> s {timeBasedCanary = a} :: TrafficRoutingConfig)

-- | A configuration that shifts traffic from one version of a Lambda
-- function or Amazon ECS task set to another in equal increments, with an
-- equal number of minutes between each increment. The original and target
-- Lambda function versions or Amazon ECS task sets are specified in the
-- deployment\'s AppSpec file.
trafficRoutingConfig_timeBasedLinear :: Lens.Lens' TrafficRoutingConfig (Prelude.Maybe TimeBasedLinear)
trafficRoutingConfig_timeBasedLinear = Lens.lens (\TrafficRoutingConfig' {timeBasedLinear} -> timeBasedLinear) (\s@TrafficRoutingConfig' {} a -> s {timeBasedLinear = a} :: TrafficRoutingConfig)

-- | The type of traffic shifting (@TimeBasedCanary@ or @TimeBasedLinear@)
-- used by a deployment configuration.
trafficRoutingConfig_type :: Lens.Lens' TrafficRoutingConfig (Prelude.Maybe TrafficRoutingType)
trafficRoutingConfig_type = Lens.lens (\TrafficRoutingConfig' {type'} -> type') (\s@TrafficRoutingConfig' {} a -> s {type' = a} :: TrafficRoutingConfig)

instance Data.FromJSON TrafficRoutingConfig where
  parseJSON =
    Data.withObject
      "TrafficRoutingConfig"
      ( \x ->
          TrafficRoutingConfig'
            Prelude.<$> (x Data..:? "timeBasedCanary")
            Prelude.<*> (x Data..:? "timeBasedLinear")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable TrafficRoutingConfig where
  hashWithSalt _salt TrafficRoutingConfig' {..} =
    _salt
      `Prelude.hashWithSalt` timeBasedCanary
      `Prelude.hashWithSalt` timeBasedLinear
      `Prelude.hashWithSalt` type'

instance Prelude.NFData TrafficRoutingConfig where
  rnf TrafficRoutingConfig' {..} =
    Prelude.rnf timeBasedCanary
      `Prelude.seq` Prelude.rnf timeBasedLinear
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON TrafficRoutingConfig where
  toJSON TrafficRoutingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("timeBasedCanary" Data..=)
              Prelude.<$> timeBasedCanary,
            ("timeBasedLinear" Data..=)
              Prelude.<$> timeBasedLinear,
            ("type" Data..=) Prelude.<$> type'
          ]
      )
