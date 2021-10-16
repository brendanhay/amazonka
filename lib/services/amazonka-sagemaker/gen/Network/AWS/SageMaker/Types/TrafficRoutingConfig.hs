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
-- Module      : Network.AWS.SageMaker.Types.TrafficRoutingConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrafficRoutingConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.CapacitySize
import Network.AWS.SageMaker.Types.TrafficRoutingConfigType

-- | Currently, the @TrafficRoutingConfig@ API is not supported.
--
-- /See:/ 'newTrafficRoutingConfig' smart constructor.
data TrafficRoutingConfig = TrafficRoutingConfig'
  { canarySize :: Prelude.Maybe CapacitySize,
    type' :: TrafficRoutingConfigType,
    waitIntervalInSeconds :: Prelude.Natural
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
-- 'canarySize', 'trafficRoutingConfig_canarySize' -
--
-- 'type'', 'trafficRoutingConfig_type' -
--
-- 'waitIntervalInSeconds', 'trafficRoutingConfig_waitIntervalInSeconds' -
newTrafficRoutingConfig ::
  -- | 'type''
  TrafficRoutingConfigType ->
  -- | 'waitIntervalInSeconds'
  Prelude.Natural ->
  TrafficRoutingConfig
newTrafficRoutingConfig
  pType_
  pWaitIntervalInSeconds_ =
    TrafficRoutingConfig'
      { canarySize = Prelude.Nothing,
        type' = pType_,
        waitIntervalInSeconds = pWaitIntervalInSeconds_
      }

-- |
trafficRoutingConfig_canarySize :: Lens.Lens' TrafficRoutingConfig (Prelude.Maybe CapacitySize)
trafficRoutingConfig_canarySize = Lens.lens (\TrafficRoutingConfig' {canarySize} -> canarySize) (\s@TrafficRoutingConfig' {} a -> s {canarySize = a} :: TrafficRoutingConfig)

-- |
trafficRoutingConfig_type :: Lens.Lens' TrafficRoutingConfig TrafficRoutingConfigType
trafficRoutingConfig_type = Lens.lens (\TrafficRoutingConfig' {type'} -> type') (\s@TrafficRoutingConfig' {} a -> s {type' = a} :: TrafficRoutingConfig)

-- |
trafficRoutingConfig_waitIntervalInSeconds :: Lens.Lens' TrafficRoutingConfig Prelude.Natural
trafficRoutingConfig_waitIntervalInSeconds = Lens.lens (\TrafficRoutingConfig' {waitIntervalInSeconds} -> waitIntervalInSeconds) (\s@TrafficRoutingConfig' {} a -> s {waitIntervalInSeconds = a} :: TrafficRoutingConfig)

instance Core.FromJSON TrafficRoutingConfig where
  parseJSON =
    Core.withObject
      "TrafficRoutingConfig"
      ( \x ->
          TrafficRoutingConfig'
            Prelude.<$> (x Core..:? "CanarySize")
            Prelude.<*> (x Core..: "Type")
            Prelude.<*> (x Core..: "WaitIntervalInSeconds")
      )

instance Prelude.Hashable TrafficRoutingConfig

instance Prelude.NFData TrafficRoutingConfig

instance Core.ToJSON TrafficRoutingConfig where
  toJSON TrafficRoutingConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CanarySize" Core..=) Prelude.<$> canarySize,
            Prelude.Just ("Type" Core..= type'),
            Prelude.Just
              ( "WaitIntervalInSeconds"
                  Core..= waitIntervalInSeconds
              )
          ]
      )
