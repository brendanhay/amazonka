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
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScalableTargetAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ScalableTargetAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the minimum and maximum capacity for a scheduled action.
--
-- /See:/ 'newScalableTargetAction' smart constructor.
data ScalableTargetAction = ScalableTargetAction'
  { -- | The maximum capacity.
    --
    -- Although you can specify a large maximum capacity, note that service
    -- quotas may impose lower limits. Each service has its own default quotas
    -- for the maximum capacity of the resource. If you want to specify a
    -- higher limit, you can request an increase. For more information, consult
    -- the documentation for that service. For information about the default
    -- quotas for each service, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-service-information.html Service Endpoints and Quotas>
    -- in the /Amazon Web Services General Reference/.
    maxCapacity :: Core.Maybe Core.Int,
    -- | The minimum capacity.
    --
    -- For certain resources, the minimum value allowed is 0. This includes
    -- Lambda provisioned concurrency, Spot Fleet, ECS services, Aurora DB
    -- clusters, EMR clusters, and custom resources. For all other resources,
    -- the minimum value allowed is 1.
    minCapacity :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScalableTargetAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxCapacity', 'scalableTargetAction_maxCapacity' - The maximum capacity.
--
-- Although you can specify a large maximum capacity, note that service
-- quotas may impose lower limits. Each service has its own default quotas
-- for the maximum capacity of the resource. If you want to specify a
-- higher limit, you can request an increase. For more information, consult
-- the documentation for that service. For information about the default
-- quotas for each service, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-service-information.html Service Endpoints and Quotas>
-- in the /Amazon Web Services General Reference/.
--
-- 'minCapacity', 'scalableTargetAction_minCapacity' - The minimum capacity.
--
-- For certain resources, the minimum value allowed is 0. This includes
-- Lambda provisioned concurrency, Spot Fleet, ECS services, Aurora DB
-- clusters, EMR clusters, and custom resources. For all other resources,
-- the minimum value allowed is 1.
newScalableTargetAction ::
  ScalableTargetAction
newScalableTargetAction =
  ScalableTargetAction'
    { maxCapacity = Core.Nothing,
      minCapacity = Core.Nothing
    }

-- | The maximum capacity.
--
-- Although you can specify a large maximum capacity, note that service
-- quotas may impose lower limits. Each service has its own default quotas
-- for the maximum capacity of the resource. If you want to specify a
-- higher limit, you can request an increase. For more information, consult
-- the documentation for that service. For information about the default
-- quotas for each service, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-service-information.html Service Endpoints and Quotas>
-- in the /Amazon Web Services General Reference/.
scalableTargetAction_maxCapacity :: Lens.Lens' ScalableTargetAction (Core.Maybe Core.Int)
scalableTargetAction_maxCapacity = Lens.lens (\ScalableTargetAction' {maxCapacity} -> maxCapacity) (\s@ScalableTargetAction' {} a -> s {maxCapacity = a} :: ScalableTargetAction)

-- | The minimum capacity.
--
-- For certain resources, the minimum value allowed is 0. This includes
-- Lambda provisioned concurrency, Spot Fleet, ECS services, Aurora DB
-- clusters, EMR clusters, and custom resources. For all other resources,
-- the minimum value allowed is 1.
scalableTargetAction_minCapacity :: Lens.Lens' ScalableTargetAction (Core.Maybe Core.Int)
scalableTargetAction_minCapacity = Lens.lens (\ScalableTargetAction' {minCapacity} -> minCapacity) (\s@ScalableTargetAction' {} a -> s {minCapacity = a} :: ScalableTargetAction)

instance Core.FromJSON ScalableTargetAction where
  parseJSON =
    Core.withObject
      "ScalableTargetAction"
      ( \x ->
          ScalableTargetAction'
            Core.<$> (x Core..:? "MaxCapacity")
            Core.<*> (x Core..:? "MinCapacity")
      )

instance Core.Hashable ScalableTargetAction

instance Core.NFData ScalableTargetAction

instance Core.ToJSON ScalableTargetAction where
  toJSON ScalableTargetAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxCapacity" Core..=) Core.<$> maxCapacity,
            ("MinCapacity" Core..=) Core.<$> minCapacity
          ]
      )
