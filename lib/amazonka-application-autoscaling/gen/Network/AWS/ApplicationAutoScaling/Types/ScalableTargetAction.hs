{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScalableTargetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ScalableTargetAction
  ( ScalableTargetAction (..),

    -- * Smart constructor
    mkScalableTargetAction,

    -- * Lenses
    staMaxCapacity,
    staMinCapacity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the minimum and maximum capacity for a scheduled action.
--
-- /See:/ 'mkScalableTargetAction' smart constructor.
data ScalableTargetAction = ScalableTargetAction'
  { -- | The maximum capacity.
    --
    -- Although you can specify a large maximum capacity, note that service quotas may impose lower limits. Each service has its own default quotas for the maximum capacity of the resource. If you want to specify a higher limit, you can request an increase. For more information, consult the documentation for that service. For information about the default quotas for each service, see <https://docs.aws.amazon.com/general/latest/gr/aws-service-information.html Service Endpoints and Quotas> in the /Amazon Web Services General Reference/ .
    maxCapacity :: Lude.Maybe Lude.Int,
    -- | The minimum capacity.
    --
    -- For certain resources, the minimum value allowed is 0. This includes Lambda provisioned concurrency, Spot Fleet, ECS services, Aurora DB clusters, EMR clusters, and custom resources. For all other resources, the minimum value allowed is 1.
    minCapacity :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalableTargetAction' with the minimum fields required to make a request.
--
-- * 'maxCapacity' - The maximum capacity.
--
-- Although you can specify a large maximum capacity, note that service quotas may impose lower limits. Each service has its own default quotas for the maximum capacity of the resource. If you want to specify a higher limit, you can request an increase. For more information, consult the documentation for that service. For information about the default quotas for each service, see <https://docs.aws.amazon.com/general/latest/gr/aws-service-information.html Service Endpoints and Quotas> in the /Amazon Web Services General Reference/ .
-- * 'minCapacity' - The minimum capacity.
--
-- For certain resources, the minimum value allowed is 0. This includes Lambda provisioned concurrency, Spot Fleet, ECS services, Aurora DB clusters, EMR clusters, and custom resources. For all other resources, the minimum value allowed is 1.
mkScalableTargetAction ::
  ScalableTargetAction
mkScalableTargetAction =
  ScalableTargetAction'
    { maxCapacity = Lude.Nothing,
      minCapacity = Lude.Nothing
    }

-- | The maximum capacity.
--
-- Although you can specify a large maximum capacity, note that service quotas may impose lower limits. Each service has its own default quotas for the maximum capacity of the resource. If you want to specify a higher limit, you can request an increase. For more information, consult the documentation for that service. For information about the default quotas for each service, see <https://docs.aws.amazon.com/general/latest/gr/aws-service-information.html Service Endpoints and Quotas> in the /Amazon Web Services General Reference/ .
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staMaxCapacity :: Lens.Lens' ScalableTargetAction (Lude.Maybe Lude.Int)
staMaxCapacity = Lens.lens (maxCapacity :: ScalableTargetAction -> Lude.Maybe Lude.Int) (\s a -> s {maxCapacity = a} :: ScalableTargetAction)
{-# DEPRECATED staMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The minimum capacity.
--
-- For certain resources, the minimum value allowed is 0. This includes Lambda provisioned concurrency, Spot Fleet, ECS services, Aurora DB clusters, EMR clusters, and custom resources. For all other resources, the minimum value allowed is 1.
--
-- /Note:/ Consider using 'minCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staMinCapacity :: Lens.Lens' ScalableTargetAction (Lude.Maybe Lude.Int)
staMinCapacity = Lens.lens (minCapacity :: ScalableTargetAction -> Lude.Maybe Lude.Int) (\s a -> s {minCapacity = a} :: ScalableTargetAction)
{-# DEPRECATED staMinCapacity "Use generic-lens or generic-optics with 'minCapacity' instead." #-}

instance Lude.FromJSON ScalableTargetAction where
  parseJSON =
    Lude.withObject
      "ScalableTargetAction"
      ( \x ->
          ScalableTargetAction'
            Lude.<$> (x Lude..:? "MaxCapacity") Lude.<*> (x Lude..:? "MinCapacity")
      )

instance Lude.ToJSON ScalableTargetAction where
  toJSON ScalableTargetAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaxCapacity" Lude..=) Lude.<$> maxCapacity,
            ("MinCapacity" Lude..=) Lude.<$> minCapacity
          ]
      )
