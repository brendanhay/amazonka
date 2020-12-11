-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TrafficRoutingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TrafficRoutingConfig
  ( TrafficRoutingConfig (..),

    -- * Smart constructor
    mkTrafficRoutingConfig,

    -- * Lenses
    trcTimeBasedCanary,
    trcTimeBasedLinear,
    trcType,
  )
where

import Network.AWS.CodeDeploy.Types.TimeBasedCanary
import Network.AWS.CodeDeploy.Types.TimeBasedLinear
import Network.AWS.CodeDeploy.Types.TrafficRoutingType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration that specifies how traffic is shifted from one version of a Lambda function to another version during an AWS Lambda deployment, or from one Amazon ECS task set to another during an Amazon ECS deployment.
--
-- /See:/ 'mkTrafficRoutingConfig' smart constructor.
data TrafficRoutingConfig = TrafficRoutingConfig'
  { timeBasedCanary ::
      Lude.Maybe TimeBasedCanary,
    timeBasedLinear :: Lude.Maybe TimeBasedLinear,
    type' :: Lude.Maybe TrafficRoutingType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrafficRoutingConfig' with the minimum fields required to make a request.
--
-- * 'timeBasedCanary' - A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in two increments. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
-- * 'timeBasedLinear' - A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
-- * 'type'' - The type of traffic shifting (@TimeBasedCanary@ or @TimeBasedLinear@ ) used by a deployment configuration.
mkTrafficRoutingConfig ::
  TrafficRoutingConfig
mkTrafficRoutingConfig =
  TrafficRoutingConfig'
    { timeBasedCanary = Lude.Nothing,
      timeBasedLinear = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in two increments. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
--
-- /Note:/ Consider using 'timeBasedCanary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trcTimeBasedCanary :: Lens.Lens' TrafficRoutingConfig (Lude.Maybe TimeBasedCanary)
trcTimeBasedCanary = Lens.lens (timeBasedCanary :: TrafficRoutingConfig -> Lude.Maybe TimeBasedCanary) (\s a -> s {timeBasedCanary = a} :: TrafficRoutingConfig)
{-# DEPRECATED trcTimeBasedCanary "Use generic-lens or generic-optics with 'timeBasedCanary' instead." #-}

-- | A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
--
-- /Note:/ Consider using 'timeBasedLinear' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trcTimeBasedLinear :: Lens.Lens' TrafficRoutingConfig (Lude.Maybe TimeBasedLinear)
trcTimeBasedLinear = Lens.lens (timeBasedLinear :: TrafficRoutingConfig -> Lude.Maybe TimeBasedLinear) (\s a -> s {timeBasedLinear = a} :: TrafficRoutingConfig)
{-# DEPRECATED trcTimeBasedLinear "Use generic-lens or generic-optics with 'timeBasedLinear' instead." #-}

-- | The type of traffic shifting (@TimeBasedCanary@ or @TimeBasedLinear@ ) used by a deployment configuration.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trcType :: Lens.Lens' TrafficRoutingConfig (Lude.Maybe TrafficRoutingType)
trcType = Lens.lens (type' :: TrafficRoutingConfig -> Lude.Maybe TrafficRoutingType) (\s a -> s {type' = a} :: TrafficRoutingConfig)
{-# DEPRECATED trcType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON TrafficRoutingConfig where
  parseJSON =
    Lude.withObject
      "TrafficRoutingConfig"
      ( \x ->
          TrafficRoutingConfig'
            Lude.<$> (x Lude..:? "timeBasedCanary")
            Lude.<*> (x Lude..:? "timeBasedLinear")
            Lude.<*> (x Lude..:? "type")
      )

instance Lude.ToJSON TrafficRoutingConfig where
  toJSON TrafficRoutingConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("timeBasedCanary" Lude..=) Lude.<$> timeBasedCanary,
            ("timeBasedLinear" Lude..=) Lude.<$> timeBasedLinear,
            ("type" Lude..=) Lude.<$> type'
          ]
      )
