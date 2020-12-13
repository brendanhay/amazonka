{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.InstanceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.InstanceDefinition
  ( InstanceDefinition (..),

    -- * Smart constructor
    mkInstanceDefinition,

    -- * Lenses
    idWeightedCapacity,
    idInstanceType,
  )
where

import Network.AWS.GameLift.Types.GameServerGroupInstanceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__
--
-- An allowed instance type for a 'GameServerGroup' . All game server groups must have at least two instance types defined for it. GameLift FleetIQ periodically evaluates each defined instance type for viability. It then updates the Auto Scaling group with the list of viable instance types.
--
-- /See:/ 'mkInstanceDefinition' smart constructor.
data InstanceDefinition = InstanceDefinition'
  { -- | Instance weighting that indicates how much this instance type contributes to the total capacity of a game server group. Instance weights are used by GameLift FleetIQ to calculate the instance type's cost per unit hour and better identify the most cost-effective options. For detailed information on weighting instance capacity, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting> in the /Amazon EC2 Auto Scaling User Guide/ . Default value is "1".
    weightedCapacity :: Lude.Maybe Lude.Text,
    -- | An EC2 instance type designation.
    instanceType :: GameServerGroupInstanceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceDefinition' with the minimum fields required to make a request.
--
-- * 'weightedCapacity' - Instance weighting that indicates how much this instance type contributes to the total capacity of a game server group. Instance weights are used by GameLift FleetIQ to calculate the instance type's cost per unit hour and better identify the most cost-effective options. For detailed information on weighting instance capacity, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting> in the /Amazon EC2 Auto Scaling User Guide/ . Default value is "1".
-- * 'instanceType' - An EC2 instance type designation.
mkInstanceDefinition ::
  -- | 'instanceType'
  GameServerGroupInstanceType ->
  InstanceDefinition
mkInstanceDefinition pInstanceType_ =
  InstanceDefinition'
    { weightedCapacity = Lude.Nothing,
      instanceType = pInstanceType_
    }

-- | Instance weighting that indicates how much this instance type contributes to the total capacity of a game server group. Instance weights are used by GameLift FleetIQ to calculate the instance type's cost per unit hour and better identify the most cost-effective options. For detailed information on weighting instance capacity, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting> in the /Amazon EC2 Auto Scaling User Guide/ . Default value is "1".
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idWeightedCapacity :: Lens.Lens' InstanceDefinition (Lude.Maybe Lude.Text)
idWeightedCapacity = Lens.lens (weightedCapacity :: InstanceDefinition -> Lude.Maybe Lude.Text) (\s a -> s {weightedCapacity = a} :: InstanceDefinition)
{-# DEPRECATED idWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}

-- | An EC2 instance type designation.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInstanceType :: Lens.Lens' InstanceDefinition GameServerGroupInstanceType
idInstanceType = Lens.lens (instanceType :: InstanceDefinition -> GameServerGroupInstanceType) (\s a -> s {instanceType = a} :: InstanceDefinition)
{-# DEPRECATED idInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

instance Lude.FromJSON InstanceDefinition where
  parseJSON =
    Lude.withObject
      "InstanceDefinition"
      ( \x ->
          InstanceDefinition'
            Lude.<$> (x Lude..:? "WeightedCapacity")
            Lude.<*> (x Lude..: "InstanceType")
      )

instance Lude.ToJSON InstanceDefinition where
  toJSON InstanceDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("WeightedCapacity" Lude..=) Lude.<$> weightedCapacity,
            Lude.Just ("InstanceType" Lude..= instanceType)
          ]
      )
