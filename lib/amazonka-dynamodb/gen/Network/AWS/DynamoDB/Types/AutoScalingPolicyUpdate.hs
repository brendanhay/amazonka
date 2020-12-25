{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingPolicyUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingPolicyUpdate
  ( AutoScalingPolicyUpdate (..),

    -- * Smart constructor
    mkAutoScalingPolicyUpdate,

    -- * Lenses
    aspuTargetTrackingScalingPolicyConfiguration,
    aspuPolicyName,
  )
where

import qualified Network.AWS.DynamoDB.Types.AutoScalingPolicyName as Types
import qualified Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the auto scaling policy to be modified.
--
-- /See:/ 'mkAutoScalingPolicyUpdate' smart constructor.
data AutoScalingPolicyUpdate = AutoScalingPolicyUpdate'
  { -- | Represents a target tracking scaling policy configuration.
    targetTrackingScalingPolicyConfiguration :: Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate,
    -- | The name of the scaling policy.
    policyName :: Core.Maybe Types.AutoScalingPolicyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingPolicyUpdate' value with any optional fields omitted.
mkAutoScalingPolicyUpdate ::
  -- | 'targetTrackingScalingPolicyConfiguration'
  Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate ->
  AutoScalingPolicyUpdate
mkAutoScalingPolicyUpdate targetTrackingScalingPolicyConfiguration =
  AutoScalingPolicyUpdate'
    { targetTrackingScalingPolicyConfiguration,
      policyName = Core.Nothing
    }

-- | Represents a target tracking scaling policy configuration.
--
-- /Note:/ Consider using 'targetTrackingScalingPolicyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspuTargetTrackingScalingPolicyConfiguration :: Lens.Lens' AutoScalingPolicyUpdate Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
aspuTargetTrackingScalingPolicyConfiguration = Lens.field @"targetTrackingScalingPolicyConfiguration"
{-# DEPRECATED aspuTargetTrackingScalingPolicyConfiguration "Use generic-lens or generic-optics with 'targetTrackingScalingPolicyConfiguration' instead." #-}

-- | The name of the scaling policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspuPolicyName :: Lens.Lens' AutoScalingPolicyUpdate (Core.Maybe Types.AutoScalingPolicyName)
aspuPolicyName = Lens.field @"policyName"
{-# DEPRECATED aspuPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Core.FromJSON AutoScalingPolicyUpdate where
  toJSON AutoScalingPolicyUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "TargetTrackingScalingPolicyConfiguration"
                  Core..= targetTrackingScalingPolicyConfiguration
              ),
            ("PolicyName" Core..=) Core.<$> policyName
          ]
      )
