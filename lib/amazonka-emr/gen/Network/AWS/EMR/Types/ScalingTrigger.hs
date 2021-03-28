{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ScalingTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ScalingTrigger
  ( ScalingTrigger (..)
  -- * Smart constructor
  , mkScalingTrigger
  -- * Lenses
  , stCloudWatchAlarmDefinition
  ) where

import qualified Network.AWS.EMR.Types.CloudWatchAlarmDefinition as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The conditions that trigger an automatic scaling activity.
--
-- /See:/ 'mkScalingTrigger' smart constructor.
newtype ScalingTrigger = ScalingTrigger'
  { cloudWatchAlarmDefinition :: Types.CloudWatchAlarmDefinition
    -- ^ The definition of a CloudWatch metric alarm. When the defined alarm conditions are met along with other trigger parameters, scaling activity begins.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ScalingTrigger' value with any optional fields omitted.
mkScalingTrigger
    :: Types.CloudWatchAlarmDefinition -- ^ 'cloudWatchAlarmDefinition'
    -> ScalingTrigger
mkScalingTrigger cloudWatchAlarmDefinition
  = ScalingTrigger'{cloudWatchAlarmDefinition}

-- | The definition of a CloudWatch metric alarm. When the defined alarm conditions are met along with other trigger parameters, scaling activity begins.
--
-- /Note:/ Consider using 'cloudWatchAlarmDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stCloudWatchAlarmDefinition :: Lens.Lens' ScalingTrigger Types.CloudWatchAlarmDefinition
stCloudWatchAlarmDefinition = Lens.field @"cloudWatchAlarmDefinition"
{-# INLINEABLE stCloudWatchAlarmDefinition #-}
{-# DEPRECATED cloudWatchAlarmDefinition "Use generic-lens or generic-optics with 'cloudWatchAlarmDefinition' instead"  #-}

instance Core.FromJSON ScalingTrigger where
        toJSON ScalingTrigger{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("CloudWatchAlarmDefinition" Core..= cloudWatchAlarmDefinition)])

instance Core.FromJSON ScalingTrigger where
        parseJSON
          = Core.withObject "ScalingTrigger" Core.$
              \ x ->
                ScalingTrigger' Core.<$> (x Core..: "CloudWatchAlarmDefinition")
