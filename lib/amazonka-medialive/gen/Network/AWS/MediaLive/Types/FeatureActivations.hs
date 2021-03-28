{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FeatureActivations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.FeatureActivations
  ( FeatureActivations (..)
  -- * Smart constructor
  , mkFeatureActivations
  -- * Lenses
  , faInputPrepareScheduleActions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.FeatureActivationsInputPrepareScheduleActions as Types
import qualified Network.AWS.Prelude as Core

-- | Feature Activations
--
-- /See:/ 'mkFeatureActivations' smart constructor.
newtype FeatureActivations = FeatureActivations'
  { inputPrepareScheduleActions :: Core.Maybe Types.FeatureActivationsInputPrepareScheduleActions
    -- ^ Enables the Input Prepare feature. You can create Input Prepare actions in the schedule only if this feature is enabled.
--
-- If you disable the feature on an existing schedule, make sure that you first delete all input prepare actions from the schedule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FeatureActivations' value with any optional fields omitted.
mkFeatureActivations
    :: FeatureActivations
mkFeatureActivations
  = FeatureActivations'{inputPrepareScheduleActions = Core.Nothing}

-- | Enables the Input Prepare feature. You can create Input Prepare actions in the schedule only if this feature is enabled.
--
-- If you disable the feature on an existing schedule, make sure that you first delete all input prepare actions from the schedule.
--
-- /Note:/ Consider using 'inputPrepareScheduleActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faInputPrepareScheduleActions :: Lens.Lens' FeatureActivations (Core.Maybe Types.FeatureActivationsInputPrepareScheduleActions)
faInputPrepareScheduleActions = Lens.field @"inputPrepareScheduleActions"
{-# INLINEABLE faInputPrepareScheduleActions #-}
{-# DEPRECATED inputPrepareScheduleActions "Use generic-lens or generic-optics with 'inputPrepareScheduleActions' instead"  #-}

instance Core.FromJSON FeatureActivations where
        toJSON FeatureActivations{..}
          = Core.object
              (Core.catMaybes
                 [("inputPrepareScheduleActions" Core..=) Core.<$>
                    inputPrepareScheduleActions])

instance Core.FromJSON FeatureActivations where
        parseJSON
          = Core.withObject "FeatureActivations" Core.$
              \ x ->
                FeatureActivations' Core.<$>
                  (x Core..:? "inputPrepareScheduleActions")
