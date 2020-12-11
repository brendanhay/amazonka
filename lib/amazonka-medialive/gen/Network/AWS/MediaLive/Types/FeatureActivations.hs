-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FeatureActivations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FeatureActivations
  ( FeatureActivations (..),

    -- * Smart constructor
    mkFeatureActivations,

    -- * Lenses
    faInputPrepareScheduleActions,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FeatureActivationsInputPrepareScheduleActions
import qualified Network.AWS.Prelude as Lude

-- | Feature Activations
--
-- /See:/ 'mkFeatureActivations' smart constructor.
newtype FeatureActivations = FeatureActivations'
  { inputPrepareScheduleActions ::
      Lude.Maybe
        FeatureActivationsInputPrepareScheduleActions
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FeatureActivations' with the minimum fields required to make a request.
--
-- * 'inputPrepareScheduleActions' - Enables the Input Prepare feature. You can create Input Prepare actions in the schedule only if this feature is enabled.
--
-- If you disable the feature on an existing schedule, make sure that you first delete all input prepare actions from the schedule.
mkFeatureActivations ::
  FeatureActivations
mkFeatureActivations =
  FeatureActivations' {inputPrepareScheduleActions = Lude.Nothing}

-- | Enables the Input Prepare feature. You can create Input Prepare actions in the schedule only if this feature is enabled.
--
-- If you disable the feature on an existing schedule, make sure that you first delete all input prepare actions from the schedule.
--
-- /Note:/ Consider using 'inputPrepareScheduleActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faInputPrepareScheduleActions :: Lens.Lens' FeatureActivations (Lude.Maybe FeatureActivationsInputPrepareScheduleActions)
faInputPrepareScheduleActions = Lens.lens (inputPrepareScheduleActions :: FeatureActivations -> Lude.Maybe FeatureActivationsInputPrepareScheduleActions) (\s a -> s {inputPrepareScheduleActions = a} :: FeatureActivations)
{-# DEPRECATED faInputPrepareScheduleActions "Use generic-lens or generic-optics with 'inputPrepareScheduleActions' instead." #-}

instance Lude.FromJSON FeatureActivations where
  parseJSON =
    Lude.withObject
      "FeatureActivations"
      ( \x ->
          FeatureActivations'
            Lude.<$> (x Lude..:? "inputPrepareScheduleActions")
      )

instance Lude.ToJSON FeatureActivations where
  toJSON FeatureActivations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("inputPrepareScheduleActions" Lude..=)
              Lude.<$> inputPrepareScheduleActions
          ]
      )
