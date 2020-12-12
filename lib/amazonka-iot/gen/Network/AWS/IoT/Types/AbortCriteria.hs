{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AbortCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AbortCriteria
  ( AbortCriteria (..),

    -- * Smart constructor
    mkAbortCriteria,

    -- * Lenses
    acFailureType,
    acAction,
    acThresholdPercentage,
    acMinNumberOfExecutedThings,
  )
where

import Network.AWS.IoT.Types.AbortAction
import Network.AWS.IoT.Types.JobExecutionFailureType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The criteria that determine when and how a job abort takes place.
--
-- /See:/ 'mkAbortCriteria' smart constructor.
data AbortCriteria = AbortCriteria'
  { failureType ::
      JobExecutionFailureType,
    action :: AbortAction,
    thresholdPercentage :: Lude.Double,
    minNumberOfExecutedThings :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AbortCriteria' with the minimum fields required to make a request.
--
-- * 'action' - The type of job action to take to initiate the job abort.
-- * 'failureType' - The type of job execution failures that can initiate a job abort.
-- * 'minNumberOfExecutedThings' - The minimum number of things which must receive job execution notifications before the job can be aborted.
-- * 'thresholdPercentage' - The minimum percentage of job execution failures that must occur to initiate the job abort.
--
-- AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
mkAbortCriteria ::
  -- | 'failureType'
  JobExecutionFailureType ->
  -- | 'action'
  AbortAction ->
  -- | 'thresholdPercentage'
  Lude.Double ->
  -- | 'minNumberOfExecutedThings'
  Lude.Natural ->
  AbortCriteria
mkAbortCriteria
  pFailureType_
  pAction_
  pThresholdPercentage_
  pMinNumberOfExecutedThings_ =
    AbortCriteria'
      { failureType = pFailureType_,
        action = pAction_,
        thresholdPercentage = pThresholdPercentage_,
        minNumberOfExecutedThings = pMinNumberOfExecutedThings_
      }

-- | The type of job execution failures that can initiate a job abort.
--
-- /Note:/ Consider using 'failureType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acFailureType :: Lens.Lens' AbortCriteria JobExecutionFailureType
acFailureType = Lens.lens (failureType :: AbortCriteria -> JobExecutionFailureType) (\s a -> s {failureType = a} :: AbortCriteria)
{-# DEPRECATED acFailureType "Use generic-lens or generic-optics with 'failureType' instead." #-}

-- | The type of job action to take to initiate the job abort.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAction :: Lens.Lens' AbortCriteria AbortAction
acAction = Lens.lens (action :: AbortCriteria -> AbortAction) (\s a -> s {action = a} :: AbortCriteria)
{-# DEPRECATED acAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The minimum percentage of job execution failures that must occur to initiate the job abort.
--
-- AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
--
-- /Note:/ Consider using 'thresholdPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acThresholdPercentage :: Lens.Lens' AbortCriteria Lude.Double
acThresholdPercentage = Lens.lens (thresholdPercentage :: AbortCriteria -> Lude.Double) (\s a -> s {thresholdPercentage = a} :: AbortCriteria)
{-# DEPRECATED acThresholdPercentage "Use generic-lens or generic-optics with 'thresholdPercentage' instead." #-}

-- | The minimum number of things which must receive job execution notifications before the job can be aborted.
--
-- /Note:/ Consider using 'minNumberOfExecutedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acMinNumberOfExecutedThings :: Lens.Lens' AbortCriteria Lude.Natural
acMinNumberOfExecutedThings = Lens.lens (minNumberOfExecutedThings :: AbortCriteria -> Lude.Natural) (\s a -> s {minNumberOfExecutedThings = a} :: AbortCriteria)
{-# DEPRECATED acMinNumberOfExecutedThings "Use generic-lens or generic-optics with 'minNumberOfExecutedThings' instead." #-}

instance Lude.FromJSON AbortCriteria where
  parseJSON =
    Lude.withObject
      "AbortCriteria"
      ( \x ->
          AbortCriteria'
            Lude.<$> (x Lude..: "failureType")
            Lude.<*> (x Lude..: "action")
            Lude.<*> (x Lude..: "thresholdPercentage")
            Lude.<*> (x Lude..: "minNumberOfExecutedThings")
      )

instance Lude.ToJSON AbortCriteria where
  toJSON AbortCriteria' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("failureType" Lude..= failureType),
            Lude.Just ("action" Lude..= action),
            Lude.Just ("thresholdPercentage" Lude..= thresholdPercentage),
            Lude.Just
              ("minNumberOfExecutedThings" Lude..= minNumberOfExecutedThings)
          ]
      )
