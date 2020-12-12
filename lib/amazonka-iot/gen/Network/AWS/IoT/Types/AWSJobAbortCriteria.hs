{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobAbortCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AWSJobAbortCriteria
  ( AWSJobAbortCriteria (..),

    -- * Smart constructor
    mkAWSJobAbortCriteria,

    -- * Lenses
    ajacFailureType,
    ajacAction,
    ajacThresholdPercentage,
    ajacMinNumberOfExecutedThings,
  )
where

import Network.AWS.IoT.Types.AWSJobAbortCriteriaAbortAction
import Network.AWS.IoT.Types.AWSJobAbortCriteriaFailureType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The criteria that determine when and how a job abort takes place.
--
-- /See:/ 'mkAWSJobAbortCriteria' smart constructor.
data AWSJobAbortCriteria = AWSJobAbortCriteria'
  { failureType ::
      AWSJobAbortCriteriaFailureType,
    action :: AWSJobAbortCriteriaAbortAction,
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

-- | Creates a value of 'AWSJobAbortCriteria' with the minimum fields required to make a request.
--
-- * 'action' - The type of job action to take to initiate the job abort.
-- * 'failureType' - The type of job execution failures that can initiate a job abort.
-- * 'minNumberOfExecutedThings' - The minimum number of things which must receive job execution notifications before the job can be aborted.
-- * 'thresholdPercentage' - The minimum percentage of job execution failures that must occur to initiate the job abort.
--
-- AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
mkAWSJobAbortCriteria ::
  -- | 'failureType'
  AWSJobAbortCriteriaFailureType ->
  -- | 'action'
  AWSJobAbortCriteriaAbortAction ->
  -- | 'thresholdPercentage'
  Lude.Double ->
  -- | 'minNumberOfExecutedThings'
  Lude.Natural ->
  AWSJobAbortCriteria
mkAWSJobAbortCriteria
  pFailureType_
  pAction_
  pThresholdPercentage_
  pMinNumberOfExecutedThings_ =
    AWSJobAbortCriteria'
      { failureType = pFailureType_,
        action = pAction_,
        thresholdPercentage = pThresholdPercentage_,
        minNumberOfExecutedThings = pMinNumberOfExecutedThings_
      }

-- | The type of job execution failures that can initiate a job abort.
--
-- /Note:/ Consider using 'failureType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajacFailureType :: Lens.Lens' AWSJobAbortCriteria AWSJobAbortCriteriaFailureType
ajacFailureType = Lens.lens (failureType :: AWSJobAbortCriteria -> AWSJobAbortCriteriaFailureType) (\s a -> s {failureType = a} :: AWSJobAbortCriteria)
{-# DEPRECATED ajacFailureType "Use generic-lens or generic-optics with 'failureType' instead." #-}

-- | The type of job action to take to initiate the job abort.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajacAction :: Lens.Lens' AWSJobAbortCriteria AWSJobAbortCriteriaAbortAction
ajacAction = Lens.lens (action :: AWSJobAbortCriteria -> AWSJobAbortCriteriaAbortAction) (\s a -> s {action = a} :: AWSJobAbortCriteria)
{-# DEPRECATED ajacAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The minimum percentage of job execution failures that must occur to initiate the job abort.
--
-- AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
--
-- /Note:/ Consider using 'thresholdPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajacThresholdPercentage :: Lens.Lens' AWSJobAbortCriteria Lude.Double
ajacThresholdPercentage = Lens.lens (thresholdPercentage :: AWSJobAbortCriteria -> Lude.Double) (\s a -> s {thresholdPercentage = a} :: AWSJobAbortCriteria)
{-# DEPRECATED ajacThresholdPercentage "Use generic-lens or generic-optics with 'thresholdPercentage' instead." #-}

-- | The minimum number of things which must receive job execution notifications before the job can be aborted.
--
-- /Note:/ Consider using 'minNumberOfExecutedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajacMinNumberOfExecutedThings :: Lens.Lens' AWSJobAbortCriteria Lude.Natural
ajacMinNumberOfExecutedThings = Lens.lens (minNumberOfExecutedThings :: AWSJobAbortCriteria -> Lude.Natural) (\s a -> s {minNumberOfExecutedThings = a} :: AWSJobAbortCriteria)
{-# DEPRECATED ajacMinNumberOfExecutedThings "Use generic-lens or generic-optics with 'minNumberOfExecutedThings' instead." #-}

instance Lude.ToJSON AWSJobAbortCriteria where
  toJSON AWSJobAbortCriteria' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("failureType" Lude..= failureType),
            Lude.Just ("action" Lude..= action),
            Lude.Just ("thresholdPercentage" Lude..= thresholdPercentage),
            Lude.Just
              ("minNumberOfExecutedThings" Lude..= minNumberOfExecutedThings)
          ]
      )
