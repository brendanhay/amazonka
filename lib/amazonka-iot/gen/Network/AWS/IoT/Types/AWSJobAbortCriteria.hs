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
    ajacThresholdPercentage,
    ajacFailureType,
    ajacAction,
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
  { -- | The minimum percentage of job execution failures that must occur to initiate the job abort.
    --
    -- AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
    thresholdPercentage :: Lude.Double,
    -- | The type of job execution failures that can initiate a job abort.
    failureType :: AWSJobAbortCriteriaFailureType,
    -- | The type of job action to take to initiate the job abort.
    action :: AWSJobAbortCriteriaAbortAction,
    -- | The minimum number of things which must receive job execution notifications before the job can be aborted.
    minNumberOfExecutedThings :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AWSJobAbortCriteria' with the minimum fields required to make a request.
--
-- * 'thresholdPercentage' - The minimum percentage of job execution failures that must occur to initiate the job abort.
--
-- AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
-- * 'failureType' - The type of job execution failures that can initiate a job abort.
-- * 'action' - The type of job action to take to initiate the job abort.
-- * 'minNumberOfExecutedThings' - The minimum number of things which must receive job execution notifications before the job can be aborted.
mkAWSJobAbortCriteria ::
  -- | 'thresholdPercentage'
  Lude.Double ->
  -- | 'failureType'
  AWSJobAbortCriteriaFailureType ->
  -- | 'action'
  AWSJobAbortCriteriaAbortAction ->
  -- | 'minNumberOfExecutedThings'
  Lude.Natural ->
  AWSJobAbortCriteria
mkAWSJobAbortCriteria
  pThresholdPercentage_
  pFailureType_
  pAction_
  pMinNumberOfExecutedThings_ =
    AWSJobAbortCriteria'
      { thresholdPercentage = pThresholdPercentage_,
        failureType = pFailureType_,
        action = pAction_,
        minNumberOfExecutedThings = pMinNumberOfExecutedThings_
      }

-- | The minimum percentage of job execution failures that must occur to initiate the job abort.
--
-- AWS IoT supports up to two digits after the decimal (for example, 10.9 and 10.99, but not 10.999).
--
-- /Note:/ Consider using 'thresholdPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajacThresholdPercentage :: Lens.Lens' AWSJobAbortCriteria Lude.Double
ajacThresholdPercentage = Lens.lens (thresholdPercentage :: AWSJobAbortCriteria -> Lude.Double) (\s a -> s {thresholdPercentage = a} :: AWSJobAbortCriteria)
{-# DEPRECATED ajacThresholdPercentage "Use generic-lens or generic-optics with 'thresholdPercentage' instead." #-}

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
          [ Lude.Just ("thresholdPercentage" Lude..= thresholdPercentage),
            Lude.Just ("failureType" Lude..= failureType),
            Lude.Just ("action" Lude..= action),
            Lude.Just
              ("minNumberOfExecutedThings" Lude..= minNumberOfExecutedThings)
          ]
      )
