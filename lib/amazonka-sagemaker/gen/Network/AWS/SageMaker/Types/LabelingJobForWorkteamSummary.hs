{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobForWorkteamSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobForWorkteamSummary
  ( LabelingJobForWorkteamSummary (..),

    -- * Smart constructor
    mkLabelingJobForWorkteamSummary,

    -- * Lenses
    ljfwsNumberOfHumanWorkersPerDataObject,
    ljfwsLabelCounters,
    ljfwsLabelingJobName,
    ljfwsJobReferenceCode,
    ljfwsWorkRequesterAccountId,
    ljfwsCreationTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.LabelCountersForWorkteam

-- | Provides summary information for a work team.
--
-- /See:/ 'mkLabelingJobForWorkteamSummary' smart constructor.
data LabelingJobForWorkteamSummary = LabelingJobForWorkteamSummary'
  { numberOfHumanWorkersPerDataObject ::
      Lude.Maybe Lude.Natural,
    labelCounters ::
      Lude.Maybe
        LabelCountersForWorkteam,
    labelingJobName ::
      Lude.Maybe Lude.Text,
    jobReferenceCode :: Lude.Text,
    workRequesterAccountId ::
      Lude.Text,
    creationTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelingJobForWorkteamSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - The date and time that the labeling job was created.
-- * 'jobReferenceCode' - A unique identifier for a labeling job. You can use this to refer to a specific labeling job.
-- * 'labelCounters' - Provides information about the progress of a labeling job.
-- * 'labelingJobName' - The name of the labeling job that the work team is assigned to.
-- * 'numberOfHumanWorkersPerDataObject' - The configured number of workers per data object.
-- * 'workRequesterAccountId' -
mkLabelingJobForWorkteamSummary ::
  -- | 'jobReferenceCode'
  Lude.Text ->
  -- | 'workRequesterAccountId'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  LabelingJobForWorkteamSummary
mkLabelingJobForWorkteamSummary
  pJobReferenceCode_
  pWorkRequesterAccountId_
  pCreationTime_ =
    LabelingJobForWorkteamSummary'
      { numberOfHumanWorkersPerDataObject =
          Lude.Nothing,
        labelCounters = Lude.Nothing,
        labelingJobName = Lude.Nothing,
        jobReferenceCode = pJobReferenceCode_,
        workRequesterAccountId = pWorkRequesterAccountId_,
        creationTime = pCreationTime_
      }

-- | The configured number of workers per data object.
--
-- /Note:/ Consider using 'numberOfHumanWorkersPerDataObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljfwsNumberOfHumanWorkersPerDataObject :: Lens.Lens' LabelingJobForWorkteamSummary (Lude.Maybe Lude.Natural)
ljfwsNumberOfHumanWorkersPerDataObject = Lens.lens (numberOfHumanWorkersPerDataObject :: LabelingJobForWorkteamSummary -> Lude.Maybe Lude.Natural) (\s a -> s {numberOfHumanWorkersPerDataObject = a} :: LabelingJobForWorkteamSummary)
{-# DEPRECATED ljfwsNumberOfHumanWorkersPerDataObject "Use generic-lens or generic-optics with 'numberOfHumanWorkersPerDataObject' instead." #-}

-- | Provides information about the progress of a labeling job.
--
-- /Note:/ Consider using 'labelCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljfwsLabelCounters :: Lens.Lens' LabelingJobForWorkteamSummary (Lude.Maybe LabelCountersForWorkteam)
ljfwsLabelCounters = Lens.lens (labelCounters :: LabelingJobForWorkteamSummary -> Lude.Maybe LabelCountersForWorkteam) (\s a -> s {labelCounters = a} :: LabelingJobForWorkteamSummary)
{-# DEPRECATED ljfwsLabelCounters "Use generic-lens or generic-optics with 'labelCounters' instead." #-}

-- | The name of the labeling job that the work team is assigned to.
--
-- /Note:/ Consider using 'labelingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljfwsLabelingJobName :: Lens.Lens' LabelingJobForWorkteamSummary (Lude.Maybe Lude.Text)
ljfwsLabelingJobName = Lens.lens (labelingJobName :: LabelingJobForWorkteamSummary -> Lude.Maybe Lude.Text) (\s a -> s {labelingJobName = a} :: LabelingJobForWorkteamSummary)
{-# DEPRECATED ljfwsLabelingJobName "Use generic-lens or generic-optics with 'labelingJobName' instead." #-}

-- | A unique identifier for a labeling job. You can use this to refer to a specific labeling job.
--
-- /Note:/ Consider using 'jobReferenceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljfwsJobReferenceCode :: Lens.Lens' LabelingJobForWorkteamSummary Lude.Text
ljfwsJobReferenceCode = Lens.lens (jobReferenceCode :: LabelingJobForWorkteamSummary -> Lude.Text) (\s a -> s {jobReferenceCode = a} :: LabelingJobForWorkteamSummary)
{-# DEPRECATED ljfwsJobReferenceCode "Use generic-lens or generic-optics with 'jobReferenceCode' instead." #-}

-- |
--
-- /Note:/ Consider using 'workRequesterAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljfwsWorkRequesterAccountId :: Lens.Lens' LabelingJobForWorkteamSummary Lude.Text
ljfwsWorkRequesterAccountId = Lens.lens (workRequesterAccountId :: LabelingJobForWorkteamSummary -> Lude.Text) (\s a -> s {workRequesterAccountId = a} :: LabelingJobForWorkteamSummary)
{-# DEPRECATED ljfwsWorkRequesterAccountId "Use generic-lens or generic-optics with 'workRequesterAccountId' instead." #-}

-- | The date and time that the labeling job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljfwsCreationTime :: Lens.Lens' LabelingJobForWorkteamSummary Lude.Timestamp
ljfwsCreationTime = Lens.lens (creationTime :: LabelingJobForWorkteamSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: LabelingJobForWorkteamSummary)
{-# DEPRECATED ljfwsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

instance Lude.FromJSON LabelingJobForWorkteamSummary where
  parseJSON =
    Lude.withObject
      "LabelingJobForWorkteamSummary"
      ( \x ->
          LabelingJobForWorkteamSummary'
            Lude.<$> (x Lude..:? "NumberOfHumanWorkersPerDataObject")
            Lude.<*> (x Lude..:? "LabelCounters")
            Lude.<*> (x Lude..:? "LabelingJobName")
            Lude.<*> (x Lude..: "JobReferenceCode")
            Lude.<*> (x Lude..: "WorkRequesterAccountId")
            Lude.<*> (x Lude..: "CreationTime")
      )
