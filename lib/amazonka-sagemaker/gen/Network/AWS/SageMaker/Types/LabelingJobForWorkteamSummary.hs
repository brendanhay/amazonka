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
    ljfwsJobReferenceCode,
    ljfwsWorkRequesterAccountId,
    ljfwsCreationTime,
    ljfwsLabelCounters,
    ljfwsLabelingJobName,
    ljfwsNumberOfHumanWorkersPerDataObject,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AccountId as Types
import qualified Network.AWS.SageMaker.Types.JobReferenceCode as Types
import qualified Network.AWS.SageMaker.Types.LabelCountersForWorkteam as Types
import qualified Network.AWS.SageMaker.Types.LabelingJobName as Types

-- | Provides summary information for a work team.
--
-- /See:/ 'mkLabelingJobForWorkteamSummary' smart constructor.
data LabelingJobForWorkteamSummary = LabelingJobForWorkteamSummary'
  { -- | A unique identifier for a labeling job. You can use this to refer to a specific labeling job.
    jobReferenceCode :: Types.JobReferenceCode,
    -- |
    workRequesterAccountId :: Types.AccountId,
    -- | The date and time that the labeling job was created.
    creationTime :: Core.NominalDiffTime,
    -- | Provides information about the progress of a labeling job.
    labelCounters :: Core.Maybe Types.LabelCountersForWorkteam,
    -- | The name of the labeling job that the work team is assigned to.
    labelingJobName :: Core.Maybe Types.LabelingJobName,
    -- | The configured number of workers per data object.
    numberOfHumanWorkersPerDataObject :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LabelingJobForWorkteamSummary' value with any optional fields omitted.
mkLabelingJobForWorkteamSummary ::
  -- | 'jobReferenceCode'
  Types.JobReferenceCode ->
  -- | 'workRequesterAccountId'
  Types.AccountId ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  LabelingJobForWorkteamSummary
mkLabelingJobForWorkteamSummary
  jobReferenceCode
  workRequesterAccountId
  creationTime =
    LabelingJobForWorkteamSummary'
      { jobReferenceCode,
        workRequesterAccountId,
        creationTime,
        labelCounters = Core.Nothing,
        labelingJobName = Core.Nothing,
        numberOfHumanWorkersPerDataObject = Core.Nothing
      }

-- | A unique identifier for a labeling job. You can use this to refer to a specific labeling job.
--
-- /Note:/ Consider using 'jobReferenceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljfwsJobReferenceCode :: Lens.Lens' LabelingJobForWorkteamSummary Types.JobReferenceCode
ljfwsJobReferenceCode = Lens.field @"jobReferenceCode"
{-# DEPRECATED ljfwsJobReferenceCode "Use generic-lens or generic-optics with 'jobReferenceCode' instead." #-}

-- |
--
-- /Note:/ Consider using 'workRequesterAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljfwsWorkRequesterAccountId :: Lens.Lens' LabelingJobForWorkteamSummary Types.AccountId
ljfwsWorkRequesterAccountId = Lens.field @"workRequesterAccountId"
{-# DEPRECATED ljfwsWorkRequesterAccountId "Use generic-lens or generic-optics with 'workRequesterAccountId' instead." #-}

-- | The date and time that the labeling job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljfwsCreationTime :: Lens.Lens' LabelingJobForWorkteamSummary Core.NominalDiffTime
ljfwsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED ljfwsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Provides information about the progress of a labeling job.
--
-- /Note:/ Consider using 'labelCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljfwsLabelCounters :: Lens.Lens' LabelingJobForWorkteamSummary (Core.Maybe Types.LabelCountersForWorkteam)
ljfwsLabelCounters = Lens.field @"labelCounters"
{-# DEPRECATED ljfwsLabelCounters "Use generic-lens or generic-optics with 'labelCounters' instead." #-}

-- | The name of the labeling job that the work team is assigned to.
--
-- /Note:/ Consider using 'labelingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljfwsLabelingJobName :: Lens.Lens' LabelingJobForWorkteamSummary (Core.Maybe Types.LabelingJobName)
ljfwsLabelingJobName = Lens.field @"labelingJobName"
{-# DEPRECATED ljfwsLabelingJobName "Use generic-lens or generic-optics with 'labelingJobName' instead." #-}

-- | The configured number of workers per data object.
--
-- /Note:/ Consider using 'numberOfHumanWorkersPerDataObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljfwsNumberOfHumanWorkersPerDataObject :: Lens.Lens' LabelingJobForWorkteamSummary (Core.Maybe Core.Natural)
ljfwsNumberOfHumanWorkersPerDataObject = Lens.field @"numberOfHumanWorkersPerDataObject"
{-# DEPRECATED ljfwsNumberOfHumanWorkersPerDataObject "Use generic-lens or generic-optics with 'numberOfHumanWorkersPerDataObject' instead." #-}

instance Core.FromJSON LabelingJobForWorkteamSummary where
  parseJSON =
    Core.withObject "LabelingJobForWorkteamSummary" Core.$
      \x ->
        LabelingJobForWorkteamSummary'
          Core.<$> (x Core..: "JobReferenceCode")
          Core.<*> (x Core..: "WorkRequesterAccountId")
          Core.<*> (x Core..: "CreationTime")
          Core.<*> (x Core..:? "LabelCounters")
          Core.<*> (x Core..:? "LabelingJobName")
          Core.<*> (x Core..:? "NumberOfHumanWorkersPerDataObject")
