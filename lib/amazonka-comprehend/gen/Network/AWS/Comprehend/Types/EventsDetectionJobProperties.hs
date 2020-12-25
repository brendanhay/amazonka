{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EventsDetectionJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EventsDetectionJobProperties
  ( EventsDetectionJobProperties (..),

    -- * Smart constructor
    mkEventsDetectionJobProperties,

    -- * Lenses
    edjpDataAccessRoleArn,
    edjpEndTime,
    edjpInputDataConfig,
    edjpJobId,
    edjpJobName,
    edjpJobStatus,
    edjpLanguageCode,
    edjpMessage,
    edjpOutputDataConfig,
    edjpSubmitTime,
    edjpTargetEventTypes,
  )
where

import qualified Network.AWS.Comprehend.Types.AnyLengthString as Types
import qualified Network.AWS.Comprehend.Types.EventTypeString as Types
import qualified Network.AWS.Comprehend.Types.IamRoleArn as Types
import qualified Network.AWS.Comprehend.Types.InputDataConfig as Types
import qualified Network.AWS.Comprehend.Types.JobId as Types
import qualified Network.AWS.Comprehend.Types.JobName as Types
import qualified Network.AWS.Comprehend.Types.JobStatus as Types
import qualified Network.AWS.Comprehend.Types.LanguageCode as Types
import qualified Network.AWS.Comprehend.Types.OutputDataConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about an events detection job.
--
-- /See:/ 'mkEventsDetectionJobProperties' smart constructor.
data EventsDetectionJobProperties = EventsDetectionJobProperties'
  { -- | The Amazon Resource Name (ARN) of the AWS Identify and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Core.Maybe Types.IamRoleArn,
    -- | The time that the events detection job completed.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The input data configuration that you supplied when you created the events detection job.
    inputDataConfig :: Core.Maybe Types.InputDataConfig,
    -- | The identifier assigned to the events detection job.
    jobId :: Core.Maybe Types.JobId,
    -- | The name you assigned the events detection job.
    jobName :: Core.Maybe Types.JobName,
    -- | The current status of the events detection job.
    jobStatus :: Core.Maybe Types.JobStatus,
    -- | The language code of the input documents.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | A description of the status of the events detection job.
    message :: Core.Maybe Types.AnyLengthString,
    -- | The output data configuration that you supplied when you created the events detection job.
    outputDataConfig :: Core.Maybe Types.OutputDataConfig,
    -- | The time that the events detection job was submitted for processing.
    submitTime :: Core.Maybe Core.NominalDiffTime,
    -- | The types of events that are detected by the job.
    targetEventTypes :: Core.Maybe (Core.NonEmpty Types.EventTypeString)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EventsDetectionJobProperties' value with any optional fields omitted.
mkEventsDetectionJobProperties ::
  EventsDetectionJobProperties
mkEventsDetectionJobProperties =
  EventsDetectionJobProperties'
    { dataAccessRoleArn = Core.Nothing,
      endTime = Core.Nothing,
      inputDataConfig = Core.Nothing,
      jobId = Core.Nothing,
      jobName = Core.Nothing,
      jobStatus = Core.Nothing,
      languageCode = Core.Nothing,
      message = Core.Nothing,
      outputDataConfig = Core.Nothing,
      submitTime = Core.Nothing,
      targetEventTypes = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the AWS Identify and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpDataAccessRoleArn :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Types.IamRoleArn)
edjpDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# DEPRECATED edjpDataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead." #-}

-- | The time that the events detection job completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpEndTime :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Core.NominalDiffTime)
edjpEndTime = Lens.field @"endTime"
{-# DEPRECATED edjpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The input data configuration that you supplied when you created the events detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpInputDataConfig :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Types.InputDataConfig)
edjpInputDataConfig = Lens.field @"inputDataConfig"
{-# DEPRECATED edjpInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | The identifier assigned to the events detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpJobId :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Types.JobId)
edjpJobId = Lens.field @"jobId"
{-# DEPRECATED edjpJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name you assigned the events detection job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpJobName :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Types.JobName)
edjpJobName = Lens.field @"jobName"
{-# DEPRECATED edjpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The current status of the events detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpJobStatus :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Types.JobStatus)
edjpJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED edjpJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The language code of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpLanguageCode :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Types.LanguageCode)
edjpLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED edjpLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | A description of the status of the events detection job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpMessage :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Types.AnyLengthString)
edjpMessage = Lens.field @"message"
{-# DEPRECATED edjpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The output data configuration that you supplied when you created the events detection job.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpOutputDataConfig :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Types.OutputDataConfig)
edjpOutputDataConfig = Lens.field @"outputDataConfig"
{-# DEPRECATED edjpOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The time that the events detection job was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpSubmitTime :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe Core.NominalDiffTime)
edjpSubmitTime = Lens.field @"submitTime"
{-# DEPRECATED edjpSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

-- | The types of events that are detected by the job.
--
-- /Note:/ Consider using 'targetEventTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpTargetEventTypes :: Lens.Lens' EventsDetectionJobProperties (Core.Maybe (Core.NonEmpty Types.EventTypeString))
edjpTargetEventTypes = Lens.field @"targetEventTypes"
{-# DEPRECATED edjpTargetEventTypes "Use generic-lens or generic-optics with 'targetEventTypes' instead." #-}

instance Core.FromJSON EventsDetectionJobProperties where
  parseJSON =
    Core.withObject "EventsDetectionJobProperties" Core.$
      \x ->
        EventsDetectionJobProperties'
          Core.<$> (x Core..:? "DataAccessRoleArn")
          Core.<*> (x Core..:? "EndTime")
          Core.<*> (x Core..:? "InputDataConfig")
          Core.<*> (x Core..:? "JobId")
          Core.<*> (x Core..:? "JobName")
          Core.<*> (x Core..:? "JobStatus")
          Core.<*> (x Core..:? "LanguageCode")
          Core.<*> (x Core..:? "Message")
          Core.<*> (x Core..:? "OutputDataConfig")
          Core.<*> (x Core..:? "SubmitTime")
          Core.<*> (x Core..:? "TargetEventTypes")
