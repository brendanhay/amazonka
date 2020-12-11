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
    edjpLanguageCode,
    edjpJobId,
    edjpJobName,
    edjpTargetEventTypes,
    edjpInputDataConfig,
    edjpEndTime,
    edjpOutputDataConfig,
    edjpDataAccessRoleARN,
    edjpJobStatus,
    edjpMessage,
    edjpSubmitTime,
  )
where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.OutputDataConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about an events detection job.
--
-- /See:/ 'mkEventsDetectionJobProperties' smart constructor.
data EventsDetectionJobProperties = EventsDetectionJobProperties'
  { languageCode ::
      Lude.Maybe LanguageCode,
    jobId :: Lude.Maybe Lude.Text,
    jobName :: Lude.Maybe Lude.Text,
    targetEventTypes ::
      Lude.Maybe
        (Lude.NonEmpty Lude.Text),
    inputDataConfig ::
      Lude.Maybe InputDataConfig,
    endTime ::
      Lude.Maybe Lude.Timestamp,
    outputDataConfig ::
      Lude.Maybe OutputDataConfig,
    dataAccessRoleARN ::
      Lude.Maybe Lude.Text,
    jobStatus :: Lude.Maybe JobStatus,
    message :: Lude.Maybe Lude.Text,
    submitTime ::
      Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventsDetectionJobProperties' with the minimum fields required to make a request.
--
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identify and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
-- * 'endTime' - The time that the events detection job completed.
-- * 'inputDataConfig' - The input data configuration that you supplied when you created the events detection job.
-- * 'jobId' - The identifier assigned to the events detection job.
-- * 'jobName' - The name you assigned the events detection job.
-- * 'jobStatus' - The current status of the events detection job.
-- * 'languageCode' - The language code of the input documents.
-- * 'message' - A description of the status of the events detection job.
-- * 'outputDataConfig' - The output data configuration that you supplied when you created the events detection job.
-- * 'submitTime' - The time that the events detection job was submitted for processing.
-- * 'targetEventTypes' - The types of events that are detected by the job.
mkEventsDetectionJobProperties ::
  EventsDetectionJobProperties
mkEventsDetectionJobProperties =
  EventsDetectionJobProperties'
    { languageCode = Lude.Nothing,
      jobId = Lude.Nothing,
      jobName = Lude.Nothing,
      targetEventTypes = Lude.Nothing,
      inputDataConfig = Lude.Nothing,
      endTime = Lude.Nothing,
      outputDataConfig = Lude.Nothing,
      dataAccessRoleARN = Lude.Nothing,
      jobStatus = Lude.Nothing,
      message = Lude.Nothing,
      submitTime = Lude.Nothing
    }

-- | The language code of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpLanguageCode :: Lens.Lens' EventsDetectionJobProperties (Lude.Maybe LanguageCode)
edjpLanguageCode = Lens.lens (languageCode :: EventsDetectionJobProperties -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: EventsDetectionJobProperties)
{-# DEPRECATED edjpLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The identifier assigned to the events detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpJobId :: Lens.Lens' EventsDetectionJobProperties (Lude.Maybe Lude.Text)
edjpJobId = Lens.lens (jobId :: EventsDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: EventsDetectionJobProperties)
{-# DEPRECATED edjpJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name you assigned the events detection job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpJobName :: Lens.Lens' EventsDetectionJobProperties (Lude.Maybe Lude.Text)
edjpJobName = Lens.lens (jobName :: EventsDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: EventsDetectionJobProperties)
{-# DEPRECATED edjpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The types of events that are detected by the job.
--
-- /Note:/ Consider using 'targetEventTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpTargetEventTypes :: Lens.Lens' EventsDetectionJobProperties (Lude.Maybe (Lude.NonEmpty Lude.Text))
edjpTargetEventTypes = Lens.lens (targetEventTypes :: EventsDetectionJobProperties -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {targetEventTypes = a} :: EventsDetectionJobProperties)
{-# DEPRECATED edjpTargetEventTypes "Use generic-lens or generic-optics with 'targetEventTypes' instead." #-}

-- | The input data configuration that you supplied when you created the events detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpInputDataConfig :: Lens.Lens' EventsDetectionJobProperties (Lude.Maybe InputDataConfig)
edjpInputDataConfig = Lens.lens (inputDataConfig :: EventsDetectionJobProperties -> Lude.Maybe InputDataConfig) (\s a -> s {inputDataConfig = a} :: EventsDetectionJobProperties)
{-# DEPRECATED edjpInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | The time that the events detection job completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpEndTime :: Lens.Lens' EventsDetectionJobProperties (Lude.Maybe Lude.Timestamp)
edjpEndTime = Lens.lens (endTime :: EventsDetectionJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: EventsDetectionJobProperties)
{-# DEPRECATED edjpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The output data configuration that you supplied when you created the events detection job.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpOutputDataConfig :: Lens.Lens' EventsDetectionJobProperties (Lude.Maybe OutputDataConfig)
edjpOutputDataConfig = Lens.lens (outputDataConfig :: EventsDetectionJobProperties -> Lude.Maybe OutputDataConfig) (\s a -> s {outputDataConfig = a} :: EventsDetectionJobProperties)
{-# DEPRECATED edjpOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identify and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpDataAccessRoleARN :: Lens.Lens' EventsDetectionJobProperties (Lude.Maybe Lude.Text)
edjpDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: EventsDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: EventsDetectionJobProperties)
{-# DEPRECATED edjpDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | The current status of the events detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpJobStatus :: Lens.Lens' EventsDetectionJobProperties (Lude.Maybe JobStatus)
edjpJobStatus = Lens.lens (jobStatus :: EventsDetectionJobProperties -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: EventsDetectionJobProperties)
{-# DEPRECATED edjpJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | A description of the status of the events detection job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpMessage :: Lens.Lens' EventsDetectionJobProperties (Lude.Maybe Lude.Text)
edjpMessage = Lens.lens (message :: EventsDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: EventsDetectionJobProperties)
{-# DEPRECATED edjpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time that the events detection job was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjpSubmitTime :: Lens.Lens' EventsDetectionJobProperties (Lude.Maybe Lude.Timestamp)
edjpSubmitTime = Lens.lens (submitTime :: EventsDetectionJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTime = a} :: EventsDetectionJobProperties)
{-# DEPRECATED edjpSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

instance Lude.FromJSON EventsDetectionJobProperties where
  parseJSON =
    Lude.withObject
      "EventsDetectionJobProperties"
      ( \x ->
          EventsDetectionJobProperties'
            Lude.<$> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "JobId")
            Lude.<*> (x Lude..:? "JobName")
            Lude.<*> (x Lude..:? "TargetEventTypes")
            Lude.<*> (x Lude..:? "InputDataConfig")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "OutputDataConfig")
            Lude.<*> (x Lude..:? "DataAccessRoleArn")
            Lude.<*> (x Lude..:? "JobStatus")
            Lude.<*> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "SubmitTime")
      )
