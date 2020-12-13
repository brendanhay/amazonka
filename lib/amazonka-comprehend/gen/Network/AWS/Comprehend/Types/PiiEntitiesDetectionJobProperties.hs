{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobProperties
  ( PiiEntitiesDetectionJobProperties (..),

    -- * Smart constructor
    mkPiiEntitiesDetectionJobProperties,

    -- * Lenses
    pedjpLanguageCode,
    pedjpJobId,
    pedjpJobName,
    pedjpMode,
    pedjpInputDataConfig,
    pedjpRedactionConfig,
    pedjpEndTime,
    pedjpOutputDataConfig,
    pedjpDataAccessRoleARN,
    pedjpJobStatus,
    pedjpMessage,
    pedjpSubmitTime,
  )
where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.PiiEntitiesDetectionMode
import Network.AWS.Comprehend.Types.PiiOutputDataConfig
import Network.AWS.Comprehend.Types.RedactionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a PII entities detection job.
--
-- /See:/ 'mkPiiEntitiesDetectionJobProperties' smart constructor.
data PiiEntitiesDetectionJobProperties = PiiEntitiesDetectionJobProperties'
  { -- | The language code of the input documents
    languageCode :: Lude.Maybe LanguageCode,
    -- | The identifier assigned to the PII entities detection job.
    jobId :: Lude.Maybe Lude.Text,
    -- | The name that you assigned the PII entities detection job.
    jobName :: Lude.Maybe Lude.Text,
    -- | Specifies whether the output provides the locations (offsets) of PII entities or a file in which PII entities are redacted.
    mode :: Lude.Maybe PiiEntitiesDetectionMode,
    -- | The input properties for a PII entities detection job.
    inputDataConfig :: Lude.Maybe InputDataConfig,
    -- | Provides configuration parameters for PII entity redaction.
    --
    -- This parameter is required if you set the @Mode@ parameter to @ONLY_REDACTION@ . In that case, you must provide a @RedactionConfig@ definition that includes the @PiiEntityTypes@ parameter.
    redactionConfig :: Lude.Maybe RedactionConfig,
    -- | The time that the PII entities detection job completed.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The output data configuration that you supplied when you created the PII entities detection job.
    outputDataConfig :: Lude.Maybe PiiOutputDataConfig,
    -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
    dataAccessRoleARN :: Lude.Maybe Lude.Text,
    -- | The current status of the PII entities detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
    jobStatus :: Lude.Maybe JobStatus,
    -- | A description of the status of a job.
    message :: Lude.Maybe Lude.Text,
    -- | The time that the PII entities detection job was submitted for processing.
    submitTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PiiEntitiesDetectionJobProperties' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language code of the input documents
-- * 'jobId' - The identifier assigned to the PII entities detection job.
-- * 'jobName' - The name that you assigned the PII entities detection job.
-- * 'mode' - Specifies whether the output provides the locations (offsets) of PII entities or a file in which PII entities are redacted.
-- * 'inputDataConfig' - The input properties for a PII entities detection job.
-- * 'redactionConfig' - Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to @ONLY_REDACTION@ . In that case, you must provide a @RedactionConfig@ definition that includes the @PiiEntityTypes@ parameter.
-- * 'endTime' - The time that the PII entities detection job completed.
-- * 'outputDataConfig' - The output data configuration that you supplied when you created the PII entities detection job.
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
-- * 'jobStatus' - The current status of the PII entities detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
-- * 'message' - A description of the status of a job.
-- * 'submitTime' - The time that the PII entities detection job was submitted for processing.
mkPiiEntitiesDetectionJobProperties ::
  PiiEntitiesDetectionJobProperties
mkPiiEntitiesDetectionJobProperties =
  PiiEntitiesDetectionJobProperties'
    { languageCode = Lude.Nothing,
      jobId = Lude.Nothing,
      jobName = Lude.Nothing,
      mode = Lude.Nothing,
      inputDataConfig = Lude.Nothing,
      redactionConfig = Lude.Nothing,
      endTime = Lude.Nothing,
      outputDataConfig = Lude.Nothing,
      dataAccessRoleARN = Lude.Nothing,
      jobStatus = Lude.Nothing,
      message = Lude.Nothing,
      submitTime = Lude.Nothing
    }

-- | The language code of the input documents
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjpLanguageCode :: Lens.Lens' PiiEntitiesDetectionJobProperties (Lude.Maybe LanguageCode)
pedjpLanguageCode = Lens.lens (languageCode :: PiiEntitiesDetectionJobProperties -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: PiiEntitiesDetectionJobProperties)
{-# DEPRECATED pedjpLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The identifier assigned to the PII entities detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjpJobId :: Lens.Lens' PiiEntitiesDetectionJobProperties (Lude.Maybe Lude.Text)
pedjpJobId = Lens.lens (jobId :: PiiEntitiesDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: PiiEntitiesDetectionJobProperties)
{-# DEPRECATED pedjpJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name that you assigned the PII entities detection job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjpJobName :: Lens.Lens' PiiEntitiesDetectionJobProperties (Lude.Maybe Lude.Text)
pedjpJobName = Lens.lens (jobName :: PiiEntitiesDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: PiiEntitiesDetectionJobProperties)
{-# DEPRECATED pedjpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Specifies whether the output provides the locations (offsets) of PII entities or a file in which PII entities are redacted.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjpMode :: Lens.Lens' PiiEntitiesDetectionJobProperties (Lude.Maybe PiiEntitiesDetectionMode)
pedjpMode = Lens.lens (mode :: PiiEntitiesDetectionJobProperties -> Lude.Maybe PiiEntitiesDetectionMode) (\s a -> s {mode = a} :: PiiEntitiesDetectionJobProperties)
{-# DEPRECATED pedjpMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The input properties for a PII entities detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjpInputDataConfig :: Lens.Lens' PiiEntitiesDetectionJobProperties (Lude.Maybe InputDataConfig)
pedjpInputDataConfig = Lens.lens (inputDataConfig :: PiiEntitiesDetectionJobProperties -> Lude.Maybe InputDataConfig) (\s a -> s {inputDataConfig = a} :: PiiEntitiesDetectionJobProperties)
{-# DEPRECATED pedjpInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to @ONLY_REDACTION@ . In that case, you must provide a @RedactionConfig@ definition that includes the @PiiEntityTypes@ parameter.
--
-- /Note:/ Consider using 'redactionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjpRedactionConfig :: Lens.Lens' PiiEntitiesDetectionJobProperties (Lude.Maybe RedactionConfig)
pedjpRedactionConfig = Lens.lens (redactionConfig :: PiiEntitiesDetectionJobProperties -> Lude.Maybe RedactionConfig) (\s a -> s {redactionConfig = a} :: PiiEntitiesDetectionJobProperties)
{-# DEPRECATED pedjpRedactionConfig "Use generic-lens or generic-optics with 'redactionConfig' instead." #-}

-- | The time that the PII entities detection job completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjpEndTime :: Lens.Lens' PiiEntitiesDetectionJobProperties (Lude.Maybe Lude.Timestamp)
pedjpEndTime = Lens.lens (endTime :: PiiEntitiesDetectionJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: PiiEntitiesDetectionJobProperties)
{-# DEPRECATED pedjpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The output data configuration that you supplied when you created the PII entities detection job.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjpOutputDataConfig :: Lens.Lens' PiiEntitiesDetectionJobProperties (Lude.Maybe PiiOutputDataConfig)
pedjpOutputDataConfig = Lens.lens (outputDataConfig :: PiiEntitiesDetectionJobProperties -> Lude.Maybe PiiOutputDataConfig) (\s a -> s {outputDataConfig = a} :: PiiEntitiesDetectionJobProperties)
{-# DEPRECATED pedjpOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjpDataAccessRoleARN :: Lens.Lens' PiiEntitiesDetectionJobProperties (Lude.Maybe Lude.Text)
pedjpDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: PiiEntitiesDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: PiiEntitiesDetectionJobProperties)
{-# DEPRECATED pedjpDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | The current status of the PII entities detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjpJobStatus :: Lens.Lens' PiiEntitiesDetectionJobProperties (Lude.Maybe JobStatus)
pedjpJobStatus = Lens.lens (jobStatus :: PiiEntitiesDetectionJobProperties -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: PiiEntitiesDetectionJobProperties)
{-# DEPRECATED pedjpJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | A description of the status of a job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjpMessage :: Lens.Lens' PiiEntitiesDetectionJobProperties (Lude.Maybe Lude.Text)
pedjpMessage = Lens.lens (message :: PiiEntitiesDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: PiiEntitiesDetectionJobProperties)
{-# DEPRECATED pedjpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time that the PII entities detection job was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjpSubmitTime :: Lens.Lens' PiiEntitiesDetectionJobProperties (Lude.Maybe Lude.Timestamp)
pedjpSubmitTime = Lens.lens (submitTime :: PiiEntitiesDetectionJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTime = a} :: PiiEntitiesDetectionJobProperties)
{-# DEPRECATED pedjpSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

instance Lude.FromJSON PiiEntitiesDetectionJobProperties where
  parseJSON =
    Lude.withObject
      "PiiEntitiesDetectionJobProperties"
      ( \x ->
          PiiEntitiesDetectionJobProperties'
            Lude.<$> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "JobId")
            Lude.<*> (x Lude..:? "JobName")
            Lude.<*> (x Lude..:? "Mode")
            Lude.<*> (x Lude..:? "InputDataConfig")
            Lude.<*> (x Lude..:? "RedactionConfig")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "OutputDataConfig")
            Lude.<*> (x Lude..:? "DataAccessRoleArn")
            Lude.<*> (x Lude..:? "JobStatus")
            Lude.<*> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "SubmitTime")
      )
