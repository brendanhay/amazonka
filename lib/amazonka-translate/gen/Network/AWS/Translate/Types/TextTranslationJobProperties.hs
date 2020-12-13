{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.TextTranslationJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.TextTranslationJobProperties
  ( TextTranslationJobProperties (..),

    -- * Smart constructor
    mkTextTranslationJobProperties,

    -- * Lenses
    ttjpJobId,
    ttjpTargetLanguageCodes,
    ttjpJobName,
    ttjpSubmittedTime,
    ttjpInputDataConfig,
    ttjpParallelDataNames,
    ttjpTerminologyNames,
    ttjpSourceLanguageCode,
    ttjpEndTime,
    ttjpOutputDataConfig,
    ttjpJobDetails,
    ttjpDataAccessRoleARN,
    ttjpJobStatus,
    ttjpMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Translate.Types.InputDataConfig
import Network.AWS.Translate.Types.JobDetails
import Network.AWS.Translate.Types.JobStatus
import Network.AWS.Translate.Types.OutputDataConfig

-- | Provides information about a translation job.
--
-- /See:/ 'mkTextTranslationJobProperties' smart constructor.
data TextTranslationJobProperties = TextTranslationJobProperties'
  { -- | The ID of the translation job.
    jobId :: Lude.Maybe Lude.Text,
    -- | The language code of the language of the target text. The language must be a language supported by Amazon Translate.
    targetLanguageCodes :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The user-defined name of the translation job.
    jobName :: Lude.Maybe Lude.Text,
    -- | The time at which the translation job was submitted.
    submittedTime :: Lude.Maybe Lude.Timestamp,
    -- | The input configuration properties that were specified when the job was requested.
    inputDataConfig :: Lude.Maybe InputDataConfig,
    -- | A list containing the names of the parallel data resources applied to the translation job.
    parallelDataNames :: Lude.Maybe [Lude.Text],
    -- | A list containing the names of the terminologies applied to a translation job. Only one terminology can be applied per 'StartTextTranslationJob' request at this time.
    terminologyNames :: Lude.Maybe [Lude.Text],
    -- | The language code of the language of the source text. The language must be a language supported by Amazon Translate.
    sourceLanguageCode :: Lude.Maybe Lude.Text,
    -- | The time at which the translation job ended.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The output configuration properties that were specified when the job was requested.
    outputDataConfig :: Lude.Maybe OutputDataConfig,
    -- | The number of documents successfully and unsuccessfully processed during the translation job.
    jobDetails :: Lude.Maybe JobDetails,
    -- | The Amazon Resource Name (ARN) of an AWS Identity Access and Management (IAM) role that granted Amazon Translate read access to the job's input data.
    dataAccessRoleARN :: Lude.Maybe Lude.Text,
    -- | The status of the translation job.
    jobStatus :: Lude.Maybe JobStatus,
    -- | An explanation of any errors that may have occured during the translation job.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TextTranslationJobProperties' with the minimum fields required to make a request.
--
-- * 'jobId' - The ID of the translation job.
-- * 'targetLanguageCodes' - The language code of the language of the target text. The language must be a language supported by Amazon Translate.
-- * 'jobName' - The user-defined name of the translation job.
-- * 'submittedTime' - The time at which the translation job was submitted.
-- * 'inputDataConfig' - The input configuration properties that were specified when the job was requested.
-- * 'parallelDataNames' - A list containing the names of the parallel data resources applied to the translation job.
-- * 'terminologyNames' - A list containing the names of the terminologies applied to a translation job. Only one terminology can be applied per 'StartTextTranslationJob' request at this time.
-- * 'sourceLanguageCode' - The language code of the language of the source text. The language must be a language supported by Amazon Translate.
-- * 'endTime' - The time at which the translation job ended.
-- * 'outputDataConfig' - The output configuration properties that were specified when the job was requested.
-- * 'jobDetails' - The number of documents successfully and unsuccessfully processed during the translation job.
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of an AWS Identity Access and Management (IAM) role that granted Amazon Translate read access to the job's input data.
-- * 'jobStatus' - The status of the translation job.
-- * 'message' - An explanation of any errors that may have occured during the translation job.
mkTextTranslationJobProperties ::
  TextTranslationJobProperties
mkTextTranslationJobProperties =
  TextTranslationJobProperties'
    { jobId = Lude.Nothing,
      targetLanguageCodes = Lude.Nothing,
      jobName = Lude.Nothing,
      submittedTime = Lude.Nothing,
      inputDataConfig = Lude.Nothing,
      parallelDataNames = Lude.Nothing,
      terminologyNames = Lude.Nothing,
      sourceLanguageCode = Lude.Nothing,
      endTime = Lude.Nothing,
      outputDataConfig = Lude.Nothing,
      jobDetails = Lude.Nothing,
      dataAccessRoleARN = Lude.Nothing,
      jobStatus = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The ID of the translation job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjpJobId :: Lens.Lens' TextTranslationJobProperties (Lude.Maybe Lude.Text)
ttjpJobId = Lens.lens (jobId :: TextTranslationJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: TextTranslationJobProperties)
{-# DEPRECATED ttjpJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The language code of the language of the target text. The language must be a language supported by Amazon Translate.
--
-- /Note:/ Consider using 'targetLanguageCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjpTargetLanguageCodes :: Lens.Lens' TextTranslationJobProperties (Lude.Maybe (Lude.NonEmpty Lude.Text))
ttjpTargetLanguageCodes = Lens.lens (targetLanguageCodes :: TextTranslationJobProperties -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {targetLanguageCodes = a} :: TextTranslationJobProperties)
{-# DEPRECATED ttjpTargetLanguageCodes "Use generic-lens or generic-optics with 'targetLanguageCodes' instead." #-}

-- | The user-defined name of the translation job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjpJobName :: Lens.Lens' TextTranslationJobProperties (Lude.Maybe Lude.Text)
ttjpJobName = Lens.lens (jobName :: TextTranslationJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: TextTranslationJobProperties)
{-# DEPRECATED ttjpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The time at which the translation job was submitted.
--
-- /Note:/ Consider using 'submittedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjpSubmittedTime :: Lens.Lens' TextTranslationJobProperties (Lude.Maybe Lude.Timestamp)
ttjpSubmittedTime = Lens.lens (submittedTime :: TextTranslationJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {submittedTime = a} :: TextTranslationJobProperties)
{-# DEPRECATED ttjpSubmittedTime "Use generic-lens or generic-optics with 'submittedTime' instead." #-}

-- | The input configuration properties that were specified when the job was requested.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjpInputDataConfig :: Lens.Lens' TextTranslationJobProperties (Lude.Maybe InputDataConfig)
ttjpInputDataConfig = Lens.lens (inputDataConfig :: TextTranslationJobProperties -> Lude.Maybe InputDataConfig) (\s a -> s {inputDataConfig = a} :: TextTranslationJobProperties)
{-# DEPRECATED ttjpInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | A list containing the names of the parallel data resources applied to the translation job.
--
-- /Note:/ Consider using 'parallelDataNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjpParallelDataNames :: Lens.Lens' TextTranslationJobProperties (Lude.Maybe [Lude.Text])
ttjpParallelDataNames = Lens.lens (parallelDataNames :: TextTranslationJobProperties -> Lude.Maybe [Lude.Text]) (\s a -> s {parallelDataNames = a} :: TextTranslationJobProperties)
{-# DEPRECATED ttjpParallelDataNames "Use generic-lens or generic-optics with 'parallelDataNames' instead." #-}

-- | A list containing the names of the terminologies applied to a translation job. Only one terminology can be applied per 'StartTextTranslationJob' request at this time.
--
-- /Note:/ Consider using 'terminologyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjpTerminologyNames :: Lens.Lens' TextTranslationJobProperties (Lude.Maybe [Lude.Text])
ttjpTerminologyNames = Lens.lens (terminologyNames :: TextTranslationJobProperties -> Lude.Maybe [Lude.Text]) (\s a -> s {terminologyNames = a} :: TextTranslationJobProperties)
{-# DEPRECATED ttjpTerminologyNames "Use generic-lens or generic-optics with 'terminologyNames' instead." #-}

-- | The language code of the language of the source text. The language must be a language supported by Amazon Translate.
--
-- /Note:/ Consider using 'sourceLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjpSourceLanguageCode :: Lens.Lens' TextTranslationJobProperties (Lude.Maybe Lude.Text)
ttjpSourceLanguageCode = Lens.lens (sourceLanguageCode :: TextTranslationJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {sourceLanguageCode = a} :: TextTranslationJobProperties)
{-# DEPRECATED ttjpSourceLanguageCode "Use generic-lens or generic-optics with 'sourceLanguageCode' instead." #-}

-- | The time at which the translation job ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjpEndTime :: Lens.Lens' TextTranslationJobProperties (Lude.Maybe Lude.Timestamp)
ttjpEndTime = Lens.lens (endTime :: TextTranslationJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: TextTranslationJobProperties)
{-# DEPRECATED ttjpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The output configuration properties that were specified when the job was requested.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjpOutputDataConfig :: Lens.Lens' TextTranslationJobProperties (Lude.Maybe OutputDataConfig)
ttjpOutputDataConfig = Lens.lens (outputDataConfig :: TextTranslationJobProperties -> Lude.Maybe OutputDataConfig) (\s a -> s {outputDataConfig = a} :: TextTranslationJobProperties)
{-# DEPRECATED ttjpOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The number of documents successfully and unsuccessfully processed during the translation job.
--
-- /Note:/ Consider using 'jobDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjpJobDetails :: Lens.Lens' TextTranslationJobProperties (Lude.Maybe JobDetails)
ttjpJobDetails = Lens.lens (jobDetails :: TextTranslationJobProperties -> Lude.Maybe JobDetails) (\s a -> s {jobDetails = a} :: TextTranslationJobProperties)
{-# DEPRECATED ttjpJobDetails "Use generic-lens or generic-optics with 'jobDetails' instead." #-}

-- | The Amazon Resource Name (ARN) of an AWS Identity Access and Management (IAM) role that granted Amazon Translate read access to the job's input data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjpDataAccessRoleARN :: Lens.Lens' TextTranslationJobProperties (Lude.Maybe Lude.Text)
ttjpDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: TextTranslationJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: TextTranslationJobProperties)
{-# DEPRECATED ttjpDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | The status of the translation job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjpJobStatus :: Lens.Lens' TextTranslationJobProperties (Lude.Maybe JobStatus)
ttjpJobStatus = Lens.lens (jobStatus :: TextTranslationJobProperties -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: TextTranslationJobProperties)
{-# DEPRECATED ttjpJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | An explanation of any errors that may have occured during the translation job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjpMessage :: Lens.Lens' TextTranslationJobProperties (Lude.Maybe Lude.Text)
ttjpMessage = Lens.lens (message :: TextTranslationJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: TextTranslationJobProperties)
{-# DEPRECATED ttjpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON TextTranslationJobProperties where
  parseJSON =
    Lude.withObject
      "TextTranslationJobProperties"
      ( \x ->
          TextTranslationJobProperties'
            Lude.<$> (x Lude..:? "JobId")
            Lude.<*> (x Lude..:? "TargetLanguageCodes")
            Lude.<*> (x Lude..:? "JobName")
            Lude.<*> (x Lude..:? "SubmittedTime")
            Lude.<*> (x Lude..:? "InputDataConfig")
            Lude.<*> (x Lude..:? "ParallelDataNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TerminologyNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SourceLanguageCode")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "OutputDataConfig")
            Lude.<*> (x Lude..:? "JobDetails")
            Lude.<*> (x Lude..:? "DataAccessRoleArn")
            Lude.<*> (x Lude..:? "JobStatus")
            Lude.<*> (x Lude..:? "Message")
      )
