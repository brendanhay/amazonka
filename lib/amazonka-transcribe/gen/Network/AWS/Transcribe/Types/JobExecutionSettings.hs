-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.JobExecutionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.JobExecutionSettings
  ( JobExecutionSettings (..),

    -- * Smart constructor
    mkJobExecutionSettings,

    -- * Lenses
    jesDataAccessRoleARN,
    jesAllowDeferredExecution,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about when a transcription job should be executed.
--
-- /See:/ 'mkJobExecutionSettings' smart constructor.
data JobExecutionSettings = JobExecutionSettings'
  { dataAccessRoleARN ::
      Lude.Maybe Lude.Text,
    allowDeferredExecution :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobExecutionSettings' with the minimum fields required to make a request.
--
-- * 'allowDeferredExecution' - Indicates whether a job should be queued by Amazon Transcribe when the concurrent execution limit is exceeded. When the @AllowDeferredExecution@ field is true, jobs are queued and executed when the number of executing jobs falls below the concurrent execution limit. If the field is false, Amazon Transcribe returns a @LimitExceededException@ exception.
--
-- If you specify the @AllowDeferredExecution@ field, you must specify the @DataAccessRoleArn@ field.
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of a role that has access to the S3 bucket that contains the input files. Amazon Transcribe assumes this role to read queued media files. If you have specified an output S3 bucket for the transcription results, this role should have access to the output bucket as well.
--
-- If you specify the @AllowDeferredExecution@ field, you must specify the @DataAccessRoleArn@ field.
mkJobExecutionSettings ::
  JobExecutionSettings
mkJobExecutionSettings =
  JobExecutionSettings'
    { dataAccessRoleARN = Lude.Nothing,
      allowDeferredExecution = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of a role that has access to the S3 bucket that contains the input files. Amazon Transcribe assumes this role to read queued media files. If you have specified an output S3 bucket for the transcription results, this role should have access to the output bucket as well.
--
-- If you specify the @AllowDeferredExecution@ field, you must specify the @DataAccessRoleArn@ field.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesDataAccessRoleARN :: Lens.Lens' JobExecutionSettings (Lude.Maybe Lude.Text)
jesDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: JobExecutionSettings -> Lude.Maybe Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: JobExecutionSettings)
{-# DEPRECATED jesDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | Indicates whether a job should be queued by Amazon Transcribe when the concurrent execution limit is exceeded. When the @AllowDeferredExecution@ field is true, jobs are queued and executed when the number of executing jobs falls below the concurrent execution limit. If the field is false, Amazon Transcribe returns a @LimitExceededException@ exception.
--
-- If you specify the @AllowDeferredExecution@ field, you must specify the @DataAccessRoleArn@ field.
--
-- /Note:/ Consider using 'allowDeferredExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesAllowDeferredExecution :: Lens.Lens' JobExecutionSettings (Lude.Maybe Lude.Bool)
jesAllowDeferredExecution = Lens.lens (allowDeferredExecution :: JobExecutionSettings -> Lude.Maybe Lude.Bool) (\s a -> s {allowDeferredExecution = a} :: JobExecutionSettings)
{-# DEPRECATED jesAllowDeferredExecution "Use generic-lens or generic-optics with 'allowDeferredExecution' instead." #-}

instance Lude.FromJSON JobExecutionSettings where
  parseJSON =
    Lude.withObject
      "JobExecutionSettings"
      ( \x ->
          JobExecutionSettings'
            Lude.<$> (x Lude..:? "DataAccessRoleArn")
            Lude.<*> (x Lude..:? "AllowDeferredExecution")
      )

instance Lude.ToJSON JobExecutionSettings where
  toJSON JobExecutionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DataAccessRoleArn" Lude..=) Lude.<$> dataAccessRoleARN,
            ("AllowDeferredExecution" Lude..=)
              Lude.<$> allowDeferredExecution
          ]
      )
