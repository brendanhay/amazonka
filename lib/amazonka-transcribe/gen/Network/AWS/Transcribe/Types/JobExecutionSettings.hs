{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    jesAllowDeferredExecution,
    jesDataAccessRoleArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.DataAccessRoleArn as Types

-- | Provides information about when a transcription job should be executed.
--
-- /See:/ 'mkJobExecutionSettings' smart constructor.
data JobExecutionSettings = JobExecutionSettings'
  { -- | Indicates whether a job should be queued by Amazon Transcribe when the concurrent execution limit is exceeded. When the @AllowDeferredExecution@ field is true, jobs are queued and executed when the number of executing jobs falls below the concurrent execution limit. If the field is false, Amazon Transcribe returns a @LimitExceededException@ exception.
    --
    -- If you specify the @AllowDeferredExecution@ field, you must specify the @DataAccessRoleArn@ field.
    allowDeferredExecution :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of a role that has access to the S3 bucket that contains the input files. Amazon Transcribe assumes this role to read queued media files. If you have specified an output S3 bucket for the transcription results, this role should have access to the output bucket as well.
    --
    -- If you specify the @AllowDeferredExecution@ field, you must specify the @DataAccessRoleArn@ field.
    dataAccessRoleArn :: Core.Maybe Types.DataAccessRoleArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobExecutionSettings' value with any optional fields omitted.
mkJobExecutionSettings ::
  JobExecutionSettings
mkJobExecutionSettings =
  JobExecutionSettings'
    { allowDeferredExecution = Core.Nothing,
      dataAccessRoleArn = Core.Nothing
    }

-- | Indicates whether a job should be queued by Amazon Transcribe when the concurrent execution limit is exceeded. When the @AllowDeferredExecution@ field is true, jobs are queued and executed when the number of executing jobs falls below the concurrent execution limit. If the field is false, Amazon Transcribe returns a @LimitExceededException@ exception.
--
-- If you specify the @AllowDeferredExecution@ field, you must specify the @DataAccessRoleArn@ field.
--
-- /Note:/ Consider using 'allowDeferredExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesAllowDeferredExecution :: Lens.Lens' JobExecutionSettings (Core.Maybe Core.Bool)
jesAllowDeferredExecution = Lens.field @"allowDeferredExecution"
{-# DEPRECATED jesAllowDeferredExecution "Use generic-lens or generic-optics with 'allowDeferredExecution' instead." #-}

-- | The Amazon Resource Name (ARN) of a role that has access to the S3 bucket that contains the input files. Amazon Transcribe assumes this role to read queued media files. If you have specified an output S3 bucket for the transcription results, this role should have access to the output bucket as well.
--
-- If you specify the @AllowDeferredExecution@ field, you must specify the @DataAccessRoleArn@ field.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesDataAccessRoleArn :: Lens.Lens' JobExecutionSettings (Core.Maybe Types.DataAccessRoleArn)
jesDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# DEPRECATED jesDataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead." #-}

instance Core.FromJSON JobExecutionSettings where
  toJSON JobExecutionSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("AllowDeferredExecution" Core..=)
              Core.<$> allowDeferredExecution,
            ("DataAccessRoleArn" Core..=) Core.<$> dataAccessRoleArn
          ]
      )

instance Core.FromJSON JobExecutionSettings where
  parseJSON =
    Core.withObject "JobExecutionSettings" Core.$
      \x ->
        JobExecutionSettings'
          Core.<$> (x Core..:? "AllowDeferredExecution")
          Core.<*> (x Core..:? "DataAccessRoleArn")
