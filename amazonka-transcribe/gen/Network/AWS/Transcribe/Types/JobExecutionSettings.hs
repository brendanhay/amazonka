{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.JobExecutionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.JobExecutionSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about when a transcription job should be executed.
--
-- /See:/ 'newJobExecutionSettings' smart constructor.
data JobExecutionSettings = JobExecutionSettings'
  { -- | Indicates whether a job should be queued by Amazon Transcribe when the
    -- concurrent execution limit is exceeded. When the
    -- @AllowDeferredExecution@ field is true, jobs are queued and executed
    -- when the number of executing jobs falls below the concurrent execution
    -- limit. If the field is false, Amazon Transcribe returns a
    -- @LimitExceededException@ exception.
    --
    -- If you specify the @AllowDeferredExecution@ field, you must specify the
    -- @DataAccessRoleArn@ field.
    allowDeferredExecution :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of a role that has access to the S3
    -- bucket that contains the input files. Amazon Transcribe assumes this
    -- role to read queued media files. If you have specified an output S3
    -- bucket for the transcription results, this role should have access to
    -- the output bucket as well.
    --
    -- If you specify the @AllowDeferredExecution@ field, you must specify the
    -- @DataAccessRoleArn@ field.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'JobExecutionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowDeferredExecution', 'jobExecutionSettings_allowDeferredExecution' - Indicates whether a job should be queued by Amazon Transcribe when the
-- concurrent execution limit is exceeded. When the
-- @AllowDeferredExecution@ field is true, jobs are queued and executed
-- when the number of executing jobs falls below the concurrent execution
-- limit. If the field is false, Amazon Transcribe returns a
-- @LimitExceededException@ exception.
--
-- If you specify the @AllowDeferredExecution@ field, you must specify the
-- @DataAccessRoleArn@ field.
--
-- 'dataAccessRoleArn', 'jobExecutionSettings_dataAccessRoleArn' - The Amazon Resource Name (ARN) of a role that has access to the S3
-- bucket that contains the input files. Amazon Transcribe assumes this
-- role to read queued media files. If you have specified an output S3
-- bucket for the transcription results, this role should have access to
-- the output bucket as well.
--
-- If you specify the @AllowDeferredExecution@ field, you must specify the
-- @DataAccessRoleArn@ field.
newJobExecutionSettings ::
  JobExecutionSettings
newJobExecutionSettings =
  JobExecutionSettings'
    { allowDeferredExecution =
        Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing
    }

-- | Indicates whether a job should be queued by Amazon Transcribe when the
-- concurrent execution limit is exceeded. When the
-- @AllowDeferredExecution@ field is true, jobs are queued and executed
-- when the number of executing jobs falls below the concurrent execution
-- limit. If the field is false, Amazon Transcribe returns a
-- @LimitExceededException@ exception.
--
-- If you specify the @AllowDeferredExecution@ field, you must specify the
-- @DataAccessRoleArn@ field.
jobExecutionSettings_allowDeferredExecution :: Lens.Lens' JobExecutionSettings (Prelude.Maybe Prelude.Bool)
jobExecutionSettings_allowDeferredExecution = Lens.lens (\JobExecutionSettings' {allowDeferredExecution} -> allowDeferredExecution) (\s@JobExecutionSettings' {} a -> s {allowDeferredExecution = a} :: JobExecutionSettings)

-- | The Amazon Resource Name (ARN) of a role that has access to the S3
-- bucket that contains the input files. Amazon Transcribe assumes this
-- role to read queued media files. If you have specified an output S3
-- bucket for the transcription results, this role should have access to
-- the output bucket as well.
--
-- If you specify the @AllowDeferredExecution@ field, you must specify the
-- @DataAccessRoleArn@ field.
jobExecutionSettings_dataAccessRoleArn :: Lens.Lens' JobExecutionSettings (Prelude.Maybe Prelude.Text)
jobExecutionSettings_dataAccessRoleArn = Lens.lens (\JobExecutionSettings' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@JobExecutionSettings' {} a -> s {dataAccessRoleArn = a} :: JobExecutionSettings)

instance Prelude.FromJSON JobExecutionSettings where
  parseJSON =
    Prelude.withObject
      "JobExecutionSettings"
      ( \x ->
          JobExecutionSettings'
            Prelude.<$> (x Prelude..:? "AllowDeferredExecution")
            Prelude.<*> (x Prelude..:? "DataAccessRoleArn")
      )

instance Prelude.Hashable JobExecutionSettings

instance Prelude.NFData JobExecutionSettings

instance Prelude.ToJSON JobExecutionSettings where
  toJSON JobExecutionSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AllowDeferredExecution" Prelude..=)
              Prelude.<$> allowDeferredExecution,
            ("DataAccessRoleArn" Prelude..=)
              Prelude.<$> dataAccessRoleArn
          ]
      )
