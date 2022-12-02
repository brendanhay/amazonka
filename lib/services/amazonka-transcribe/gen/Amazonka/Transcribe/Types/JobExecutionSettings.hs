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
-- Module      : Amazonka.Transcribe.Types.JobExecutionSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.JobExecutionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Allows you to control how your transcription job is processed.
-- Currently, the only @JobExecutionSettings@ modification you can choose
-- is enabling job queueing using the @AllowDeferredExecution@
-- sub-parameter.
--
-- If you include @JobExecutionSettings@ in your request, you must also
-- include the sub-parameters: @AllowDeferredExecution@ and
-- @DataAccessRoleArn@.
--
-- /See:/ 'newJobExecutionSettings' smart constructor.
data JobExecutionSettings = JobExecutionSettings'
  { -- | Allows you to enable job queuing when your concurrent request limit is
    -- exceeded. When @AllowDeferredExecution@ is set to @true@, transcription
    -- job requests are placed in a queue until the number of jobs falls below
    -- the concurrent request limit. If @AllowDeferredExecution@ is set to
    -- @false@ and the number of transcription job requests exceed the
    -- concurrent request limit, you get a @LimitExceededException@ error.
    --
    -- Note that job queuing is enabled by default for Call Analytics jobs.
    --
    -- If you include @AllowDeferredExecution@ in your request, you must also
    -- include @DataAccessRoleArn@.
    allowDeferredExecution :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of an IAM role that has permissions to
    -- access the Amazon S3 bucket that contains your input files. If the role
    -- you specify doesn’t have the appropriate permissions to access the
    -- specified Amazon S3 location, your request fails.
    --
    -- IAM role ARNs have the format
    -- @arn:partition:iam::account:role\/role-name-with-path@. For example:
    -- @arn:aws:iam::111122223333:role\/Admin@. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-arns IAM ARNs>.
    --
    -- Note that if you include @DataAccessRoleArn@ in your request, you must
    -- also include @AllowDeferredExecution@.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobExecutionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowDeferredExecution', 'jobExecutionSettings_allowDeferredExecution' - Allows you to enable job queuing when your concurrent request limit is
-- exceeded. When @AllowDeferredExecution@ is set to @true@, transcription
-- job requests are placed in a queue until the number of jobs falls below
-- the concurrent request limit. If @AllowDeferredExecution@ is set to
-- @false@ and the number of transcription job requests exceed the
-- concurrent request limit, you get a @LimitExceededException@ error.
--
-- Note that job queuing is enabled by default for Call Analytics jobs.
--
-- If you include @AllowDeferredExecution@ in your request, you must also
-- include @DataAccessRoleArn@.
--
-- 'dataAccessRoleArn', 'jobExecutionSettings_dataAccessRoleArn' - The Amazon Resource Name (ARN) of an IAM role that has permissions to
-- access the Amazon S3 bucket that contains your input files. If the role
-- you specify doesn’t have the appropriate permissions to access the
-- specified Amazon S3 location, your request fails.
--
-- IAM role ARNs have the format
-- @arn:partition:iam::account:role\/role-name-with-path@. For example:
-- @arn:aws:iam::111122223333:role\/Admin@. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-arns IAM ARNs>.
--
-- Note that if you include @DataAccessRoleArn@ in your request, you must
-- also include @AllowDeferredExecution@.
newJobExecutionSettings ::
  JobExecutionSettings
newJobExecutionSettings =
  JobExecutionSettings'
    { allowDeferredExecution =
        Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing
    }

-- | Allows you to enable job queuing when your concurrent request limit is
-- exceeded. When @AllowDeferredExecution@ is set to @true@, transcription
-- job requests are placed in a queue until the number of jobs falls below
-- the concurrent request limit. If @AllowDeferredExecution@ is set to
-- @false@ and the number of transcription job requests exceed the
-- concurrent request limit, you get a @LimitExceededException@ error.
--
-- Note that job queuing is enabled by default for Call Analytics jobs.
--
-- If you include @AllowDeferredExecution@ in your request, you must also
-- include @DataAccessRoleArn@.
jobExecutionSettings_allowDeferredExecution :: Lens.Lens' JobExecutionSettings (Prelude.Maybe Prelude.Bool)
jobExecutionSettings_allowDeferredExecution = Lens.lens (\JobExecutionSettings' {allowDeferredExecution} -> allowDeferredExecution) (\s@JobExecutionSettings' {} a -> s {allowDeferredExecution = a} :: JobExecutionSettings)

-- | The Amazon Resource Name (ARN) of an IAM role that has permissions to
-- access the Amazon S3 bucket that contains your input files. If the role
-- you specify doesn’t have the appropriate permissions to access the
-- specified Amazon S3 location, your request fails.
--
-- IAM role ARNs have the format
-- @arn:partition:iam::account:role\/role-name-with-path@. For example:
-- @arn:aws:iam::111122223333:role\/Admin@. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-arns IAM ARNs>.
--
-- Note that if you include @DataAccessRoleArn@ in your request, you must
-- also include @AllowDeferredExecution@.
jobExecutionSettings_dataAccessRoleArn :: Lens.Lens' JobExecutionSettings (Prelude.Maybe Prelude.Text)
jobExecutionSettings_dataAccessRoleArn = Lens.lens (\JobExecutionSettings' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@JobExecutionSettings' {} a -> s {dataAccessRoleArn = a} :: JobExecutionSettings)

instance Data.FromJSON JobExecutionSettings where
  parseJSON =
    Data.withObject
      "JobExecutionSettings"
      ( \x ->
          JobExecutionSettings'
            Prelude.<$> (x Data..:? "AllowDeferredExecution")
            Prelude.<*> (x Data..:? "DataAccessRoleArn")
      )

instance Prelude.Hashable JobExecutionSettings where
  hashWithSalt _salt JobExecutionSettings' {..} =
    _salt `Prelude.hashWithSalt` allowDeferredExecution
      `Prelude.hashWithSalt` dataAccessRoleArn

instance Prelude.NFData JobExecutionSettings where
  rnf JobExecutionSettings' {..} =
    Prelude.rnf allowDeferredExecution
      `Prelude.seq` Prelude.rnf dataAccessRoleArn

instance Data.ToJSON JobExecutionSettings where
  toJSON JobExecutionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowDeferredExecution" Data..=)
              Prelude.<$> allowDeferredExecution,
            ("DataAccessRoleArn" Data..=)
              Prelude.<$> dataAccessRoleArn
          ]
      )
