{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Transcribe.StartCallAnalyticsJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous analytics job that not only transcribes the audio
-- recording of a caller and agent, but also returns additional insights.
-- These insights include how quickly or loudly the caller or agent was
-- speaking. To retrieve additional insights with your analytics jobs,
-- create categories. A category is a way to classify analytics jobs based
-- on attributes, such as a customer\'s sentiment or a particular phrase
-- being used during the call. For more information, see the operation.
module Amazonka.Transcribe.StartCallAnalyticsJob
  ( -- * Creating a Request
    StartCallAnalyticsJob (..),
    newStartCallAnalyticsJob,

    -- * Request Lenses
    startCallAnalyticsJob_settings,
    startCallAnalyticsJob_outputEncryptionKMSKeyId,
    startCallAnalyticsJob_outputLocation,
    startCallAnalyticsJob_channelDefinitions,
    startCallAnalyticsJob_callAnalyticsJobName,
    startCallAnalyticsJob_media,
    startCallAnalyticsJob_dataAccessRoleArn,

    -- * Destructuring the Response
    StartCallAnalyticsJobResponse (..),
    newStartCallAnalyticsJobResponse,

    -- * Response Lenses
    startCallAnalyticsJobResponse_callAnalyticsJob,
    startCallAnalyticsJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newStartCallAnalyticsJob' smart constructor.
data StartCallAnalyticsJob = StartCallAnalyticsJob'
  { -- | A @Settings@ object that provides optional settings for a call analytics
    -- job.
    settings :: Prelude.Maybe CallAnalyticsJobSettings,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services Key Management
    -- Service key used to encrypt the output of the call analytics job. The
    -- user calling the operation must have permission to use the specified KMS
    -- key.
    --
    -- You use either of the following to identify an Amazon Web Services KMS
    -- key in the current account:
    --
    -- -   KMS Key ID: \"1234abcd-12ab-34cd-56ef-1234567890ab\"
    --
    -- -   KMS Key Alias: \"alias\/ExampleAlias\"
    --
    -- You can use either of the following to identify a KMS key in the current
    -- account or another account:
    --
    -- -   Amazon Resource Name (ARN) of a KMS key in the current account or
    --     another account: \"arn:aws:kms:region:account
    --     ID:key\/1234abcd-12ab-34cd-56ef1234567890ab\"
    --
    -- -   ARN of a KMS Key Alias: \"arn:aws:kms:region:account
    --     ID:alias\/ExampleAlias\"
    --
    -- If you don\'t specify an encryption key, the output of the call
    -- analytics job is encrypted with the default Amazon S3 key (SSE-S3).
    --
    -- If you specify a KMS key to encrypt your output, you must also specify
    -- an output location in the @OutputLocation@ parameter.
    outputEncryptionKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location where the output of the call analytics job is
    -- stored. You can provide the following location types to store the output
    -- of call analytics job:
    --
    -- -   s3:\/\/DOC-EXAMPLE-BUCKET1
    --
    --     If you specify a bucket, Amazon Transcribe saves the output of the
    --     analytics job as a JSON file at the root level of the bucket.
    --
    -- -   s3:\/\/DOC-EXAMPLE-BUCKET1\/folder\/
    --
    --     f you specify a path, Amazon Transcribe saves the output of the
    --     analytics job as
    --     s3:\/\/DOC-EXAMPLE-BUCKET1\/folder\/your-transcription-job-name.json
    --
    --     If you specify a folder, you must provide a trailing slash.
    --
    -- -   s3:\/\/DOC-EXAMPLE-BUCKET1\/folder\/filename.json
    --
    --     If you provide a path that has the filename specified, Amazon
    --     Transcribe saves the output of the analytics job as
    --     s3:\/\/DOC-EXAMPLEBUCKET1\/folder\/filename.json
    --
    -- You can specify an Amazon Web Services Key Management Service (KMS) key
    -- to encrypt the output of our analytics job using the
    -- @OutputEncryptionKMSKeyId@ parameter. If you don\'t specify a KMS key,
    -- Amazon Transcribe uses the default Amazon S3 key for server-side
    -- encryption of the analytics job output that is placed in your S3 bucket.
    outputLocation :: Prelude.Maybe Prelude.Text,
    -- | When you start a call analytics job, you must pass an array that maps
    -- the agent and the customer to specific audio channels. The values you
    -- can assign to a channel are 0 and 1. The agent and the customer must
    -- each have their own channel. You can\'t assign more than one channel to
    -- an agent or customer.
    channelDefinitions :: Prelude.Maybe (Prelude.NonEmpty ChannelDefinition),
    -- | The name of the call analytics job. You can\'t use the string \".\" or
    -- \"..\" by themselves as the job name. The name must also be unique
    -- within an Amazon Web Services account. If you try to create a call
    -- analytics job with the same name as a previous call analytics job, you
    -- get a @ConflictException@ error.
    callAnalyticsJobName :: Prelude.Text,
    media :: Media,
    -- | The Amazon Resource Name (ARN) of a role that has access to the S3
    -- bucket that contains your input files. Amazon Transcribe assumes this
    -- role to read queued audio files. If you have specified an output S3
    -- bucket for your transcription results, this role should have access to
    -- the output bucket as well.
    dataAccessRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCallAnalyticsJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settings', 'startCallAnalyticsJob_settings' - A @Settings@ object that provides optional settings for a call analytics
-- job.
--
-- 'outputEncryptionKMSKeyId', 'startCallAnalyticsJob_outputEncryptionKMSKeyId' - The Amazon Resource Name (ARN) of the Amazon Web Services Key Management
-- Service key used to encrypt the output of the call analytics job. The
-- user calling the operation must have permission to use the specified KMS
-- key.
--
-- You use either of the following to identify an Amazon Web Services KMS
-- key in the current account:
--
-- -   KMS Key ID: \"1234abcd-12ab-34cd-56ef-1234567890ab\"
--
-- -   KMS Key Alias: \"alias\/ExampleAlias\"
--
-- You can use either of the following to identify a KMS key in the current
-- account or another account:
--
-- -   Amazon Resource Name (ARN) of a KMS key in the current account or
--     another account: \"arn:aws:kms:region:account
--     ID:key\/1234abcd-12ab-34cd-56ef1234567890ab\"
--
-- -   ARN of a KMS Key Alias: \"arn:aws:kms:region:account
--     ID:alias\/ExampleAlias\"
--
-- If you don\'t specify an encryption key, the output of the call
-- analytics job is encrypted with the default Amazon S3 key (SSE-S3).
--
-- If you specify a KMS key to encrypt your output, you must also specify
-- an output location in the @OutputLocation@ parameter.
--
-- 'outputLocation', 'startCallAnalyticsJob_outputLocation' - The Amazon S3 location where the output of the call analytics job is
-- stored. You can provide the following location types to store the output
-- of call analytics job:
--
-- -   s3:\/\/DOC-EXAMPLE-BUCKET1
--
--     If you specify a bucket, Amazon Transcribe saves the output of the
--     analytics job as a JSON file at the root level of the bucket.
--
-- -   s3:\/\/DOC-EXAMPLE-BUCKET1\/folder\/
--
--     f you specify a path, Amazon Transcribe saves the output of the
--     analytics job as
--     s3:\/\/DOC-EXAMPLE-BUCKET1\/folder\/your-transcription-job-name.json
--
--     If you specify a folder, you must provide a trailing slash.
--
-- -   s3:\/\/DOC-EXAMPLE-BUCKET1\/folder\/filename.json
--
--     If you provide a path that has the filename specified, Amazon
--     Transcribe saves the output of the analytics job as
--     s3:\/\/DOC-EXAMPLEBUCKET1\/folder\/filename.json
--
-- You can specify an Amazon Web Services Key Management Service (KMS) key
-- to encrypt the output of our analytics job using the
-- @OutputEncryptionKMSKeyId@ parameter. If you don\'t specify a KMS key,
-- Amazon Transcribe uses the default Amazon S3 key for server-side
-- encryption of the analytics job output that is placed in your S3 bucket.
--
-- 'channelDefinitions', 'startCallAnalyticsJob_channelDefinitions' - When you start a call analytics job, you must pass an array that maps
-- the agent and the customer to specific audio channels. The values you
-- can assign to a channel are 0 and 1. The agent and the customer must
-- each have their own channel. You can\'t assign more than one channel to
-- an agent or customer.
--
-- 'callAnalyticsJobName', 'startCallAnalyticsJob_callAnalyticsJobName' - The name of the call analytics job. You can\'t use the string \".\" or
-- \"..\" by themselves as the job name. The name must also be unique
-- within an Amazon Web Services account. If you try to create a call
-- analytics job with the same name as a previous call analytics job, you
-- get a @ConflictException@ error.
--
-- 'media', 'startCallAnalyticsJob_media' - Undocumented member.
--
-- 'dataAccessRoleArn', 'startCallAnalyticsJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of a role that has access to the S3
-- bucket that contains your input files. Amazon Transcribe assumes this
-- role to read queued audio files. If you have specified an output S3
-- bucket for your transcription results, this role should have access to
-- the output bucket as well.
newStartCallAnalyticsJob ::
  -- | 'callAnalyticsJobName'
  Prelude.Text ->
  -- | 'media'
  Media ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  StartCallAnalyticsJob
newStartCallAnalyticsJob
  pCallAnalyticsJobName_
  pMedia_
  pDataAccessRoleArn_ =
    StartCallAnalyticsJob'
      { settings = Prelude.Nothing,
        outputEncryptionKMSKeyId = Prelude.Nothing,
        outputLocation = Prelude.Nothing,
        channelDefinitions = Prelude.Nothing,
        callAnalyticsJobName = pCallAnalyticsJobName_,
        media = pMedia_,
        dataAccessRoleArn = pDataAccessRoleArn_
      }

-- | A @Settings@ object that provides optional settings for a call analytics
-- job.
startCallAnalyticsJob_settings :: Lens.Lens' StartCallAnalyticsJob (Prelude.Maybe CallAnalyticsJobSettings)
startCallAnalyticsJob_settings = Lens.lens (\StartCallAnalyticsJob' {settings} -> settings) (\s@StartCallAnalyticsJob' {} a -> s {settings = a} :: StartCallAnalyticsJob)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Key Management
-- Service key used to encrypt the output of the call analytics job. The
-- user calling the operation must have permission to use the specified KMS
-- key.
--
-- You use either of the following to identify an Amazon Web Services KMS
-- key in the current account:
--
-- -   KMS Key ID: \"1234abcd-12ab-34cd-56ef-1234567890ab\"
--
-- -   KMS Key Alias: \"alias\/ExampleAlias\"
--
-- You can use either of the following to identify a KMS key in the current
-- account or another account:
--
-- -   Amazon Resource Name (ARN) of a KMS key in the current account or
--     another account: \"arn:aws:kms:region:account
--     ID:key\/1234abcd-12ab-34cd-56ef1234567890ab\"
--
-- -   ARN of a KMS Key Alias: \"arn:aws:kms:region:account
--     ID:alias\/ExampleAlias\"
--
-- If you don\'t specify an encryption key, the output of the call
-- analytics job is encrypted with the default Amazon S3 key (SSE-S3).
--
-- If you specify a KMS key to encrypt your output, you must also specify
-- an output location in the @OutputLocation@ parameter.
startCallAnalyticsJob_outputEncryptionKMSKeyId :: Lens.Lens' StartCallAnalyticsJob (Prelude.Maybe Prelude.Text)
startCallAnalyticsJob_outputEncryptionKMSKeyId = Lens.lens (\StartCallAnalyticsJob' {outputEncryptionKMSKeyId} -> outputEncryptionKMSKeyId) (\s@StartCallAnalyticsJob' {} a -> s {outputEncryptionKMSKeyId = a} :: StartCallAnalyticsJob)

-- | The Amazon S3 location where the output of the call analytics job is
-- stored. You can provide the following location types to store the output
-- of call analytics job:
--
-- -   s3:\/\/DOC-EXAMPLE-BUCKET1
--
--     If you specify a bucket, Amazon Transcribe saves the output of the
--     analytics job as a JSON file at the root level of the bucket.
--
-- -   s3:\/\/DOC-EXAMPLE-BUCKET1\/folder\/
--
--     f you specify a path, Amazon Transcribe saves the output of the
--     analytics job as
--     s3:\/\/DOC-EXAMPLE-BUCKET1\/folder\/your-transcription-job-name.json
--
--     If you specify a folder, you must provide a trailing slash.
--
-- -   s3:\/\/DOC-EXAMPLE-BUCKET1\/folder\/filename.json
--
--     If you provide a path that has the filename specified, Amazon
--     Transcribe saves the output of the analytics job as
--     s3:\/\/DOC-EXAMPLEBUCKET1\/folder\/filename.json
--
-- You can specify an Amazon Web Services Key Management Service (KMS) key
-- to encrypt the output of our analytics job using the
-- @OutputEncryptionKMSKeyId@ parameter. If you don\'t specify a KMS key,
-- Amazon Transcribe uses the default Amazon S3 key for server-side
-- encryption of the analytics job output that is placed in your S3 bucket.
startCallAnalyticsJob_outputLocation :: Lens.Lens' StartCallAnalyticsJob (Prelude.Maybe Prelude.Text)
startCallAnalyticsJob_outputLocation = Lens.lens (\StartCallAnalyticsJob' {outputLocation} -> outputLocation) (\s@StartCallAnalyticsJob' {} a -> s {outputLocation = a} :: StartCallAnalyticsJob)

-- | When you start a call analytics job, you must pass an array that maps
-- the agent and the customer to specific audio channels. The values you
-- can assign to a channel are 0 and 1. The agent and the customer must
-- each have their own channel. You can\'t assign more than one channel to
-- an agent or customer.
startCallAnalyticsJob_channelDefinitions :: Lens.Lens' StartCallAnalyticsJob (Prelude.Maybe (Prelude.NonEmpty ChannelDefinition))
startCallAnalyticsJob_channelDefinitions = Lens.lens (\StartCallAnalyticsJob' {channelDefinitions} -> channelDefinitions) (\s@StartCallAnalyticsJob' {} a -> s {channelDefinitions = a} :: StartCallAnalyticsJob) Prelude.. Lens.mapping Lens.coerced

-- | The name of the call analytics job. You can\'t use the string \".\" or
-- \"..\" by themselves as the job name. The name must also be unique
-- within an Amazon Web Services account. If you try to create a call
-- analytics job with the same name as a previous call analytics job, you
-- get a @ConflictException@ error.
startCallAnalyticsJob_callAnalyticsJobName :: Lens.Lens' StartCallAnalyticsJob Prelude.Text
startCallAnalyticsJob_callAnalyticsJobName = Lens.lens (\StartCallAnalyticsJob' {callAnalyticsJobName} -> callAnalyticsJobName) (\s@StartCallAnalyticsJob' {} a -> s {callAnalyticsJobName = a} :: StartCallAnalyticsJob)

-- | Undocumented member.
startCallAnalyticsJob_media :: Lens.Lens' StartCallAnalyticsJob Media
startCallAnalyticsJob_media = Lens.lens (\StartCallAnalyticsJob' {media} -> media) (\s@StartCallAnalyticsJob' {} a -> s {media = a} :: StartCallAnalyticsJob)

-- | The Amazon Resource Name (ARN) of a role that has access to the S3
-- bucket that contains your input files. Amazon Transcribe assumes this
-- role to read queued audio files. If you have specified an output S3
-- bucket for your transcription results, this role should have access to
-- the output bucket as well.
startCallAnalyticsJob_dataAccessRoleArn :: Lens.Lens' StartCallAnalyticsJob Prelude.Text
startCallAnalyticsJob_dataAccessRoleArn = Lens.lens (\StartCallAnalyticsJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartCallAnalyticsJob' {} a -> s {dataAccessRoleArn = a} :: StartCallAnalyticsJob)

instance Core.AWSRequest StartCallAnalyticsJob where
  type
    AWSResponse StartCallAnalyticsJob =
      StartCallAnalyticsJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartCallAnalyticsJobResponse'
            Prelude.<$> (x Core..?> "CallAnalyticsJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartCallAnalyticsJob where
  hashWithSalt _salt StartCallAnalyticsJob' {..} =
    _salt `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` outputEncryptionKMSKeyId
      `Prelude.hashWithSalt` outputLocation
      `Prelude.hashWithSalt` channelDefinitions
      `Prelude.hashWithSalt` callAnalyticsJobName
      `Prelude.hashWithSalt` media
      `Prelude.hashWithSalt` dataAccessRoleArn

instance Prelude.NFData StartCallAnalyticsJob where
  rnf StartCallAnalyticsJob' {..} =
    Prelude.rnf settings
      `Prelude.seq` Prelude.rnf outputEncryptionKMSKeyId
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf channelDefinitions
      `Prelude.seq` Prelude.rnf callAnalyticsJobName
      `Prelude.seq` Prelude.rnf media
      `Prelude.seq` Prelude.rnf dataAccessRoleArn

instance Core.ToHeaders StartCallAnalyticsJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.StartCallAnalyticsJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartCallAnalyticsJob where
  toJSON StartCallAnalyticsJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Settings" Core..=) Prelude.<$> settings,
            ("OutputEncryptionKMSKeyId" Core..=)
              Prelude.<$> outputEncryptionKMSKeyId,
            ("OutputLocation" Core..=)
              Prelude.<$> outputLocation,
            ("ChannelDefinitions" Core..=)
              Prelude.<$> channelDefinitions,
            Prelude.Just
              ( "CallAnalyticsJobName"
                  Core..= callAnalyticsJobName
              ),
            Prelude.Just ("Media" Core..= media),
            Prelude.Just
              ("DataAccessRoleArn" Core..= dataAccessRoleArn)
          ]
      )

instance Core.ToPath StartCallAnalyticsJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StartCallAnalyticsJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartCallAnalyticsJobResponse' smart constructor.
data StartCallAnalyticsJobResponse = StartCallAnalyticsJobResponse'
  { -- | An object containing the details of the asynchronous call analytics job.
    callAnalyticsJob :: Prelude.Maybe CallAnalyticsJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCallAnalyticsJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callAnalyticsJob', 'startCallAnalyticsJobResponse_callAnalyticsJob' - An object containing the details of the asynchronous call analytics job.
--
-- 'httpStatus', 'startCallAnalyticsJobResponse_httpStatus' - The response's http status code.
newStartCallAnalyticsJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartCallAnalyticsJobResponse
newStartCallAnalyticsJobResponse pHttpStatus_ =
  StartCallAnalyticsJobResponse'
    { callAnalyticsJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object containing the details of the asynchronous call analytics job.
startCallAnalyticsJobResponse_callAnalyticsJob :: Lens.Lens' StartCallAnalyticsJobResponse (Prelude.Maybe CallAnalyticsJob)
startCallAnalyticsJobResponse_callAnalyticsJob = Lens.lens (\StartCallAnalyticsJobResponse' {callAnalyticsJob} -> callAnalyticsJob) (\s@StartCallAnalyticsJobResponse' {} a -> s {callAnalyticsJob = a} :: StartCallAnalyticsJobResponse)

-- | The response's http status code.
startCallAnalyticsJobResponse_httpStatus :: Lens.Lens' StartCallAnalyticsJobResponse Prelude.Int
startCallAnalyticsJobResponse_httpStatus = Lens.lens (\StartCallAnalyticsJobResponse' {httpStatus} -> httpStatus) (\s@StartCallAnalyticsJobResponse' {} a -> s {httpStatus = a} :: StartCallAnalyticsJobResponse)

instance Prelude.NFData StartCallAnalyticsJobResponse where
  rnf StartCallAnalyticsJobResponse' {..} =
    Prelude.rnf callAnalyticsJob
      `Prelude.seq` Prelude.rnf httpStatus
