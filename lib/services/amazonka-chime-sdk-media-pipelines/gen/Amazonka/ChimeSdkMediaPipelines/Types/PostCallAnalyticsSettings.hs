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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.PostCallAnalyticsSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.PostCallAnalyticsSettings where

import Amazonka.ChimeSdkMediaPipelines.Types.ContentRedactionOutput
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Allows you to specify additional settings for your Call Analytics
-- post-call request, including output locations for your redacted
-- transcript, which IAM role to use, and which encryption key to use.
--
-- @DataAccessRoleArn@ and @OutputLocation@ are required fields.
--
-- @PostCallAnalyticsSettings@ provides the same insights as a Call
-- Analytics post-call transcription. For more information, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tca-post-call.html Post-call analytics with real-time transcriptions>
-- in the /Amazon Transcribe Developer Guide/.
--
-- /See:/ 'newPostCallAnalyticsSettings' smart constructor.
data PostCallAnalyticsSettings = PostCallAnalyticsSettings'
  { -- | The content redaction output settings for a post-call analysis task.
    contentRedactionOutput :: Prelude.Maybe ContentRedactionOutput,
    -- | The ID of the KMS (Key Management Service) key used to encrypt the
    -- output.
    outputEncryptionKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | The URL of the Amazon S3 bucket that contains the post-call data.
    outputLocation :: Prelude.Text,
    -- | The ARN of the role used by Amazon Web Services Transcribe to upload
    -- your post call analysis. For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/tca-post-call.html Post-call analytics with real-time transcriptions>
    -- in the /Amazon Transcribe Developer Guide/.
    dataAccessRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostCallAnalyticsSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentRedactionOutput', 'postCallAnalyticsSettings_contentRedactionOutput' - The content redaction output settings for a post-call analysis task.
--
-- 'outputEncryptionKMSKeyId', 'postCallAnalyticsSettings_outputEncryptionKMSKeyId' - The ID of the KMS (Key Management Service) key used to encrypt the
-- output.
--
-- 'outputLocation', 'postCallAnalyticsSettings_outputLocation' - The URL of the Amazon S3 bucket that contains the post-call data.
--
-- 'dataAccessRoleArn', 'postCallAnalyticsSettings_dataAccessRoleArn' - The ARN of the role used by Amazon Web Services Transcribe to upload
-- your post call analysis. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tca-post-call.html Post-call analytics with real-time transcriptions>
-- in the /Amazon Transcribe Developer Guide/.
newPostCallAnalyticsSettings ::
  -- | 'outputLocation'
  Prelude.Text ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  PostCallAnalyticsSettings
newPostCallAnalyticsSettings
  pOutputLocation_
  pDataAccessRoleArn_ =
    PostCallAnalyticsSettings'
      { contentRedactionOutput =
          Prelude.Nothing,
        outputEncryptionKMSKeyId = Prelude.Nothing,
        outputLocation = pOutputLocation_,
        dataAccessRoleArn = pDataAccessRoleArn_
      }

-- | The content redaction output settings for a post-call analysis task.
postCallAnalyticsSettings_contentRedactionOutput :: Lens.Lens' PostCallAnalyticsSettings (Prelude.Maybe ContentRedactionOutput)
postCallAnalyticsSettings_contentRedactionOutput = Lens.lens (\PostCallAnalyticsSettings' {contentRedactionOutput} -> contentRedactionOutput) (\s@PostCallAnalyticsSettings' {} a -> s {contentRedactionOutput = a} :: PostCallAnalyticsSettings)

-- | The ID of the KMS (Key Management Service) key used to encrypt the
-- output.
postCallAnalyticsSettings_outputEncryptionKMSKeyId :: Lens.Lens' PostCallAnalyticsSettings (Prelude.Maybe Prelude.Text)
postCallAnalyticsSettings_outputEncryptionKMSKeyId = Lens.lens (\PostCallAnalyticsSettings' {outputEncryptionKMSKeyId} -> outputEncryptionKMSKeyId) (\s@PostCallAnalyticsSettings' {} a -> s {outputEncryptionKMSKeyId = a} :: PostCallAnalyticsSettings)

-- | The URL of the Amazon S3 bucket that contains the post-call data.
postCallAnalyticsSettings_outputLocation :: Lens.Lens' PostCallAnalyticsSettings Prelude.Text
postCallAnalyticsSettings_outputLocation = Lens.lens (\PostCallAnalyticsSettings' {outputLocation} -> outputLocation) (\s@PostCallAnalyticsSettings' {} a -> s {outputLocation = a} :: PostCallAnalyticsSettings)

-- | The ARN of the role used by Amazon Web Services Transcribe to upload
-- your post call analysis. For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tca-post-call.html Post-call analytics with real-time transcriptions>
-- in the /Amazon Transcribe Developer Guide/.
postCallAnalyticsSettings_dataAccessRoleArn :: Lens.Lens' PostCallAnalyticsSettings Prelude.Text
postCallAnalyticsSettings_dataAccessRoleArn = Lens.lens (\PostCallAnalyticsSettings' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@PostCallAnalyticsSettings' {} a -> s {dataAccessRoleArn = a} :: PostCallAnalyticsSettings)

instance Data.FromJSON PostCallAnalyticsSettings where
  parseJSON =
    Data.withObject
      "PostCallAnalyticsSettings"
      ( \x ->
          PostCallAnalyticsSettings'
            Prelude.<$> (x Data..:? "ContentRedactionOutput")
            Prelude.<*> (x Data..:? "OutputEncryptionKMSKeyId")
            Prelude.<*> (x Data..: "OutputLocation")
            Prelude.<*> (x Data..: "DataAccessRoleArn")
      )

instance Prelude.Hashable PostCallAnalyticsSettings where
  hashWithSalt _salt PostCallAnalyticsSettings' {..} =
    _salt
      `Prelude.hashWithSalt` contentRedactionOutput
      `Prelude.hashWithSalt` outputEncryptionKMSKeyId
      `Prelude.hashWithSalt` outputLocation
      `Prelude.hashWithSalt` dataAccessRoleArn

instance Prelude.NFData PostCallAnalyticsSettings where
  rnf PostCallAnalyticsSettings' {..} =
    Prelude.rnf contentRedactionOutput
      `Prelude.seq` Prelude.rnf outputEncryptionKMSKeyId
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf dataAccessRoleArn

instance Data.ToJSON PostCallAnalyticsSettings where
  toJSON PostCallAnalyticsSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContentRedactionOutput" Data..=)
              Prelude.<$> contentRedactionOutput,
            ("OutputEncryptionKMSKeyId" Data..=)
              Prelude.<$> outputEncryptionKMSKeyId,
            Prelude.Just
              ("OutputLocation" Data..= outputLocation),
            Prelude.Just
              ("DataAccessRoleArn" Data..= dataAccessRoleArn)
          ]
      )
