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
-- Module      : Amazonka.Rekognition.CreateStreamProcessor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Rekognition stream processor that you can use to
-- detect and recognize faces or to detect labels in a streaming video.
--
-- Amazon Rekognition Video is a consumer of live video from Amazon Kinesis
-- Video Streams. There are two different settings for stream processors in
-- Amazon Rekognition: detecting faces and detecting labels.
--
-- -   If you are creating a stream processor for detecting faces, you
--     provide as input a Kinesis video stream (@Input@) and a Kinesis data
--     stream (@Output@) stream. You also specify the face recognition
--     criteria in @Settings@. For example, the collection containing faces
--     that you want to recognize. After you have finished analyzing a
--     streaming video, use StopStreamProcessor to stop processing.
--
-- -   If you are creating a stream processor to detect labels, you provide
--     as input a Kinesis video stream (@Input@), Amazon S3 bucket
--     information (@Output@), and an Amazon SNS topic ARN
--     (@NotificationChannel@). You can also provide a KMS key ID to
--     encrypt the data sent to your Amazon S3 bucket. You specify what you
--     want to detect in @ConnectedHomeSettings@, such as people, packages
--     and people, or pets, people, and packages. You can also specify
--     where in the frame you want Amazon Rekognition to monitor with
--     @RegionsOfInterest@. When you run the StartStreamProcessor operation
--     on a label detection stream processor, you input start and stop
--     information to determine the length of the processing time.
--
-- Use @Name@ to assign an identifier for the stream processor. You use
-- @Name@ to manage the stream processor. For example, you can start
-- processing the source video by calling StartStreamProcessor with the
-- @Name@ field.
--
-- This operation requires permissions to perform the
-- @rekognition:CreateStreamProcessor@ action. If you want to tag your
-- stream processor, you also require permission to perform the
-- @rekognition:TagResource@ operation.
module Amazonka.Rekognition.CreateStreamProcessor
  ( -- * Creating a Request
    CreateStreamProcessor (..),
    newCreateStreamProcessor,

    -- * Request Lenses
    createStreamProcessor_tags,
    createStreamProcessor_regionsOfInterest,
    createStreamProcessor_kmsKeyId,
    createStreamProcessor_dataSharingPreference,
    createStreamProcessor_notificationChannel,
    createStreamProcessor_input,
    createStreamProcessor_output,
    createStreamProcessor_name,
    createStreamProcessor_settings,
    createStreamProcessor_roleArn,

    -- * Destructuring the Response
    CreateStreamProcessorResponse (..),
    newCreateStreamProcessorResponse,

    -- * Response Lenses
    createStreamProcessorResponse_streamProcessorArn,
    createStreamProcessorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStreamProcessor' smart constructor.
data CreateStreamProcessor = CreateStreamProcessor'
  { -- | A set of tags (key-value pairs) that you want to attach to the stream
    -- processor.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies locations in the frames where Amazon Rekognition checks for
    -- objects or people. You can specify up to 10 regions of interest, and
    -- each region has either a polygon or a bounding box. This is an optional
    -- parameter for label detection stream processors and should not be used
    -- to create a face search stream processor.
    regionsOfInterest :: Prelude.Maybe [RegionOfInterest],
    -- | The identifier for your AWS Key Management Service key (AWS KMS key).
    -- This is an optional parameter for label detection stream processors and
    -- should not be used to create a face search stream processor. You can
    -- supply the Amazon Resource Name (ARN) of your KMS key, the ID of your
    -- KMS key, an alias for your KMS key, or an alias ARN. The key is used to
    -- encrypt results and data published to your Amazon S3 bucket, which
    -- includes image frames and hero images. Your source images are
    -- unaffected.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Shows whether you are sharing data with Rekognition to improve model
    -- performance. You can choose this option at the account level or on a
    -- per-stream basis. Note that if you opt out at the account level this
    -- setting is ignored on individual streams.
    dataSharingPreference :: Prelude.Maybe StreamProcessorDataSharingPreference,
    notificationChannel :: Prelude.Maybe StreamProcessorNotificationChannel,
    -- | Kinesis video stream stream that provides the source streaming video. If
    -- you are using the AWS CLI, the parameter name is @StreamProcessorInput@.
    -- This is required for both face search and label detection stream
    -- processors.
    input :: StreamProcessorInput,
    -- | Kinesis data stream stream or Amazon S3 bucket location to which Amazon
    -- Rekognition Video puts the analysis results. If you are using the AWS
    -- CLI, the parameter name is @StreamProcessorOutput@. This must be a
    -- S3Destination of an Amazon S3 bucket that you own for a label detection
    -- stream processor or a Kinesis data stream ARN for a face search stream
    -- processor.
    output :: StreamProcessorOutput,
    -- | An identifier you assign to the stream processor. You can use @Name@ to
    -- manage the stream processor. For example, you can get the current status
    -- of the stream processor by calling DescribeStreamProcessor. @Name@ is
    -- idempotent. This is required for both face search and label detection
    -- stream processors.
    name :: Prelude.Text,
    -- | Input parameters used in a streaming video analyzed by a stream
    -- processor. You can use @FaceSearch@ to recognize faces in a streaming
    -- video, or you can use @ConnectedHome@ to detect labels.
    settings :: StreamProcessorSettings,
    -- | The Amazon Resource Number (ARN) of the IAM role that allows access to
    -- the stream processor. The IAM role provides Rekognition read permissions
    -- for a Kinesis stream. It also provides write permissions to an Amazon S3
    -- bucket and Amazon Simple Notification Service topic for a label
    -- detection stream processor. This is required for both face search and
    -- label detection stream processors.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamProcessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createStreamProcessor_tags' - A set of tags (key-value pairs) that you want to attach to the stream
-- processor.
--
-- 'regionsOfInterest', 'createStreamProcessor_regionsOfInterest' - Specifies locations in the frames where Amazon Rekognition checks for
-- objects or people. You can specify up to 10 regions of interest, and
-- each region has either a polygon or a bounding box. This is an optional
-- parameter for label detection stream processors and should not be used
-- to create a face search stream processor.
--
-- 'kmsKeyId', 'createStreamProcessor_kmsKeyId' - The identifier for your AWS Key Management Service key (AWS KMS key).
-- This is an optional parameter for label detection stream processors and
-- should not be used to create a face search stream processor. You can
-- supply the Amazon Resource Name (ARN) of your KMS key, the ID of your
-- KMS key, an alias for your KMS key, or an alias ARN. The key is used to
-- encrypt results and data published to your Amazon S3 bucket, which
-- includes image frames and hero images. Your source images are
-- unaffected.
--
-- 'dataSharingPreference', 'createStreamProcessor_dataSharingPreference' - Shows whether you are sharing data with Rekognition to improve model
-- performance. You can choose this option at the account level or on a
-- per-stream basis. Note that if you opt out at the account level this
-- setting is ignored on individual streams.
--
-- 'notificationChannel', 'createStreamProcessor_notificationChannel' - Undocumented member.
--
-- 'input', 'createStreamProcessor_input' - Kinesis video stream stream that provides the source streaming video. If
-- you are using the AWS CLI, the parameter name is @StreamProcessorInput@.
-- This is required for both face search and label detection stream
-- processors.
--
-- 'output', 'createStreamProcessor_output' - Kinesis data stream stream or Amazon S3 bucket location to which Amazon
-- Rekognition Video puts the analysis results. If you are using the AWS
-- CLI, the parameter name is @StreamProcessorOutput@. This must be a
-- S3Destination of an Amazon S3 bucket that you own for a label detection
-- stream processor or a Kinesis data stream ARN for a face search stream
-- processor.
--
-- 'name', 'createStreamProcessor_name' - An identifier you assign to the stream processor. You can use @Name@ to
-- manage the stream processor. For example, you can get the current status
-- of the stream processor by calling DescribeStreamProcessor. @Name@ is
-- idempotent. This is required for both face search and label detection
-- stream processors.
--
-- 'settings', 'createStreamProcessor_settings' - Input parameters used in a streaming video analyzed by a stream
-- processor. You can use @FaceSearch@ to recognize faces in a streaming
-- video, or you can use @ConnectedHome@ to detect labels.
--
-- 'roleArn', 'createStreamProcessor_roleArn' - The Amazon Resource Number (ARN) of the IAM role that allows access to
-- the stream processor. The IAM role provides Rekognition read permissions
-- for a Kinesis stream. It also provides write permissions to an Amazon S3
-- bucket and Amazon Simple Notification Service topic for a label
-- detection stream processor. This is required for both face search and
-- label detection stream processors.
newCreateStreamProcessor ::
  -- | 'input'
  StreamProcessorInput ->
  -- | 'output'
  StreamProcessorOutput ->
  -- | 'name'
  Prelude.Text ->
  -- | 'settings'
  StreamProcessorSettings ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateStreamProcessor
newCreateStreamProcessor
  pInput_
  pOutput_
  pName_
  pSettings_
  pRoleArn_ =
    CreateStreamProcessor'
      { tags = Prelude.Nothing,
        regionsOfInterest = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        dataSharingPreference = Prelude.Nothing,
        notificationChannel = Prelude.Nothing,
        input = pInput_,
        output = pOutput_,
        name = pName_,
        settings = pSettings_,
        roleArn = pRoleArn_
      }

-- | A set of tags (key-value pairs) that you want to attach to the stream
-- processor.
createStreamProcessor_tags :: Lens.Lens' CreateStreamProcessor (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStreamProcessor_tags = Lens.lens (\CreateStreamProcessor' {tags} -> tags) (\s@CreateStreamProcessor' {} a -> s {tags = a} :: CreateStreamProcessor) Prelude.. Lens.mapping Lens.coerced

-- | Specifies locations in the frames where Amazon Rekognition checks for
-- objects or people. You can specify up to 10 regions of interest, and
-- each region has either a polygon or a bounding box. This is an optional
-- parameter for label detection stream processors and should not be used
-- to create a face search stream processor.
createStreamProcessor_regionsOfInterest :: Lens.Lens' CreateStreamProcessor (Prelude.Maybe [RegionOfInterest])
createStreamProcessor_regionsOfInterest = Lens.lens (\CreateStreamProcessor' {regionsOfInterest} -> regionsOfInterest) (\s@CreateStreamProcessor' {} a -> s {regionsOfInterest = a} :: CreateStreamProcessor) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for your AWS Key Management Service key (AWS KMS key).
-- This is an optional parameter for label detection stream processors and
-- should not be used to create a face search stream processor. You can
-- supply the Amazon Resource Name (ARN) of your KMS key, the ID of your
-- KMS key, an alias for your KMS key, or an alias ARN. The key is used to
-- encrypt results and data published to your Amazon S3 bucket, which
-- includes image frames and hero images. Your source images are
-- unaffected.
createStreamProcessor_kmsKeyId :: Lens.Lens' CreateStreamProcessor (Prelude.Maybe Prelude.Text)
createStreamProcessor_kmsKeyId = Lens.lens (\CreateStreamProcessor' {kmsKeyId} -> kmsKeyId) (\s@CreateStreamProcessor' {} a -> s {kmsKeyId = a} :: CreateStreamProcessor)

-- | Shows whether you are sharing data with Rekognition to improve model
-- performance. You can choose this option at the account level or on a
-- per-stream basis. Note that if you opt out at the account level this
-- setting is ignored on individual streams.
createStreamProcessor_dataSharingPreference :: Lens.Lens' CreateStreamProcessor (Prelude.Maybe StreamProcessorDataSharingPreference)
createStreamProcessor_dataSharingPreference = Lens.lens (\CreateStreamProcessor' {dataSharingPreference} -> dataSharingPreference) (\s@CreateStreamProcessor' {} a -> s {dataSharingPreference = a} :: CreateStreamProcessor)

-- | Undocumented member.
createStreamProcessor_notificationChannel :: Lens.Lens' CreateStreamProcessor (Prelude.Maybe StreamProcessorNotificationChannel)
createStreamProcessor_notificationChannel = Lens.lens (\CreateStreamProcessor' {notificationChannel} -> notificationChannel) (\s@CreateStreamProcessor' {} a -> s {notificationChannel = a} :: CreateStreamProcessor)

-- | Kinesis video stream stream that provides the source streaming video. If
-- you are using the AWS CLI, the parameter name is @StreamProcessorInput@.
-- This is required for both face search and label detection stream
-- processors.
createStreamProcessor_input :: Lens.Lens' CreateStreamProcessor StreamProcessorInput
createStreamProcessor_input = Lens.lens (\CreateStreamProcessor' {input} -> input) (\s@CreateStreamProcessor' {} a -> s {input = a} :: CreateStreamProcessor)

-- | Kinesis data stream stream or Amazon S3 bucket location to which Amazon
-- Rekognition Video puts the analysis results. If you are using the AWS
-- CLI, the parameter name is @StreamProcessorOutput@. This must be a
-- S3Destination of an Amazon S3 bucket that you own for a label detection
-- stream processor or a Kinesis data stream ARN for a face search stream
-- processor.
createStreamProcessor_output :: Lens.Lens' CreateStreamProcessor StreamProcessorOutput
createStreamProcessor_output = Lens.lens (\CreateStreamProcessor' {output} -> output) (\s@CreateStreamProcessor' {} a -> s {output = a} :: CreateStreamProcessor)

-- | An identifier you assign to the stream processor. You can use @Name@ to
-- manage the stream processor. For example, you can get the current status
-- of the stream processor by calling DescribeStreamProcessor. @Name@ is
-- idempotent. This is required for both face search and label detection
-- stream processors.
createStreamProcessor_name :: Lens.Lens' CreateStreamProcessor Prelude.Text
createStreamProcessor_name = Lens.lens (\CreateStreamProcessor' {name} -> name) (\s@CreateStreamProcessor' {} a -> s {name = a} :: CreateStreamProcessor)

-- | Input parameters used in a streaming video analyzed by a stream
-- processor. You can use @FaceSearch@ to recognize faces in a streaming
-- video, or you can use @ConnectedHome@ to detect labels.
createStreamProcessor_settings :: Lens.Lens' CreateStreamProcessor StreamProcessorSettings
createStreamProcessor_settings = Lens.lens (\CreateStreamProcessor' {settings} -> settings) (\s@CreateStreamProcessor' {} a -> s {settings = a} :: CreateStreamProcessor)

-- | The Amazon Resource Number (ARN) of the IAM role that allows access to
-- the stream processor. The IAM role provides Rekognition read permissions
-- for a Kinesis stream. It also provides write permissions to an Amazon S3
-- bucket and Amazon Simple Notification Service topic for a label
-- detection stream processor. This is required for both face search and
-- label detection stream processors.
createStreamProcessor_roleArn :: Lens.Lens' CreateStreamProcessor Prelude.Text
createStreamProcessor_roleArn = Lens.lens (\CreateStreamProcessor' {roleArn} -> roleArn) (\s@CreateStreamProcessor' {} a -> s {roleArn = a} :: CreateStreamProcessor)

instance Core.AWSRequest CreateStreamProcessor where
  type
    AWSResponse CreateStreamProcessor =
      CreateStreamProcessorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamProcessorResponse'
            Prelude.<$> (x Data..?> "StreamProcessorArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStreamProcessor where
  hashWithSalt _salt CreateStreamProcessor' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` regionsOfInterest
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` dataSharingPreference
      `Prelude.hashWithSalt` notificationChannel
      `Prelude.hashWithSalt` input
      `Prelude.hashWithSalt` output
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateStreamProcessor where
  rnf CreateStreamProcessor' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf regionsOfInterest
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf dataSharingPreference
      `Prelude.seq` Prelude.rnf notificationChannel
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf output
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders CreateStreamProcessor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.CreateStreamProcessor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateStreamProcessor where
  toJSON CreateStreamProcessor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("RegionsOfInterest" Data..=)
              Prelude.<$> regionsOfInterest,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("DataSharingPreference" Data..=)
              Prelude.<$> dataSharingPreference,
            ("NotificationChannel" Data..=)
              Prelude.<$> notificationChannel,
            Prelude.Just ("Input" Data..= input),
            Prelude.Just ("Output" Data..= output),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Settings" Data..= settings),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateStreamProcessor where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateStreamProcessor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStreamProcessorResponse' smart constructor.
data CreateStreamProcessorResponse = CreateStreamProcessorResponse'
  { -- | Amazon Resource Number for the newly created stream processor.
    streamProcessorArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamProcessorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamProcessorArn', 'createStreamProcessorResponse_streamProcessorArn' - Amazon Resource Number for the newly created stream processor.
--
-- 'httpStatus', 'createStreamProcessorResponse_httpStatus' - The response's http status code.
newCreateStreamProcessorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStreamProcessorResponse
newCreateStreamProcessorResponse pHttpStatus_ =
  CreateStreamProcessorResponse'
    { streamProcessorArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Amazon Resource Number for the newly created stream processor.
createStreamProcessorResponse_streamProcessorArn :: Lens.Lens' CreateStreamProcessorResponse (Prelude.Maybe Prelude.Text)
createStreamProcessorResponse_streamProcessorArn = Lens.lens (\CreateStreamProcessorResponse' {streamProcessorArn} -> streamProcessorArn) (\s@CreateStreamProcessorResponse' {} a -> s {streamProcessorArn = a} :: CreateStreamProcessorResponse)

-- | The response's http status code.
createStreamProcessorResponse_httpStatus :: Lens.Lens' CreateStreamProcessorResponse Prelude.Int
createStreamProcessorResponse_httpStatus = Lens.lens (\CreateStreamProcessorResponse' {httpStatus} -> httpStatus) (\s@CreateStreamProcessorResponse' {} a -> s {httpStatus = a} :: CreateStreamProcessorResponse)

instance Prelude.NFData CreateStreamProcessorResponse where
  rnf CreateStreamProcessorResponse' {..} =
    Prelude.rnf streamProcessorArn
      `Prelude.seq` Prelude.rnf httpStatus
