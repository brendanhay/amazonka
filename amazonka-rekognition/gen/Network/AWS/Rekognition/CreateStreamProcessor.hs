{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Rekognition.CreateStreamProcessor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Rekognition stream processor that you can use to
-- detect and recognize faces in a streaming video.
--
-- Amazon Rekognition Video is a consumer of live video from Amazon Kinesis
-- Video Streams. Amazon Rekognition Video sends analysis results to Amazon
-- Kinesis Data Streams.
--
-- You provide as input a Kinesis video stream (@Input@) and a Kinesis data
-- stream (@Output@) stream. You also specify the face recognition criteria
-- in @Settings@. For example, the collection containing faces that you
-- want to recognize. Use @Name@ to assign an identifier for the stream
-- processor. You use @Name@ to manage the stream processor. For example,
-- you can start processing the source video by calling
-- StartStreamProcessor with the @Name@ field.
--
-- After you have finished analyzing a streaming video, use
-- StopStreamProcessor to stop processing. You can delete the stream
-- processor by calling DeleteStreamProcessor.
module Network.AWS.Rekognition.CreateStreamProcessor
  ( -- * Creating a Request
    CreateStreamProcessor (..),
    newCreateStreamProcessor,

    -- * Request Lenses
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateStreamProcessor' smart constructor.
data CreateStreamProcessor = CreateStreamProcessor'
  { -- | Kinesis video stream stream that provides the source streaming video. If
    -- you are using the AWS CLI, the parameter name is @StreamProcessorInput@.
    input :: StreamProcessorInput,
    -- | Kinesis data stream stream to which Amazon Rekognition Video puts the
    -- analysis results. If you are using the AWS CLI, the parameter name is
    -- @StreamProcessorOutput@.
    output :: StreamProcessorOutput,
    -- | An identifier you assign to the stream processor. You can use @Name@ to
    -- manage the stream processor. For example, you can get the current status
    -- of the stream processor by calling DescribeStreamProcessor. @Name@ is
    -- idempotent.
    name :: Prelude.Text,
    -- | Face recognition input parameters to be used by the stream processor.
    -- Includes the collection to use for face recognition and the face
    -- attributes to detect.
    settings :: StreamProcessorSettings,
    -- | ARN of the IAM role that allows access to the stream processor.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamProcessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'createStreamProcessor_input' - Kinesis video stream stream that provides the source streaming video. If
-- you are using the AWS CLI, the parameter name is @StreamProcessorInput@.
--
-- 'output', 'createStreamProcessor_output' - Kinesis data stream stream to which Amazon Rekognition Video puts the
-- analysis results. If you are using the AWS CLI, the parameter name is
-- @StreamProcessorOutput@.
--
-- 'name', 'createStreamProcessor_name' - An identifier you assign to the stream processor. You can use @Name@ to
-- manage the stream processor. For example, you can get the current status
-- of the stream processor by calling DescribeStreamProcessor. @Name@ is
-- idempotent.
--
-- 'settings', 'createStreamProcessor_settings' - Face recognition input parameters to be used by the stream processor.
-- Includes the collection to use for face recognition and the face
-- attributes to detect.
--
-- 'roleArn', 'createStreamProcessor_roleArn' - ARN of the IAM role that allows access to the stream processor.
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
      { input = pInput_,
        output = pOutput_,
        name = pName_,
        settings = pSettings_,
        roleArn = pRoleArn_
      }

-- | Kinesis video stream stream that provides the source streaming video. If
-- you are using the AWS CLI, the parameter name is @StreamProcessorInput@.
createStreamProcessor_input :: Lens.Lens' CreateStreamProcessor StreamProcessorInput
createStreamProcessor_input = Lens.lens (\CreateStreamProcessor' {input} -> input) (\s@CreateStreamProcessor' {} a -> s {input = a} :: CreateStreamProcessor)

-- | Kinesis data stream stream to which Amazon Rekognition Video puts the
-- analysis results. If you are using the AWS CLI, the parameter name is
-- @StreamProcessorOutput@.
createStreamProcessor_output :: Lens.Lens' CreateStreamProcessor StreamProcessorOutput
createStreamProcessor_output = Lens.lens (\CreateStreamProcessor' {output} -> output) (\s@CreateStreamProcessor' {} a -> s {output = a} :: CreateStreamProcessor)

-- | An identifier you assign to the stream processor. You can use @Name@ to
-- manage the stream processor. For example, you can get the current status
-- of the stream processor by calling DescribeStreamProcessor. @Name@ is
-- idempotent.
createStreamProcessor_name :: Lens.Lens' CreateStreamProcessor Prelude.Text
createStreamProcessor_name = Lens.lens (\CreateStreamProcessor' {name} -> name) (\s@CreateStreamProcessor' {} a -> s {name = a} :: CreateStreamProcessor)

-- | Face recognition input parameters to be used by the stream processor.
-- Includes the collection to use for face recognition and the face
-- attributes to detect.
createStreamProcessor_settings :: Lens.Lens' CreateStreamProcessor StreamProcessorSettings
createStreamProcessor_settings = Lens.lens (\CreateStreamProcessor' {settings} -> settings) (\s@CreateStreamProcessor' {} a -> s {settings = a} :: CreateStreamProcessor)

-- | ARN of the IAM role that allows access to the stream processor.
createStreamProcessor_roleArn :: Lens.Lens' CreateStreamProcessor Prelude.Text
createStreamProcessor_roleArn = Lens.lens (\CreateStreamProcessor' {roleArn} -> roleArn) (\s@CreateStreamProcessor' {} a -> s {roleArn = a} :: CreateStreamProcessor)

instance Prelude.AWSRequest CreateStreamProcessor where
  type
    Rs CreateStreamProcessor =
      CreateStreamProcessorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamProcessorResponse'
            Prelude.<$> (x Prelude..?> "StreamProcessorArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStreamProcessor

instance Prelude.NFData CreateStreamProcessor

instance Prelude.ToHeaders CreateStreamProcessor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "RekognitionService.CreateStreamProcessor" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateStreamProcessor where
  toJSON CreateStreamProcessor' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Input" Prelude..= input),
            Prelude.Just ("Output" Prelude..= output),
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("Settings" Prelude..= settings),
            Prelude.Just ("RoleArn" Prelude..= roleArn)
          ]
      )

instance Prelude.ToPath CreateStreamProcessor where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateStreamProcessor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStreamProcessorResponse' smart constructor.
data CreateStreamProcessorResponse = CreateStreamProcessorResponse'
  { -- | ARN for the newly create stream processor.
    streamProcessorArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamProcessorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamProcessorArn', 'createStreamProcessorResponse_streamProcessorArn' - ARN for the newly create stream processor.
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

-- | ARN for the newly create stream processor.
createStreamProcessorResponse_streamProcessorArn :: Lens.Lens' CreateStreamProcessorResponse (Prelude.Maybe Prelude.Text)
createStreamProcessorResponse_streamProcessorArn = Lens.lens (\CreateStreamProcessorResponse' {streamProcessorArn} -> streamProcessorArn) (\s@CreateStreamProcessorResponse' {} a -> s {streamProcessorArn = a} :: CreateStreamProcessorResponse)

-- | The response's http status code.
createStreamProcessorResponse_httpStatus :: Lens.Lens' CreateStreamProcessorResponse Prelude.Int
createStreamProcessorResponse_httpStatus = Lens.lens (\CreateStreamProcessorResponse' {httpStatus} -> httpStatus) (\s@CreateStreamProcessorResponse' {} a -> s {httpStatus = a} :: CreateStreamProcessorResponse)

instance Prelude.NFData CreateStreamProcessorResponse
