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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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
    name :: Core.Text,
    -- | Face recognition input parameters to be used by the stream processor.
    -- Includes the collection to use for face recognition and the face
    -- attributes to detect.
    settings :: StreamProcessorSettings,
    -- | ARN of the IAM role that allows access to the stream processor.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'settings'
  StreamProcessorSettings ->
  -- | 'roleArn'
  Core.Text ->
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
createStreamProcessor_name :: Lens.Lens' CreateStreamProcessor Core.Text
createStreamProcessor_name = Lens.lens (\CreateStreamProcessor' {name} -> name) (\s@CreateStreamProcessor' {} a -> s {name = a} :: CreateStreamProcessor)

-- | Face recognition input parameters to be used by the stream processor.
-- Includes the collection to use for face recognition and the face
-- attributes to detect.
createStreamProcessor_settings :: Lens.Lens' CreateStreamProcessor StreamProcessorSettings
createStreamProcessor_settings = Lens.lens (\CreateStreamProcessor' {settings} -> settings) (\s@CreateStreamProcessor' {} a -> s {settings = a} :: CreateStreamProcessor)

-- | ARN of the IAM role that allows access to the stream processor.
createStreamProcessor_roleArn :: Lens.Lens' CreateStreamProcessor Core.Text
createStreamProcessor_roleArn = Lens.lens (\CreateStreamProcessor' {roleArn} -> roleArn) (\s@CreateStreamProcessor' {} a -> s {roleArn = a} :: CreateStreamProcessor)

instance Core.AWSRequest CreateStreamProcessor where
  type
    AWSResponse CreateStreamProcessor =
      CreateStreamProcessorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamProcessorResponse'
            Core.<$> (x Core..?> "StreamProcessorArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateStreamProcessor

instance Core.NFData CreateStreamProcessor

instance Core.ToHeaders CreateStreamProcessor where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.CreateStreamProcessor" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateStreamProcessor where
  toJSON CreateStreamProcessor' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Input" Core..= input),
            Core.Just ("Output" Core..= output),
            Core.Just ("Name" Core..= name),
            Core.Just ("Settings" Core..= settings),
            Core.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateStreamProcessor where
  toPath = Core.const "/"

instance Core.ToQuery CreateStreamProcessor where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateStreamProcessorResponse' smart constructor.
data CreateStreamProcessorResponse = CreateStreamProcessorResponse'
  { -- | ARN for the newly create stream processor.
    streamProcessorArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateStreamProcessorResponse
newCreateStreamProcessorResponse pHttpStatus_ =
  CreateStreamProcessorResponse'
    { streamProcessorArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | ARN for the newly create stream processor.
createStreamProcessorResponse_streamProcessorArn :: Lens.Lens' CreateStreamProcessorResponse (Core.Maybe Core.Text)
createStreamProcessorResponse_streamProcessorArn = Lens.lens (\CreateStreamProcessorResponse' {streamProcessorArn} -> streamProcessorArn) (\s@CreateStreamProcessorResponse' {} a -> s {streamProcessorArn = a} :: CreateStreamProcessorResponse)

-- | The response's http status code.
createStreamProcessorResponse_httpStatus :: Lens.Lens' CreateStreamProcessorResponse Core.Int
createStreamProcessorResponse_httpStatus = Lens.lens (\CreateStreamProcessorResponse' {httpStatus} -> httpStatus) (\s@CreateStreamProcessorResponse' {} a -> s {httpStatus = a} :: CreateStreamProcessorResponse)

instance Core.NFData CreateStreamProcessorResponse
