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
-- Module      : Network.AWS.Rekognition.DescribeStreamProcessor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about a stream processor created by
-- CreateStreamProcessor. You can get information about the input and
-- output streams, the input parameters for the face recognition being
-- performed, and the current status of the stream processor.
module Network.AWS.Rekognition.DescribeStreamProcessor
  ( -- * Creating a Request
    DescribeStreamProcessor (..),
    newDescribeStreamProcessor,

    -- * Request Lenses
    describeStreamProcessor_name,

    -- * Destructuring the Response
    DescribeStreamProcessorResponse (..),
    newDescribeStreamProcessorResponse,

    -- * Response Lenses
    describeStreamProcessorResponse_creationTimestamp,
    describeStreamProcessorResponse_statusMessage,
    describeStreamProcessorResponse_status,
    describeStreamProcessorResponse_roleArn,
    describeStreamProcessorResponse_input,
    describeStreamProcessorResponse_streamProcessorArn,
    describeStreamProcessorResponse_output,
    describeStreamProcessorResponse_name,
    describeStreamProcessorResponse_lastUpdateTimestamp,
    describeStreamProcessorResponse_settings,
    describeStreamProcessorResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStreamProcessor' smart constructor.
data DescribeStreamProcessor = DescribeStreamProcessor'
  { -- | Name of the stream processor for which you want information.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStreamProcessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeStreamProcessor_name' - Name of the stream processor for which you want information.
newDescribeStreamProcessor ::
  -- | 'name'
  Prelude.Text ->
  DescribeStreamProcessor
newDescribeStreamProcessor pName_ =
  DescribeStreamProcessor' {name = pName_}

-- | Name of the stream processor for which you want information.
describeStreamProcessor_name :: Lens.Lens' DescribeStreamProcessor Prelude.Text
describeStreamProcessor_name = Lens.lens (\DescribeStreamProcessor' {name} -> name) (\s@DescribeStreamProcessor' {} a -> s {name = a} :: DescribeStreamProcessor)

instance Core.AWSRequest DescribeStreamProcessor where
  type
    AWSResponse DescribeStreamProcessor =
      DescribeStreamProcessorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStreamProcessorResponse'
            Prelude.<$> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "StatusMessage")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "RoleArn")
            Prelude.<*> (x Core..?> "Input")
            Prelude.<*> (x Core..?> "StreamProcessorArn")
            Prelude.<*> (x Core..?> "Output")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "LastUpdateTimestamp")
            Prelude.<*> (x Core..?> "Settings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStreamProcessor

instance Prelude.NFData DescribeStreamProcessor

instance Core.ToHeaders DescribeStreamProcessor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.DescribeStreamProcessor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeStreamProcessor where
  toJSON DescribeStreamProcessor' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Core..= name)]
      )

instance Core.ToPath DescribeStreamProcessor where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeStreamProcessor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStreamProcessorResponse' smart constructor.
data DescribeStreamProcessorResponse = DescribeStreamProcessorResponse'
  { -- | Date and time the stream processor was created
    creationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Detailed status message about the stream processor.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Current status of the stream processor.
    status :: Prelude.Maybe StreamProcessorStatus,
    -- | ARN of the IAM role that allows access to the stream processor.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Kinesis video stream that provides the source streaming video.
    input :: Prelude.Maybe StreamProcessorInput,
    -- | ARN of the stream processor.
    streamProcessorArn :: Prelude.Maybe Prelude.Text,
    -- | Kinesis data stream to which Amazon Rekognition Video puts the analysis
    -- results.
    output :: Prelude.Maybe StreamProcessorOutput,
    -- | Name of the stream processor.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time, in Unix format, the stream processor was last updated. For
    -- example, when the stream processor moves from a running state to a
    -- failed state, or when the user starts or stops the stream processor.
    lastUpdateTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Face recognition input parameters that are being used by the stream
    -- processor. Includes the collection to use for face recognition and the
    -- face attributes to detect.
    settings :: Prelude.Maybe StreamProcessorSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStreamProcessorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'describeStreamProcessorResponse_creationTimestamp' - Date and time the stream processor was created
--
-- 'statusMessage', 'describeStreamProcessorResponse_statusMessage' - Detailed status message about the stream processor.
--
-- 'status', 'describeStreamProcessorResponse_status' - Current status of the stream processor.
--
-- 'roleArn', 'describeStreamProcessorResponse_roleArn' - ARN of the IAM role that allows access to the stream processor.
--
-- 'input', 'describeStreamProcessorResponse_input' - Kinesis video stream that provides the source streaming video.
--
-- 'streamProcessorArn', 'describeStreamProcessorResponse_streamProcessorArn' - ARN of the stream processor.
--
-- 'output', 'describeStreamProcessorResponse_output' - Kinesis data stream to which Amazon Rekognition Video puts the analysis
-- results.
--
-- 'name', 'describeStreamProcessorResponse_name' - Name of the stream processor.
--
-- 'lastUpdateTimestamp', 'describeStreamProcessorResponse_lastUpdateTimestamp' - The time, in Unix format, the stream processor was last updated. For
-- example, when the stream processor moves from a running state to a
-- failed state, or when the user starts or stops the stream processor.
--
-- 'settings', 'describeStreamProcessorResponse_settings' - Face recognition input parameters that are being used by the stream
-- processor. Includes the collection to use for face recognition and the
-- face attributes to detect.
--
-- 'httpStatus', 'describeStreamProcessorResponse_httpStatus' - The response's http status code.
newDescribeStreamProcessorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStreamProcessorResponse
newDescribeStreamProcessorResponse pHttpStatus_ =
  DescribeStreamProcessorResponse'
    { creationTimestamp =
        Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      status = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      input = Prelude.Nothing,
      streamProcessorArn = Prelude.Nothing,
      output = Prelude.Nothing,
      name = Prelude.Nothing,
      lastUpdateTimestamp = Prelude.Nothing,
      settings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Date and time the stream processor was created
describeStreamProcessorResponse_creationTimestamp :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.UTCTime)
describeStreamProcessorResponse_creationTimestamp = Lens.lens (\DescribeStreamProcessorResponse' {creationTimestamp} -> creationTimestamp) (\s@DescribeStreamProcessorResponse' {} a -> s {creationTimestamp = a} :: DescribeStreamProcessorResponse) Prelude.. Lens.mapping Core._Time

-- | Detailed status message about the stream processor.
describeStreamProcessorResponse_statusMessage :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.Text)
describeStreamProcessorResponse_statusMessage = Lens.lens (\DescribeStreamProcessorResponse' {statusMessage} -> statusMessage) (\s@DescribeStreamProcessorResponse' {} a -> s {statusMessage = a} :: DescribeStreamProcessorResponse)

-- | Current status of the stream processor.
describeStreamProcessorResponse_status :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorStatus)
describeStreamProcessorResponse_status = Lens.lens (\DescribeStreamProcessorResponse' {status} -> status) (\s@DescribeStreamProcessorResponse' {} a -> s {status = a} :: DescribeStreamProcessorResponse)

-- | ARN of the IAM role that allows access to the stream processor.
describeStreamProcessorResponse_roleArn :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.Text)
describeStreamProcessorResponse_roleArn = Lens.lens (\DescribeStreamProcessorResponse' {roleArn} -> roleArn) (\s@DescribeStreamProcessorResponse' {} a -> s {roleArn = a} :: DescribeStreamProcessorResponse)

-- | Kinesis video stream that provides the source streaming video.
describeStreamProcessorResponse_input :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorInput)
describeStreamProcessorResponse_input = Lens.lens (\DescribeStreamProcessorResponse' {input} -> input) (\s@DescribeStreamProcessorResponse' {} a -> s {input = a} :: DescribeStreamProcessorResponse)

-- | ARN of the stream processor.
describeStreamProcessorResponse_streamProcessorArn :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.Text)
describeStreamProcessorResponse_streamProcessorArn = Lens.lens (\DescribeStreamProcessorResponse' {streamProcessorArn} -> streamProcessorArn) (\s@DescribeStreamProcessorResponse' {} a -> s {streamProcessorArn = a} :: DescribeStreamProcessorResponse)

-- | Kinesis data stream to which Amazon Rekognition Video puts the analysis
-- results.
describeStreamProcessorResponse_output :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorOutput)
describeStreamProcessorResponse_output = Lens.lens (\DescribeStreamProcessorResponse' {output} -> output) (\s@DescribeStreamProcessorResponse' {} a -> s {output = a} :: DescribeStreamProcessorResponse)

-- | Name of the stream processor.
describeStreamProcessorResponse_name :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.Text)
describeStreamProcessorResponse_name = Lens.lens (\DescribeStreamProcessorResponse' {name} -> name) (\s@DescribeStreamProcessorResponse' {} a -> s {name = a} :: DescribeStreamProcessorResponse)

-- | The time, in Unix format, the stream processor was last updated. For
-- example, when the stream processor moves from a running state to a
-- failed state, or when the user starts or stops the stream processor.
describeStreamProcessorResponse_lastUpdateTimestamp :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.UTCTime)
describeStreamProcessorResponse_lastUpdateTimestamp = Lens.lens (\DescribeStreamProcessorResponse' {lastUpdateTimestamp} -> lastUpdateTimestamp) (\s@DescribeStreamProcessorResponse' {} a -> s {lastUpdateTimestamp = a} :: DescribeStreamProcessorResponse) Prelude.. Lens.mapping Core._Time

-- | Face recognition input parameters that are being used by the stream
-- processor. Includes the collection to use for face recognition and the
-- face attributes to detect.
describeStreamProcessorResponse_settings :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorSettings)
describeStreamProcessorResponse_settings = Lens.lens (\DescribeStreamProcessorResponse' {settings} -> settings) (\s@DescribeStreamProcessorResponse' {} a -> s {settings = a} :: DescribeStreamProcessorResponse)

-- | The response's http status code.
describeStreamProcessorResponse_httpStatus :: Lens.Lens' DescribeStreamProcessorResponse Prelude.Int
describeStreamProcessorResponse_httpStatus = Lens.lens (\DescribeStreamProcessorResponse' {httpStatus} -> httpStatus) (\s@DescribeStreamProcessorResponse' {} a -> s {httpStatus = a} :: DescribeStreamProcessorResponse)

instance
  Prelude.NFData
    DescribeStreamProcessorResponse
