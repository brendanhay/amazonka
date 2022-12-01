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
-- Module      : Amazonka.Rekognition.DescribeStreamProcessor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about a stream processor created by
-- CreateStreamProcessor. You can get information about the input and
-- output streams, the input parameters for the face recognition being
-- performed, and the current status of the stream processor.
module Amazonka.Rekognition.DescribeStreamProcessor
  ( -- * Creating a Request
    DescribeStreamProcessor (..),
    newDescribeStreamProcessor,

    -- * Request Lenses
    describeStreamProcessor_name,

    -- * Destructuring the Response
    DescribeStreamProcessorResponse (..),
    newDescribeStreamProcessorResponse,

    -- * Response Lenses
    describeStreamProcessorResponse_streamProcessorArn,
    describeStreamProcessorResponse_regionsOfInterest,
    describeStreamProcessorResponse_name,
    describeStreamProcessorResponse_lastUpdateTimestamp,
    describeStreamProcessorResponse_roleArn,
    describeStreamProcessorResponse_status,
    describeStreamProcessorResponse_creationTimestamp,
    describeStreamProcessorResponse_input,
    describeStreamProcessorResponse_settings,
    describeStreamProcessorResponse_output,
    describeStreamProcessorResponse_kmsKeyId,
    describeStreamProcessorResponse_dataSharingPreference,
    describeStreamProcessorResponse_statusMessage,
    describeStreamProcessorResponse_notificationChannel,
    describeStreamProcessorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStreamProcessorResponse'
            Prelude.<$> (x Core..?> "StreamProcessorArn")
            Prelude.<*> ( x Core..?> "RegionsOfInterest"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "LastUpdateTimestamp")
            Prelude.<*> (x Core..?> "RoleArn")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "Input")
            Prelude.<*> (x Core..?> "Settings")
            Prelude.<*> (x Core..?> "Output")
            Prelude.<*> (x Core..?> "KmsKeyId")
            Prelude.<*> (x Core..?> "DataSharingPreference")
            Prelude.<*> (x Core..?> "StatusMessage")
            Prelude.<*> (x Core..?> "NotificationChannel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStreamProcessor where
  hashWithSalt _salt DescribeStreamProcessor' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeStreamProcessor where
  rnf DescribeStreamProcessor' {..} = Prelude.rnf name

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
  { -- | ARN of the stream processor.
    streamProcessorArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies locations in the frames where Amazon Rekognition checks for
    -- objects or people. This is an optional parameter for label detection
    -- stream processors.
    regionsOfInterest :: Prelude.Maybe [RegionOfInterest],
    -- | Name of the stream processor.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time, in Unix format, the stream processor was last updated. For
    -- example, when the stream processor moves from a running state to a
    -- failed state, or when the user starts or stops the stream processor.
    lastUpdateTimestamp :: Prelude.Maybe Core.POSIX,
    -- | ARN of the IAM role that allows access to the stream processor.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Current status of the stream processor.
    status :: Prelude.Maybe StreamProcessorStatus,
    -- | Date and time the stream processor was created
    creationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Kinesis video stream that provides the source streaming video.
    input :: Prelude.Maybe StreamProcessorInput,
    -- | Input parameters used in a streaming video analyzed by a stream
    -- processor. You can use @FaceSearch@ to recognize faces in a streaming
    -- video, or you can use @ConnectedHome@ to detect labels.
    settings :: Prelude.Maybe StreamProcessorSettings,
    -- | Kinesis data stream to which Amazon Rekognition Video puts the analysis
    -- results.
    output :: Prelude.Maybe StreamProcessorOutput,
    -- | The identifier for your AWS Key Management Service key (AWS KMS key).
    -- This is an optional parameter for label detection stream processors.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Shows whether you are sharing data with Rekognition to improve model
    -- performance. You can choose this option at the account level or on a
    -- per-stream basis. Note that if you opt out at the account level this
    -- setting is ignored on individual streams.
    dataSharingPreference :: Prelude.Maybe StreamProcessorDataSharingPreference,
    -- | Detailed status message about the stream processor.
    statusMessage :: Prelude.Maybe Prelude.Text,
    notificationChannel :: Prelude.Maybe StreamProcessorNotificationChannel,
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
-- 'streamProcessorArn', 'describeStreamProcessorResponse_streamProcessorArn' - ARN of the stream processor.
--
-- 'regionsOfInterest', 'describeStreamProcessorResponse_regionsOfInterest' - Specifies locations in the frames where Amazon Rekognition checks for
-- objects or people. This is an optional parameter for label detection
-- stream processors.
--
-- 'name', 'describeStreamProcessorResponse_name' - Name of the stream processor.
--
-- 'lastUpdateTimestamp', 'describeStreamProcessorResponse_lastUpdateTimestamp' - The time, in Unix format, the stream processor was last updated. For
-- example, when the stream processor moves from a running state to a
-- failed state, or when the user starts or stops the stream processor.
--
-- 'roleArn', 'describeStreamProcessorResponse_roleArn' - ARN of the IAM role that allows access to the stream processor.
--
-- 'status', 'describeStreamProcessorResponse_status' - Current status of the stream processor.
--
-- 'creationTimestamp', 'describeStreamProcessorResponse_creationTimestamp' - Date and time the stream processor was created
--
-- 'input', 'describeStreamProcessorResponse_input' - Kinesis video stream that provides the source streaming video.
--
-- 'settings', 'describeStreamProcessorResponse_settings' - Input parameters used in a streaming video analyzed by a stream
-- processor. You can use @FaceSearch@ to recognize faces in a streaming
-- video, or you can use @ConnectedHome@ to detect labels.
--
-- 'output', 'describeStreamProcessorResponse_output' - Kinesis data stream to which Amazon Rekognition Video puts the analysis
-- results.
--
-- 'kmsKeyId', 'describeStreamProcessorResponse_kmsKeyId' - The identifier for your AWS Key Management Service key (AWS KMS key).
-- This is an optional parameter for label detection stream processors.
--
-- 'dataSharingPreference', 'describeStreamProcessorResponse_dataSharingPreference' - Shows whether you are sharing data with Rekognition to improve model
-- performance. You can choose this option at the account level or on a
-- per-stream basis. Note that if you opt out at the account level this
-- setting is ignored on individual streams.
--
-- 'statusMessage', 'describeStreamProcessorResponse_statusMessage' - Detailed status message about the stream processor.
--
-- 'notificationChannel', 'describeStreamProcessorResponse_notificationChannel' - Undocumented member.
--
-- 'httpStatus', 'describeStreamProcessorResponse_httpStatus' - The response's http status code.
newDescribeStreamProcessorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStreamProcessorResponse
newDescribeStreamProcessorResponse pHttpStatus_ =
  DescribeStreamProcessorResponse'
    { streamProcessorArn =
        Prelude.Nothing,
      regionsOfInterest = Prelude.Nothing,
      name = Prelude.Nothing,
      lastUpdateTimestamp = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      status = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      input = Prelude.Nothing,
      settings = Prelude.Nothing,
      output = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      dataSharingPreference = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      notificationChannel = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | ARN of the stream processor.
describeStreamProcessorResponse_streamProcessorArn :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.Text)
describeStreamProcessorResponse_streamProcessorArn = Lens.lens (\DescribeStreamProcessorResponse' {streamProcessorArn} -> streamProcessorArn) (\s@DescribeStreamProcessorResponse' {} a -> s {streamProcessorArn = a} :: DescribeStreamProcessorResponse)

-- | Specifies locations in the frames where Amazon Rekognition checks for
-- objects or people. This is an optional parameter for label detection
-- stream processors.
describeStreamProcessorResponse_regionsOfInterest :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe [RegionOfInterest])
describeStreamProcessorResponse_regionsOfInterest = Lens.lens (\DescribeStreamProcessorResponse' {regionsOfInterest} -> regionsOfInterest) (\s@DescribeStreamProcessorResponse' {} a -> s {regionsOfInterest = a} :: DescribeStreamProcessorResponse) Prelude.. Lens.mapping Lens.coerced

-- | Name of the stream processor.
describeStreamProcessorResponse_name :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.Text)
describeStreamProcessorResponse_name = Lens.lens (\DescribeStreamProcessorResponse' {name} -> name) (\s@DescribeStreamProcessorResponse' {} a -> s {name = a} :: DescribeStreamProcessorResponse)

-- | The time, in Unix format, the stream processor was last updated. For
-- example, when the stream processor moves from a running state to a
-- failed state, or when the user starts or stops the stream processor.
describeStreamProcessorResponse_lastUpdateTimestamp :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.UTCTime)
describeStreamProcessorResponse_lastUpdateTimestamp = Lens.lens (\DescribeStreamProcessorResponse' {lastUpdateTimestamp} -> lastUpdateTimestamp) (\s@DescribeStreamProcessorResponse' {} a -> s {lastUpdateTimestamp = a} :: DescribeStreamProcessorResponse) Prelude.. Lens.mapping Core._Time

-- | ARN of the IAM role that allows access to the stream processor.
describeStreamProcessorResponse_roleArn :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.Text)
describeStreamProcessorResponse_roleArn = Lens.lens (\DescribeStreamProcessorResponse' {roleArn} -> roleArn) (\s@DescribeStreamProcessorResponse' {} a -> s {roleArn = a} :: DescribeStreamProcessorResponse)

-- | Current status of the stream processor.
describeStreamProcessorResponse_status :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorStatus)
describeStreamProcessorResponse_status = Lens.lens (\DescribeStreamProcessorResponse' {status} -> status) (\s@DescribeStreamProcessorResponse' {} a -> s {status = a} :: DescribeStreamProcessorResponse)

-- | Date and time the stream processor was created
describeStreamProcessorResponse_creationTimestamp :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.UTCTime)
describeStreamProcessorResponse_creationTimestamp = Lens.lens (\DescribeStreamProcessorResponse' {creationTimestamp} -> creationTimestamp) (\s@DescribeStreamProcessorResponse' {} a -> s {creationTimestamp = a} :: DescribeStreamProcessorResponse) Prelude.. Lens.mapping Core._Time

-- | Kinesis video stream that provides the source streaming video.
describeStreamProcessorResponse_input :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorInput)
describeStreamProcessorResponse_input = Lens.lens (\DescribeStreamProcessorResponse' {input} -> input) (\s@DescribeStreamProcessorResponse' {} a -> s {input = a} :: DescribeStreamProcessorResponse)

-- | Input parameters used in a streaming video analyzed by a stream
-- processor. You can use @FaceSearch@ to recognize faces in a streaming
-- video, or you can use @ConnectedHome@ to detect labels.
describeStreamProcessorResponse_settings :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorSettings)
describeStreamProcessorResponse_settings = Lens.lens (\DescribeStreamProcessorResponse' {settings} -> settings) (\s@DescribeStreamProcessorResponse' {} a -> s {settings = a} :: DescribeStreamProcessorResponse)

-- | Kinesis data stream to which Amazon Rekognition Video puts the analysis
-- results.
describeStreamProcessorResponse_output :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorOutput)
describeStreamProcessorResponse_output = Lens.lens (\DescribeStreamProcessorResponse' {output} -> output) (\s@DescribeStreamProcessorResponse' {} a -> s {output = a} :: DescribeStreamProcessorResponse)

-- | The identifier for your AWS Key Management Service key (AWS KMS key).
-- This is an optional parameter for label detection stream processors.
describeStreamProcessorResponse_kmsKeyId :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.Text)
describeStreamProcessorResponse_kmsKeyId = Lens.lens (\DescribeStreamProcessorResponse' {kmsKeyId} -> kmsKeyId) (\s@DescribeStreamProcessorResponse' {} a -> s {kmsKeyId = a} :: DescribeStreamProcessorResponse)

-- | Shows whether you are sharing data with Rekognition to improve model
-- performance. You can choose this option at the account level or on a
-- per-stream basis. Note that if you opt out at the account level this
-- setting is ignored on individual streams.
describeStreamProcessorResponse_dataSharingPreference :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorDataSharingPreference)
describeStreamProcessorResponse_dataSharingPreference = Lens.lens (\DescribeStreamProcessorResponse' {dataSharingPreference} -> dataSharingPreference) (\s@DescribeStreamProcessorResponse' {} a -> s {dataSharingPreference = a} :: DescribeStreamProcessorResponse)

-- | Detailed status message about the stream processor.
describeStreamProcessorResponse_statusMessage :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.Text)
describeStreamProcessorResponse_statusMessage = Lens.lens (\DescribeStreamProcessorResponse' {statusMessage} -> statusMessage) (\s@DescribeStreamProcessorResponse' {} a -> s {statusMessage = a} :: DescribeStreamProcessorResponse)

-- | Undocumented member.
describeStreamProcessorResponse_notificationChannel :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorNotificationChannel)
describeStreamProcessorResponse_notificationChannel = Lens.lens (\DescribeStreamProcessorResponse' {notificationChannel} -> notificationChannel) (\s@DescribeStreamProcessorResponse' {} a -> s {notificationChannel = a} :: DescribeStreamProcessorResponse)

-- | The response's http status code.
describeStreamProcessorResponse_httpStatus :: Lens.Lens' DescribeStreamProcessorResponse Prelude.Int
describeStreamProcessorResponse_httpStatus = Lens.lens (\DescribeStreamProcessorResponse' {httpStatus} -> httpStatus) (\s@DescribeStreamProcessorResponse' {} a -> s {httpStatus = a} :: DescribeStreamProcessorResponse)

instance
  Prelude.NFData
    DescribeStreamProcessorResponse
  where
  rnf DescribeStreamProcessorResponse' {..} =
    Prelude.rnf streamProcessorArn
      `Prelude.seq` Prelude.rnf regionsOfInterest
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastUpdateTimestamp
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf output
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf dataSharingPreference
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf notificationChannel
      `Prelude.seq` Prelude.rnf httpStatus
