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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    describeStreamProcessorResponse_creationTimestamp,
    describeStreamProcessorResponse_dataSharingPreference,
    describeStreamProcessorResponse_input,
    describeStreamProcessorResponse_kmsKeyId,
    describeStreamProcessorResponse_lastUpdateTimestamp,
    describeStreamProcessorResponse_name,
    describeStreamProcessorResponse_notificationChannel,
    describeStreamProcessorResponse_output,
    describeStreamProcessorResponse_regionsOfInterest,
    describeStreamProcessorResponse_roleArn,
    describeStreamProcessorResponse_settings,
    describeStreamProcessorResponse_status,
    describeStreamProcessorResponse_statusMessage,
    describeStreamProcessorResponse_streamProcessorArn,
    describeStreamProcessorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
            Prelude.<$> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "DataSharingPreference")
            Prelude.<*> (x Data..?> "Input")
            Prelude.<*> (x Data..?> "KmsKeyId")
            Prelude.<*> (x Data..?> "LastUpdateTimestamp")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "NotificationChannel")
            Prelude.<*> (x Data..?> "Output")
            Prelude.<*> ( x
                            Data..?> "RegionsOfInterest"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "Settings")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "StatusMessage")
            Prelude.<*> (x Data..?> "StreamProcessorArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStreamProcessor where
  hashWithSalt _salt DescribeStreamProcessor' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeStreamProcessor where
  rnf DescribeStreamProcessor' {..} = Prelude.rnf name

instance Data.ToHeaders DescribeStreamProcessor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.DescribeStreamProcessor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeStreamProcessor where
  toJSON DescribeStreamProcessor' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DescribeStreamProcessor where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStreamProcessor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStreamProcessorResponse' smart constructor.
data DescribeStreamProcessorResponse = DescribeStreamProcessorResponse'
  { -- | Date and time the stream processor was created
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | Shows whether you are sharing data with Rekognition to improve model
    -- performance. You can choose this option at the account level or on a
    -- per-stream basis. Note that if you opt out at the account level this
    -- setting is ignored on individual streams.
    dataSharingPreference :: Prelude.Maybe StreamProcessorDataSharingPreference,
    -- | Kinesis video stream that provides the source streaming video.
    input :: Prelude.Maybe StreamProcessorInput,
    -- | The identifier for your AWS Key Management Service key (AWS KMS key).
    -- This is an optional parameter for label detection stream processors.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The time, in Unix format, the stream processor was last updated. For
    -- example, when the stream processor moves from a running state to a
    -- failed state, or when the user starts or stops the stream processor.
    lastUpdateTimestamp :: Prelude.Maybe Data.POSIX,
    -- | Name of the stream processor.
    name :: Prelude.Maybe Prelude.Text,
    notificationChannel :: Prelude.Maybe StreamProcessorNotificationChannel,
    -- | Kinesis data stream to which Amazon Rekognition Video puts the analysis
    -- results.
    output :: Prelude.Maybe StreamProcessorOutput,
    -- | Specifies locations in the frames where Amazon Rekognition checks for
    -- objects or people. This is an optional parameter for label detection
    -- stream processors.
    regionsOfInterest :: Prelude.Maybe [RegionOfInterest],
    -- | ARN of the IAM role that allows access to the stream processor.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Input parameters used in a streaming video analyzed by a stream
    -- processor. You can use @FaceSearch@ to recognize faces in a streaming
    -- video, or you can use @ConnectedHome@ to detect labels.
    settings :: Prelude.Maybe StreamProcessorSettings,
    -- | Current status of the stream processor.
    status :: Prelude.Maybe StreamProcessorStatus,
    -- | Detailed status message about the stream processor.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | ARN of the stream processor.
    streamProcessorArn :: Prelude.Maybe Prelude.Text,
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
-- 'dataSharingPreference', 'describeStreamProcessorResponse_dataSharingPreference' - Shows whether you are sharing data with Rekognition to improve model
-- performance. You can choose this option at the account level or on a
-- per-stream basis. Note that if you opt out at the account level this
-- setting is ignored on individual streams.
--
-- 'input', 'describeStreamProcessorResponse_input' - Kinesis video stream that provides the source streaming video.
--
-- 'kmsKeyId', 'describeStreamProcessorResponse_kmsKeyId' - The identifier for your AWS Key Management Service key (AWS KMS key).
-- This is an optional parameter for label detection stream processors.
--
-- 'lastUpdateTimestamp', 'describeStreamProcessorResponse_lastUpdateTimestamp' - The time, in Unix format, the stream processor was last updated. For
-- example, when the stream processor moves from a running state to a
-- failed state, or when the user starts or stops the stream processor.
--
-- 'name', 'describeStreamProcessorResponse_name' - Name of the stream processor.
--
-- 'notificationChannel', 'describeStreamProcessorResponse_notificationChannel' - Undocumented member.
--
-- 'output', 'describeStreamProcessorResponse_output' - Kinesis data stream to which Amazon Rekognition Video puts the analysis
-- results.
--
-- 'regionsOfInterest', 'describeStreamProcessorResponse_regionsOfInterest' - Specifies locations in the frames where Amazon Rekognition checks for
-- objects or people. This is an optional parameter for label detection
-- stream processors.
--
-- 'roleArn', 'describeStreamProcessorResponse_roleArn' - ARN of the IAM role that allows access to the stream processor.
--
-- 'settings', 'describeStreamProcessorResponse_settings' - Input parameters used in a streaming video analyzed by a stream
-- processor. You can use @FaceSearch@ to recognize faces in a streaming
-- video, or you can use @ConnectedHome@ to detect labels.
--
-- 'status', 'describeStreamProcessorResponse_status' - Current status of the stream processor.
--
-- 'statusMessage', 'describeStreamProcessorResponse_statusMessage' - Detailed status message about the stream processor.
--
-- 'streamProcessorArn', 'describeStreamProcessorResponse_streamProcessorArn' - ARN of the stream processor.
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
      dataSharingPreference = Prelude.Nothing,
      input = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      lastUpdateTimestamp = Prelude.Nothing,
      name = Prelude.Nothing,
      notificationChannel = Prelude.Nothing,
      output = Prelude.Nothing,
      regionsOfInterest = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      settings = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      streamProcessorArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Date and time the stream processor was created
describeStreamProcessorResponse_creationTimestamp :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.UTCTime)
describeStreamProcessorResponse_creationTimestamp = Lens.lens (\DescribeStreamProcessorResponse' {creationTimestamp} -> creationTimestamp) (\s@DescribeStreamProcessorResponse' {} a -> s {creationTimestamp = a} :: DescribeStreamProcessorResponse) Prelude.. Lens.mapping Data._Time

-- | Shows whether you are sharing data with Rekognition to improve model
-- performance. You can choose this option at the account level or on a
-- per-stream basis. Note that if you opt out at the account level this
-- setting is ignored on individual streams.
describeStreamProcessorResponse_dataSharingPreference :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorDataSharingPreference)
describeStreamProcessorResponse_dataSharingPreference = Lens.lens (\DescribeStreamProcessorResponse' {dataSharingPreference} -> dataSharingPreference) (\s@DescribeStreamProcessorResponse' {} a -> s {dataSharingPreference = a} :: DescribeStreamProcessorResponse)

-- | Kinesis video stream that provides the source streaming video.
describeStreamProcessorResponse_input :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorInput)
describeStreamProcessorResponse_input = Lens.lens (\DescribeStreamProcessorResponse' {input} -> input) (\s@DescribeStreamProcessorResponse' {} a -> s {input = a} :: DescribeStreamProcessorResponse)

-- | The identifier for your AWS Key Management Service key (AWS KMS key).
-- This is an optional parameter for label detection stream processors.
describeStreamProcessorResponse_kmsKeyId :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.Text)
describeStreamProcessorResponse_kmsKeyId = Lens.lens (\DescribeStreamProcessorResponse' {kmsKeyId} -> kmsKeyId) (\s@DescribeStreamProcessorResponse' {} a -> s {kmsKeyId = a} :: DescribeStreamProcessorResponse)

-- | The time, in Unix format, the stream processor was last updated. For
-- example, when the stream processor moves from a running state to a
-- failed state, or when the user starts or stops the stream processor.
describeStreamProcessorResponse_lastUpdateTimestamp :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.UTCTime)
describeStreamProcessorResponse_lastUpdateTimestamp = Lens.lens (\DescribeStreamProcessorResponse' {lastUpdateTimestamp} -> lastUpdateTimestamp) (\s@DescribeStreamProcessorResponse' {} a -> s {lastUpdateTimestamp = a} :: DescribeStreamProcessorResponse) Prelude.. Lens.mapping Data._Time

-- | Name of the stream processor.
describeStreamProcessorResponse_name :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.Text)
describeStreamProcessorResponse_name = Lens.lens (\DescribeStreamProcessorResponse' {name} -> name) (\s@DescribeStreamProcessorResponse' {} a -> s {name = a} :: DescribeStreamProcessorResponse)

-- | Undocumented member.
describeStreamProcessorResponse_notificationChannel :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorNotificationChannel)
describeStreamProcessorResponse_notificationChannel = Lens.lens (\DescribeStreamProcessorResponse' {notificationChannel} -> notificationChannel) (\s@DescribeStreamProcessorResponse' {} a -> s {notificationChannel = a} :: DescribeStreamProcessorResponse)

-- | Kinesis data stream to which Amazon Rekognition Video puts the analysis
-- results.
describeStreamProcessorResponse_output :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorOutput)
describeStreamProcessorResponse_output = Lens.lens (\DescribeStreamProcessorResponse' {output} -> output) (\s@DescribeStreamProcessorResponse' {} a -> s {output = a} :: DescribeStreamProcessorResponse)

-- | Specifies locations in the frames where Amazon Rekognition checks for
-- objects or people. This is an optional parameter for label detection
-- stream processors.
describeStreamProcessorResponse_regionsOfInterest :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe [RegionOfInterest])
describeStreamProcessorResponse_regionsOfInterest = Lens.lens (\DescribeStreamProcessorResponse' {regionsOfInterest} -> regionsOfInterest) (\s@DescribeStreamProcessorResponse' {} a -> s {regionsOfInterest = a} :: DescribeStreamProcessorResponse) Prelude.. Lens.mapping Lens.coerced

-- | ARN of the IAM role that allows access to the stream processor.
describeStreamProcessorResponse_roleArn :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.Text)
describeStreamProcessorResponse_roleArn = Lens.lens (\DescribeStreamProcessorResponse' {roleArn} -> roleArn) (\s@DescribeStreamProcessorResponse' {} a -> s {roleArn = a} :: DescribeStreamProcessorResponse)

-- | Input parameters used in a streaming video analyzed by a stream
-- processor. You can use @FaceSearch@ to recognize faces in a streaming
-- video, or you can use @ConnectedHome@ to detect labels.
describeStreamProcessorResponse_settings :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorSettings)
describeStreamProcessorResponse_settings = Lens.lens (\DescribeStreamProcessorResponse' {settings} -> settings) (\s@DescribeStreamProcessorResponse' {} a -> s {settings = a} :: DescribeStreamProcessorResponse)

-- | Current status of the stream processor.
describeStreamProcessorResponse_status :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe StreamProcessorStatus)
describeStreamProcessorResponse_status = Lens.lens (\DescribeStreamProcessorResponse' {status} -> status) (\s@DescribeStreamProcessorResponse' {} a -> s {status = a} :: DescribeStreamProcessorResponse)

-- | Detailed status message about the stream processor.
describeStreamProcessorResponse_statusMessage :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.Text)
describeStreamProcessorResponse_statusMessage = Lens.lens (\DescribeStreamProcessorResponse' {statusMessage} -> statusMessage) (\s@DescribeStreamProcessorResponse' {} a -> s {statusMessage = a} :: DescribeStreamProcessorResponse)

-- | ARN of the stream processor.
describeStreamProcessorResponse_streamProcessorArn :: Lens.Lens' DescribeStreamProcessorResponse (Prelude.Maybe Prelude.Text)
describeStreamProcessorResponse_streamProcessorArn = Lens.lens (\DescribeStreamProcessorResponse' {streamProcessorArn} -> streamProcessorArn) (\s@DescribeStreamProcessorResponse' {} a -> s {streamProcessorArn = a} :: DescribeStreamProcessorResponse)

-- | The response's http status code.
describeStreamProcessorResponse_httpStatus :: Lens.Lens' DescribeStreamProcessorResponse Prelude.Int
describeStreamProcessorResponse_httpStatus = Lens.lens (\DescribeStreamProcessorResponse' {httpStatus} -> httpStatus) (\s@DescribeStreamProcessorResponse' {} a -> s {httpStatus = a} :: DescribeStreamProcessorResponse)

instance
  Prelude.NFData
    DescribeStreamProcessorResponse
  where
  rnf DescribeStreamProcessorResponse' {..} =
    Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf dataSharingPreference
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf lastUpdateTimestamp
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf notificationChannel
      `Prelude.seq` Prelude.rnf output
      `Prelude.seq` Prelude.rnf regionsOfInterest
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf streamProcessorArn
      `Prelude.seq` Prelude.rnf httpStatus
