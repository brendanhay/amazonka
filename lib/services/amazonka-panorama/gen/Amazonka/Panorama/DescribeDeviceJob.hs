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
-- Module      : Amazonka.Panorama.DescribeDeviceJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a device job.
module Amazonka.Panorama.DescribeDeviceJob
  ( -- * Creating a Request
    DescribeDeviceJob (..),
    newDescribeDeviceJob,

    -- * Request Lenses
    describeDeviceJob_jobId,

    -- * Destructuring the Response
    DescribeDeviceJobResponse (..),
    newDescribeDeviceJobResponse,

    -- * Response Lenses
    describeDeviceJobResponse_createdTime,
    describeDeviceJobResponse_deviceId,
    describeDeviceJobResponse_deviceName,
    describeDeviceJobResponse_jobId,
    describeDeviceJobResponse_status,
    describeDeviceJobResponse_imageVersion,
    describeDeviceJobResponse_deviceArn,
    describeDeviceJobResponse_deviceType,
    describeDeviceJobResponse_jobType,
    describeDeviceJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDeviceJob' smart constructor.
data DescribeDeviceJob = DescribeDeviceJob'
  { -- | The job\'s ID.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDeviceJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeDeviceJob_jobId' - The job\'s ID.
newDescribeDeviceJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeDeviceJob
newDescribeDeviceJob pJobId_ =
  DescribeDeviceJob' {jobId = pJobId_}

-- | The job\'s ID.
describeDeviceJob_jobId :: Lens.Lens' DescribeDeviceJob Prelude.Text
describeDeviceJob_jobId = Lens.lens (\DescribeDeviceJob' {jobId} -> jobId) (\s@DescribeDeviceJob' {} a -> s {jobId = a} :: DescribeDeviceJob)

instance Core.AWSRequest DescribeDeviceJob where
  type
    AWSResponse DescribeDeviceJob =
      DescribeDeviceJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeviceJobResponse'
            Prelude.<$> (x Core..?> "CreatedTime")
            Prelude.<*> (x Core..?> "DeviceId")
            Prelude.<*> (x Core..?> "DeviceName")
            Prelude.<*> (x Core..?> "JobId")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "ImageVersion")
            Prelude.<*> (x Core..?> "DeviceArn")
            Prelude.<*> (x Core..?> "DeviceType")
            Prelude.<*> (x Core..?> "JobType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDeviceJob where
  hashWithSalt _salt DescribeDeviceJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeDeviceJob where
  rnf DescribeDeviceJob' {..} = Prelude.rnf jobId

instance Core.ToHeaders DescribeDeviceJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeDeviceJob where
  toPath DescribeDeviceJob' {..} =
    Prelude.mconcat ["/jobs/", Core.toBS jobId]

instance Core.ToQuery DescribeDeviceJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDeviceJobResponse' smart constructor.
data DescribeDeviceJobResponse = DescribeDeviceJobResponse'
  { -- | When the job was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The device\'s ID.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The device\'s name.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The job\'s ID.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The job\'s status.
    status :: Prelude.Maybe UpdateProgress,
    -- | For an OTA job, the target version of the device software.
    imageVersion :: Prelude.Maybe Prelude.Text,
    -- | The device\'s ARN.
    deviceArn :: Prelude.Maybe Prelude.Text,
    -- | The device\'s type.
    deviceType :: Prelude.Maybe DeviceType,
    -- | The job\'s type.
    jobType :: Prelude.Maybe JobType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDeviceJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTime', 'describeDeviceJobResponse_createdTime' - When the job was created.
--
-- 'deviceId', 'describeDeviceJobResponse_deviceId' - The device\'s ID.
--
-- 'deviceName', 'describeDeviceJobResponse_deviceName' - The device\'s name.
--
-- 'jobId', 'describeDeviceJobResponse_jobId' - The job\'s ID.
--
-- 'status', 'describeDeviceJobResponse_status' - The job\'s status.
--
-- 'imageVersion', 'describeDeviceJobResponse_imageVersion' - For an OTA job, the target version of the device software.
--
-- 'deviceArn', 'describeDeviceJobResponse_deviceArn' - The device\'s ARN.
--
-- 'deviceType', 'describeDeviceJobResponse_deviceType' - The device\'s type.
--
-- 'jobType', 'describeDeviceJobResponse_jobType' - The job\'s type.
--
-- 'httpStatus', 'describeDeviceJobResponse_httpStatus' - The response's http status code.
newDescribeDeviceJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDeviceJobResponse
newDescribeDeviceJobResponse pHttpStatus_ =
  DescribeDeviceJobResponse'
    { createdTime =
        Prelude.Nothing,
      deviceId = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      imageVersion = Prelude.Nothing,
      deviceArn = Prelude.Nothing,
      deviceType = Prelude.Nothing,
      jobType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the job was created.
describeDeviceJobResponse_createdTime :: Lens.Lens' DescribeDeviceJobResponse (Prelude.Maybe Prelude.UTCTime)
describeDeviceJobResponse_createdTime = Lens.lens (\DescribeDeviceJobResponse' {createdTime} -> createdTime) (\s@DescribeDeviceJobResponse' {} a -> s {createdTime = a} :: DescribeDeviceJobResponse) Prelude.. Lens.mapping Core._Time

-- | The device\'s ID.
describeDeviceJobResponse_deviceId :: Lens.Lens' DescribeDeviceJobResponse (Prelude.Maybe Prelude.Text)
describeDeviceJobResponse_deviceId = Lens.lens (\DescribeDeviceJobResponse' {deviceId} -> deviceId) (\s@DescribeDeviceJobResponse' {} a -> s {deviceId = a} :: DescribeDeviceJobResponse)

-- | The device\'s name.
describeDeviceJobResponse_deviceName :: Lens.Lens' DescribeDeviceJobResponse (Prelude.Maybe Prelude.Text)
describeDeviceJobResponse_deviceName = Lens.lens (\DescribeDeviceJobResponse' {deviceName} -> deviceName) (\s@DescribeDeviceJobResponse' {} a -> s {deviceName = a} :: DescribeDeviceJobResponse)

-- | The job\'s ID.
describeDeviceJobResponse_jobId :: Lens.Lens' DescribeDeviceJobResponse (Prelude.Maybe Prelude.Text)
describeDeviceJobResponse_jobId = Lens.lens (\DescribeDeviceJobResponse' {jobId} -> jobId) (\s@DescribeDeviceJobResponse' {} a -> s {jobId = a} :: DescribeDeviceJobResponse)

-- | The job\'s status.
describeDeviceJobResponse_status :: Lens.Lens' DescribeDeviceJobResponse (Prelude.Maybe UpdateProgress)
describeDeviceJobResponse_status = Lens.lens (\DescribeDeviceJobResponse' {status} -> status) (\s@DescribeDeviceJobResponse' {} a -> s {status = a} :: DescribeDeviceJobResponse)

-- | For an OTA job, the target version of the device software.
describeDeviceJobResponse_imageVersion :: Lens.Lens' DescribeDeviceJobResponse (Prelude.Maybe Prelude.Text)
describeDeviceJobResponse_imageVersion = Lens.lens (\DescribeDeviceJobResponse' {imageVersion} -> imageVersion) (\s@DescribeDeviceJobResponse' {} a -> s {imageVersion = a} :: DescribeDeviceJobResponse)

-- | The device\'s ARN.
describeDeviceJobResponse_deviceArn :: Lens.Lens' DescribeDeviceJobResponse (Prelude.Maybe Prelude.Text)
describeDeviceJobResponse_deviceArn = Lens.lens (\DescribeDeviceJobResponse' {deviceArn} -> deviceArn) (\s@DescribeDeviceJobResponse' {} a -> s {deviceArn = a} :: DescribeDeviceJobResponse)

-- | The device\'s type.
describeDeviceJobResponse_deviceType :: Lens.Lens' DescribeDeviceJobResponse (Prelude.Maybe DeviceType)
describeDeviceJobResponse_deviceType = Lens.lens (\DescribeDeviceJobResponse' {deviceType} -> deviceType) (\s@DescribeDeviceJobResponse' {} a -> s {deviceType = a} :: DescribeDeviceJobResponse)

-- | The job\'s type.
describeDeviceJobResponse_jobType :: Lens.Lens' DescribeDeviceJobResponse (Prelude.Maybe JobType)
describeDeviceJobResponse_jobType = Lens.lens (\DescribeDeviceJobResponse' {jobType} -> jobType) (\s@DescribeDeviceJobResponse' {} a -> s {jobType = a} :: DescribeDeviceJobResponse)

-- | The response's http status code.
describeDeviceJobResponse_httpStatus :: Lens.Lens' DescribeDeviceJobResponse Prelude.Int
describeDeviceJobResponse_httpStatus = Lens.lens (\DescribeDeviceJobResponse' {httpStatus} -> httpStatus) (\s@DescribeDeviceJobResponse' {} a -> s {httpStatus = a} :: DescribeDeviceJobResponse)

instance Prelude.NFData DescribeDeviceJobResponse where
  rnf DescribeDeviceJobResponse' {..} =
    Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf imageVersion
      `Prelude.seq` Prelude.rnf deviceArn
      `Prelude.seq` Prelude.rnf deviceType
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf httpStatus
