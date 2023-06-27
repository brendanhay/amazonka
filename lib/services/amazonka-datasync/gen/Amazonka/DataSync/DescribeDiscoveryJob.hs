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
-- Module      : Amazonka.DataSync.DescribeDiscoveryJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a DataSync discovery job.
module Amazonka.DataSync.DescribeDiscoveryJob
  ( -- * Creating a Request
    DescribeDiscoveryJob (..),
    newDescribeDiscoveryJob,

    -- * Request Lenses
    describeDiscoveryJob_discoveryJobArn,

    -- * Destructuring the Response
    DescribeDiscoveryJobResponse (..),
    newDescribeDiscoveryJobResponse,

    -- * Response Lenses
    describeDiscoveryJobResponse_collectionDurationMinutes,
    describeDiscoveryJobResponse_discoveryJobArn,
    describeDiscoveryJobResponse_jobEndTime,
    describeDiscoveryJobResponse_jobStartTime,
    describeDiscoveryJobResponse_status,
    describeDiscoveryJobResponse_storageSystemArn,
    describeDiscoveryJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDiscoveryJob' smart constructor.
data DescribeDiscoveryJob = DescribeDiscoveryJob'
  { -- | Specifies the Amazon Resource Name (ARN) of the discovery job that you
    -- want information about.
    discoveryJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDiscoveryJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discoveryJobArn', 'describeDiscoveryJob_discoveryJobArn' - Specifies the Amazon Resource Name (ARN) of the discovery job that you
-- want information about.
newDescribeDiscoveryJob ::
  -- | 'discoveryJobArn'
  Prelude.Text ->
  DescribeDiscoveryJob
newDescribeDiscoveryJob pDiscoveryJobArn_ =
  DescribeDiscoveryJob'
    { discoveryJobArn =
        pDiscoveryJobArn_
    }

-- | Specifies the Amazon Resource Name (ARN) of the discovery job that you
-- want information about.
describeDiscoveryJob_discoveryJobArn :: Lens.Lens' DescribeDiscoveryJob Prelude.Text
describeDiscoveryJob_discoveryJobArn = Lens.lens (\DescribeDiscoveryJob' {discoveryJobArn} -> discoveryJobArn) (\s@DescribeDiscoveryJob' {} a -> s {discoveryJobArn = a} :: DescribeDiscoveryJob)

instance Core.AWSRequest DescribeDiscoveryJob where
  type
    AWSResponse DescribeDiscoveryJob =
      DescribeDiscoveryJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDiscoveryJobResponse'
            Prelude.<$> (x Data..?> "CollectionDurationMinutes")
            Prelude.<*> (x Data..?> "DiscoveryJobArn")
            Prelude.<*> (x Data..?> "JobEndTime")
            Prelude.<*> (x Data..?> "JobStartTime")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "StorageSystemArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDiscoveryJob where
  hashWithSalt _salt DescribeDiscoveryJob' {..} =
    _salt `Prelude.hashWithSalt` discoveryJobArn

instance Prelude.NFData DescribeDiscoveryJob where
  rnf DescribeDiscoveryJob' {..} =
    Prelude.rnf discoveryJobArn

instance Data.ToHeaders DescribeDiscoveryJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.DescribeDiscoveryJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDiscoveryJob where
  toJSON DescribeDiscoveryJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DiscoveryJobArn" Data..= discoveryJobArn)
          ]
      )

instance Data.ToPath DescribeDiscoveryJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDiscoveryJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDiscoveryJobResponse' smart constructor.
data DescribeDiscoveryJobResponse = DescribeDiscoveryJobResponse'
  { -- | The number of minutes that the discovery job runs.
    collectionDurationMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the discovery job.
    discoveryJobArn :: Prelude.Maybe Prelude.Text,
    -- | The time when the discovery job ended.
    jobEndTime :: Prelude.Maybe Data.POSIX,
    -- | The time when the discovery job started.
    jobStartTime :: Prelude.Maybe Data.POSIX,
    -- | Indicates the status of a discovery job. For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#discovery-job-statuses-table Discovery job statuses>.
    status :: Prelude.Maybe DiscoveryJobStatus,
    -- | The ARN of the on-premises storage system you\'re running the discovery
    -- job on.
    storageSystemArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDiscoveryJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionDurationMinutes', 'describeDiscoveryJobResponse_collectionDurationMinutes' - The number of minutes that the discovery job runs.
--
-- 'discoveryJobArn', 'describeDiscoveryJobResponse_discoveryJobArn' - The ARN of the discovery job.
--
-- 'jobEndTime', 'describeDiscoveryJobResponse_jobEndTime' - The time when the discovery job ended.
--
-- 'jobStartTime', 'describeDiscoveryJobResponse_jobStartTime' - The time when the discovery job started.
--
-- 'status', 'describeDiscoveryJobResponse_status' - Indicates the status of a discovery job. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#discovery-job-statuses-table Discovery job statuses>.
--
-- 'storageSystemArn', 'describeDiscoveryJobResponse_storageSystemArn' - The ARN of the on-premises storage system you\'re running the discovery
-- job on.
--
-- 'httpStatus', 'describeDiscoveryJobResponse_httpStatus' - The response's http status code.
newDescribeDiscoveryJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDiscoveryJobResponse
newDescribeDiscoveryJobResponse pHttpStatus_ =
  DescribeDiscoveryJobResponse'
    { collectionDurationMinutes =
        Prelude.Nothing,
      discoveryJobArn = Prelude.Nothing,
      jobEndTime = Prelude.Nothing,
      jobStartTime = Prelude.Nothing,
      status = Prelude.Nothing,
      storageSystemArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of minutes that the discovery job runs.
describeDiscoveryJobResponse_collectionDurationMinutes :: Lens.Lens' DescribeDiscoveryJobResponse (Prelude.Maybe Prelude.Natural)
describeDiscoveryJobResponse_collectionDurationMinutes = Lens.lens (\DescribeDiscoveryJobResponse' {collectionDurationMinutes} -> collectionDurationMinutes) (\s@DescribeDiscoveryJobResponse' {} a -> s {collectionDurationMinutes = a} :: DescribeDiscoveryJobResponse)

-- | The ARN of the discovery job.
describeDiscoveryJobResponse_discoveryJobArn :: Lens.Lens' DescribeDiscoveryJobResponse (Prelude.Maybe Prelude.Text)
describeDiscoveryJobResponse_discoveryJobArn = Lens.lens (\DescribeDiscoveryJobResponse' {discoveryJobArn} -> discoveryJobArn) (\s@DescribeDiscoveryJobResponse' {} a -> s {discoveryJobArn = a} :: DescribeDiscoveryJobResponse)

-- | The time when the discovery job ended.
describeDiscoveryJobResponse_jobEndTime :: Lens.Lens' DescribeDiscoveryJobResponse (Prelude.Maybe Prelude.UTCTime)
describeDiscoveryJobResponse_jobEndTime = Lens.lens (\DescribeDiscoveryJobResponse' {jobEndTime} -> jobEndTime) (\s@DescribeDiscoveryJobResponse' {} a -> s {jobEndTime = a} :: DescribeDiscoveryJobResponse) Prelude.. Lens.mapping Data._Time

-- | The time when the discovery job started.
describeDiscoveryJobResponse_jobStartTime :: Lens.Lens' DescribeDiscoveryJobResponse (Prelude.Maybe Prelude.UTCTime)
describeDiscoveryJobResponse_jobStartTime = Lens.lens (\DescribeDiscoveryJobResponse' {jobStartTime} -> jobStartTime) (\s@DescribeDiscoveryJobResponse' {} a -> s {jobStartTime = a} :: DescribeDiscoveryJobResponse) Prelude.. Lens.mapping Data._Time

-- | Indicates the status of a discovery job. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#discovery-job-statuses-table Discovery job statuses>.
describeDiscoveryJobResponse_status :: Lens.Lens' DescribeDiscoveryJobResponse (Prelude.Maybe DiscoveryJobStatus)
describeDiscoveryJobResponse_status = Lens.lens (\DescribeDiscoveryJobResponse' {status} -> status) (\s@DescribeDiscoveryJobResponse' {} a -> s {status = a} :: DescribeDiscoveryJobResponse)

-- | The ARN of the on-premises storage system you\'re running the discovery
-- job on.
describeDiscoveryJobResponse_storageSystemArn :: Lens.Lens' DescribeDiscoveryJobResponse (Prelude.Maybe Prelude.Text)
describeDiscoveryJobResponse_storageSystemArn = Lens.lens (\DescribeDiscoveryJobResponse' {storageSystemArn} -> storageSystemArn) (\s@DescribeDiscoveryJobResponse' {} a -> s {storageSystemArn = a} :: DescribeDiscoveryJobResponse)

-- | The response's http status code.
describeDiscoveryJobResponse_httpStatus :: Lens.Lens' DescribeDiscoveryJobResponse Prelude.Int
describeDiscoveryJobResponse_httpStatus = Lens.lens (\DescribeDiscoveryJobResponse' {httpStatus} -> httpStatus) (\s@DescribeDiscoveryJobResponse' {} a -> s {httpStatus = a} :: DescribeDiscoveryJobResponse)

instance Prelude.NFData DescribeDiscoveryJobResponse where
  rnf DescribeDiscoveryJobResponse' {..} =
    Prelude.rnf collectionDurationMinutes
      `Prelude.seq` Prelude.rnf discoveryJobArn
      `Prelude.seq` Prelude.rnf jobEndTime
      `Prelude.seq` Prelude.rnf jobStartTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf storageSystemArn
      `Prelude.seq` Prelude.rnf httpStatus
