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
-- Module      : Amazonka.MediaPackage.DescribeHarvestJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about an existing HarvestJob.
module Amazonka.MediaPackage.DescribeHarvestJob
  ( -- * Creating a Request
    DescribeHarvestJob (..),
    newDescribeHarvestJob,

    -- * Request Lenses
    describeHarvestJob_id,

    -- * Destructuring the Response
    DescribeHarvestJobResponse (..),
    newDescribeHarvestJobResponse,

    -- * Response Lenses
    describeHarvestJobResponse_arn,
    describeHarvestJobResponse_channelId,
    describeHarvestJobResponse_createdAt,
    describeHarvestJobResponse_endTime,
    describeHarvestJobResponse_id,
    describeHarvestJobResponse_originEndpointId,
    describeHarvestJobResponse_s3Destination,
    describeHarvestJobResponse_startTime,
    describeHarvestJobResponse_status,
    describeHarvestJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeHarvestJob' smart constructor.
data DescribeHarvestJob = DescribeHarvestJob'
  { -- | The ID of the HarvestJob.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHarvestJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeHarvestJob_id' - The ID of the HarvestJob.
newDescribeHarvestJob ::
  -- | 'id'
  Prelude.Text ->
  DescribeHarvestJob
newDescribeHarvestJob pId_ =
  DescribeHarvestJob' {id = pId_}

-- | The ID of the HarvestJob.
describeHarvestJob_id :: Lens.Lens' DescribeHarvestJob Prelude.Text
describeHarvestJob_id = Lens.lens (\DescribeHarvestJob' {id} -> id) (\s@DescribeHarvestJob' {} a -> s {id = a} :: DescribeHarvestJob)

instance Core.AWSRequest DescribeHarvestJob where
  type
    AWSResponse DescribeHarvestJob =
      DescribeHarvestJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHarvestJobResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "channelId")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "endTime")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "originEndpointId")
            Prelude.<*> (x Data..?> "s3Destination")
            Prelude.<*> (x Data..?> "startTime")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeHarvestJob where
  hashWithSalt _salt DescribeHarvestJob' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeHarvestJob where
  rnf DescribeHarvestJob' {..} = Prelude.rnf id

instance Data.ToHeaders DescribeHarvestJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeHarvestJob where
  toPath DescribeHarvestJob' {..} =
    Prelude.mconcat ["/harvest_jobs/", Data.toBS id]

instance Data.ToQuery DescribeHarvestJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeHarvestJobResponse' smart constructor.
data DescribeHarvestJobResponse = DescribeHarvestJobResponse'
  { -- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Channel that the HarvestJob will harvest from.
    channelId :: Prelude.Maybe Prelude.Text,
    -- | The time the HarvestJob was submitted
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The end of the time-window which will be harvested.
    endTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the HarvestJob. The ID must be unique within the region and it
    -- cannot be changed after the HarvestJob is submitted.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the OriginEndpoint that the HarvestJob will harvest from. This
    -- cannot be changed after the HarvestJob is submitted.
    originEndpointId :: Prelude.Maybe Prelude.Text,
    s3Destination :: Prelude.Maybe S3Destination,
    -- | The start of the time-window which will be harvested.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The current status of the HarvestJob. Consider setting up a CloudWatch
    -- Event to listen for HarvestJobs as they succeed or fail. In the event of
    -- failure, the CloudWatch Event will include an explanation of why the
    -- HarvestJob failed.
    status :: Prelude.Maybe Status,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHarvestJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeHarvestJobResponse_arn' - The Amazon Resource Name (ARN) assigned to the HarvestJob.
--
-- 'channelId', 'describeHarvestJobResponse_channelId' - The ID of the Channel that the HarvestJob will harvest from.
--
-- 'createdAt', 'describeHarvestJobResponse_createdAt' - The time the HarvestJob was submitted
--
-- 'endTime', 'describeHarvestJobResponse_endTime' - The end of the time-window which will be harvested.
--
-- 'id', 'describeHarvestJobResponse_id' - The ID of the HarvestJob. The ID must be unique within the region and it
-- cannot be changed after the HarvestJob is submitted.
--
-- 'originEndpointId', 'describeHarvestJobResponse_originEndpointId' - The ID of the OriginEndpoint that the HarvestJob will harvest from. This
-- cannot be changed after the HarvestJob is submitted.
--
-- 's3Destination', 'describeHarvestJobResponse_s3Destination' - Undocumented member.
--
-- 'startTime', 'describeHarvestJobResponse_startTime' - The start of the time-window which will be harvested.
--
-- 'status', 'describeHarvestJobResponse_status' - The current status of the HarvestJob. Consider setting up a CloudWatch
-- Event to listen for HarvestJobs as they succeed or fail. In the event of
-- failure, the CloudWatch Event will include an explanation of why the
-- HarvestJob failed.
--
-- 'httpStatus', 'describeHarvestJobResponse_httpStatus' - The response's http status code.
newDescribeHarvestJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeHarvestJobResponse
newDescribeHarvestJobResponse pHttpStatus_ =
  DescribeHarvestJobResponse'
    { arn = Prelude.Nothing,
      channelId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      endTime = Prelude.Nothing,
      id = Prelude.Nothing,
      originEndpointId = Prelude.Nothing,
      s3Destination = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
describeHarvestJobResponse_arn :: Lens.Lens' DescribeHarvestJobResponse (Prelude.Maybe Prelude.Text)
describeHarvestJobResponse_arn = Lens.lens (\DescribeHarvestJobResponse' {arn} -> arn) (\s@DescribeHarvestJobResponse' {} a -> s {arn = a} :: DescribeHarvestJobResponse)

-- | The ID of the Channel that the HarvestJob will harvest from.
describeHarvestJobResponse_channelId :: Lens.Lens' DescribeHarvestJobResponse (Prelude.Maybe Prelude.Text)
describeHarvestJobResponse_channelId = Lens.lens (\DescribeHarvestJobResponse' {channelId} -> channelId) (\s@DescribeHarvestJobResponse' {} a -> s {channelId = a} :: DescribeHarvestJobResponse)

-- | The time the HarvestJob was submitted
describeHarvestJobResponse_createdAt :: Lens.Lens' DescribeHarvestJobResponse (Prelude.Maybe Prelude.Text)
describeHarvestJobResponse_createdAt = Lens.lens (\DescribeHarvestJobResponse' {createdAt} -> createdAt) (\s@DescribeHarvestJobResponse' {} a -> s {createdAt = a} :: DescribeHarvestJobResponse)

-- | The end of the time-window which will be harvested.
describeHarvestJobResponse_endTime :: Lens.Lens' DescribeHarvestJobResponse (Prelude.Maybe Prelude.Text)
describeHarvestJobResponse_endTime = Lens.lens (\DescribeHarvestJobResponse' {endTime} -> endTime) (\s@DescribeHarvestJobResponse' {} a -> s {endTime = a} :: DescribeHarvestJobResponse)

-- | The ID of the HarvestJob. The ID must be unique within the region and it
-- cannot be changed after the HarvestJob is submitted.
describeHarvestJobResponse_id :: Lens.Lens' DescribeHarvestJobResponse (Prelude.Maybe Prelude.Text)
describeHarvestJobResponse_id = Lens.lens (\DescribeHarvestJobResponse' {id} -> id) (\s@DescribeHarvestJobResponse' {} a -> s {id = a} :: DescribeHarvestJobResponse)

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from. This
-- cannot be changed after the HarvestJob is submitted.
describeHarvestJobResponse_originEndpointId :: Lens.Lens' DescribeHarvestJobResponse (Prelude.Maybe Prelude.Text)
describeHarvestJobResponse_originEndpointId = Lens.lens (\DescribeHarvestJobResponse' {originEndpointId} -> originEndpointId) (\s@DescribeHarvestJobResponse' {} a -> s {originEndpointId = a} :: DescribeHarvestJobResponse)

-- | Undocumented member.
describeHarvestJobResponse_s3Destination :: Lens.Lens' DescribeHarvestJobResponse (Prelude.Maybe S3Destination)
describeHarvestJobResponse_s3Destination = Lens.lens (\DescribeHarvestJobResponse' {s3Destination} -> s3Destination) (\s@DescribeHarvestJobResponse' {} a -> s {s3Destination = a} :: DescribeHarvestJobResponse)

-- | The start of the time-window which will be harvested.
describeHarvestJobResponse_startTime :: Lens.Lens' DescribeHarvestJobResponse (Prelude.Maybe Prelude.Text)
describeHarvestJobResponse_startTime = Lens.lens (\DescribeHarvestJobResponse' {startTime} -> startTime) (\s@DescribeHarvestJobResponse' {} a -> s {startTime = a} :: DescribeHarvestJobResponse)

-- | The current status of the HarvestJob. Consider setting up a CloudWatch
-- Event to listen for HarvestJobs as they succeed or fail. In the event of
-- failure, the CloudWatch Event will include an explanation of why the
-- HarvestJob failed.
describeHarvestJobResponse_status :: Lens.Lens' DescribeHarvestJobResponse (Prelude.Maybe Status)
describeHarvestJobResponse_status = Lens.lens (\DescribeHarvestJobResponse' {status} -> status) (\s@DescribeHarvestJobResponse' {} a -> s {status = a} :: DescribeHarvestJobResponse)

-- | The response's http status code.
describeHarvestJobResponse_httpStatus :: Lens.Lens' DescribeHarvestJobResponse Prelude.Int
describeHarvestJobResponse_httpStatus = Lens.lens (\DescribeHarvestJobResponse' {httpStatus} -> httpStatus) (\s@DescribeHarvestJobResponse' {} a -> s {httpStatus = a} :: DescribeHarvestJobResponse)

instance Prelude.NFData DescribeHarvestJobResponse where
  rnf DescribeHarvestJobResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf channelId
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf originEndpointId
      `Prelude.seq` Prelude.rnf s3Destination
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
