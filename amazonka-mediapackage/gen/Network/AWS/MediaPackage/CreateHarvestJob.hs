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
-- Module      : Network.AWS.MediaPackage.CreateHarvestJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new HarvestJob record.
module Network.AWS.MediaPackage.CreateHarvestJob
  ( -- * Creating a Request
    CreateHarvestJob (..),
    newCreateHarvestJob,

    -- * Request Lenses
    createHarvestJob_s3Destination,
    createHarvestJob_endTime,
    createHarvestJob_originEndpointId,
    createHarvestJob_startTime,
    createHarvestJob_id,

    -- * Destructuring the Response
    CreateHarvestJobResponse (..),
    newCreateHarvestJobResponse,

    -- * Response Lenses
    createHarvestJobResponse_status,
    createHarvestJobResponse_s3Destination,
    createHarvestJobResponse_channelId,
    createHarvestJobResponse_startTime,
    createHarvestJobResponse_arn,
    createHarvestJobResponse_id,
    createHarvestJobResponse_createdAt,
    createHarvestJobResponse_originEndpointId,
    createHarvestJobResponse_endTime,
    createHarvestJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Configuration parameters used to create a new HarvestJob.
--
-- /See:/ 'newCreateHarvestJob' smart constructor.
data CreateHarvestJob = CreateHarvestJob'
  { s3Destination :: S3Destination,
    -- | The end of the time-window which will be harvested
    endTime :: Prelude.Text,
    -- | The ID of the OriginEndpoint that the HarvestJob will harvest from. This
    -- cannot be changed after the HarvestJob is submitted.
    originEndpointId :: Prelude.Text,
    -- | The start of the time-window which will be harvested
    startTime :: Prelude.Text,
    -- | The ID of the HarvestJob. The ID must be unique within the region and it
    -- cannot be changed after the HarvestJob is submitted
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHarvestJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Destination', 'createHarvestJob_s3Destination' - Undocumented member.
--
-- 'endTime', 'createHarvestJob_endTime' - The end of the time-window which will be harvested
--
-- 'originEndpointId', 'createHarvestJob_originEndpointId' - The ID of the OriginEndpoint that the HarvestJob will harvest from. This
-- cannot be changed after the HarvestJob is submitted.
--
-- 'startTime', 'createHarvestJob_startTime' - The start of the time-window which will be harvested
--
-- 'id', 'createHarvestJob_id' - The ID of the HarvestJob. The ID must be unique within the region and it
-- cannot be changed after the HarvestJob is submitted
newCreateHarvestJob ::
  -- | 's3Destination'
  S3Destination ->
  -- | 'endTime'
  Prelude.Text ->
  -- | 'originEndpointId'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  CreateHarvestJob
newCreateHarvestJob
  pS3Destination_
  pEndTime_
  pOriginEndpointId_
  pStartTime_
  pId_ =
    CreateHarvestJob'
      { s3Destination = pS3Destination_,
        endTime = pEndTime_,
        originEndpointId = pOriginEndpointId_,
        startTime = pStartTime_,
        id = pId_
      }

-- | Undocumented member.
createHarvestJob_s3Destination :: Lens.Lens' CreateHarvestJob S3Destination
createHarvestJob_s3Destination = Lens.lens (\CreateHarvestJob' {s3Destination} -> s3Destination) (\s@CreateHarvestJob' {} a -> s {s3Destination = a} :: CreateHarvestJob)

-- | The end of the time-window which will be harvested
createHarvestJob_endTime :: Lens.Lens' CreateHarvestJob Prelude.Text
createHarvestJob_endTime = Lens.lens (\CreateHarvestJob' {endTime} -> endTime) (\s@CreateHarvestJob' {} a -> s {endTime = a} :: CreateHarvestJob)

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from. This
-- cannot be changed after the HarvestJob is submitted.
createHarvestJob_originEndpointId :: Lens.Lens' CreateHarvestJob Prelude.Text
createHarvestJob_originEndpointId = Lens.lens (\CreateHarvestJob' {originEndpointId} -> originEndpointId) (\s@CreateHarvestJob' {} a -> s {originEndpointId = a} :: CreateHarvestJob)

-- | The start of the time-window which will be harvested
createHarvestJob_startTime :: Lens.Lens' CreateHarvestJob Prelude.Text
createHarvestJob_startTime = Lens.lens (\CreateHarvestJob' {startTime} -> startTime) (\s@CreateHarvestJob' {} a -> s {startTime = a} :: CreateHarvestJob)

-- | The ID of the HarvestJob. The ID must be unique within the region and it
-- cannot be changed after the HarvestJob is submitted
createHarvestJob_id :: Lens.Lens' CreateHarvestJob Prelude.Text
createHarvestJob_id = Lens.lens (\CreateHarvestJob' {id} -> id) (\s@CreateHarvestJob' {} a -> s {id = a} :: CreateHarvestJob)

instance Core.AWSRequest CreateHarvestJob where
  type
    AWSResponse CreateHarvestJob =
      CreateHarvestJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHarvestJobResponse'
            Prelude.<$> (x Core..?> "status")
            Prelude.<*> (x Core..?> "s3Destination")
            Prelude.<*> (x Core..?> "channelId")
            Prelude.<*> (x Core..?> "startTime")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (x Core..?> "originEndpointId")
            Prelude.<*> (x Core..?> "endTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateHarvestJob

instance Prelude.NFData CreateHarvestJob

instance Core.ToHeaders CreateHarvestJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateHarvestJob where
  toJSON CreateHarvestJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("s3Destination" Core..= s3Destination),
            Prelude.Just ("endTime" Core..= endTime),
            Prelude.Just
              ("originEndpointId" Core..= originEndpointId),
            Prelude.Just ("startTime" Core..= startTime),
            Prelude.Just ("id" Core..= id)
          ]
      )

instance Core.ToPath CreateHarvestJob where
  toPath = Prelude.const "/harvest_jobs"

instance Core.ToQuery CreateHarvestJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateHarvestJobResponse' smart constructor.
data CreateHarvestJobResponse = CreateHarvestJobResponse'
  { -- | The current status of the HarvestJob. Consider setting up a CloudWatch
    -- Event to listen for HarvestJobs as they succeed or fail. In the event of
    -- failure, the CloudWatch Event will include an explanation of why the
    -- HarvestJob failed.
    status :: Prelude.Maybe Status,
    s3Destination :: Prelude.Maybe S3Destination,
    -- | The ID of the Channel that the HarvestJob will harvest from.
    channelId :: Prelude.Maybe Prelude.Text,
    -- | The start of the time-window which will be harvested.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the HarvestJob. The ID must be unique within the region and it
    -- cannot be changed after the HarvestJob is submitted.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time the HarvestJob was submitted
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The ID of the OriginEndpoint that the HarvestJob will harvest from. This
    -- cannot be changed after the HarvestJob is submitted.
    originEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The end of the time-window which will be harvested.
    endTime :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHarvestJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'createHarvestJobResponse_status' - The current status of the HarvestJob. Consider setting up a CloudWatch
-- Event to listen for HarvestJobs as they succeed or fail. In the event of
-- failure, the CloudWatch Event will include an explanation of why the
-- HarvestJob failed.
--
-- 's3Destination', 'createHarvestJobResponse_s3Destination' - Undocumented member.
--
-- 'channelId', 'createHarvestJobResponse_channelId' - The ID of the Channel that the HarvestJob will harvest from.
--
-- 'startTime', 'createHarvestJobResponse_startTime' - The start of the time-window which will be harvested.
--
-- 'arn', 'createHarvestJobResponse_arn' - The Amazon Resource Name (ARN) assigned to the HarvestJob.
--
-- 'id', 'createHarvestJobResponse_id' - The ID of the HarvestJob. The ID must be unique within the region and it
-- cannot be changed after the HarvestJob is submitted.
--
-- 'createdAt', 'createHarvestJobResponse_createdAt' - The time the HarvestJob was submitted
--
-- 'originEndpointId', 'createHarvestJobResponse_originEndpointId' - The ID of the OriginEndpoint that the HarvestJob will harvest from. This
-- cannot be changed after the HarvestJob is submitted.
--
-- 'endTime', 'createHarvestJobResponse_endTime' - The end of the time-window which will be harvested.
--
-- 'httpStatus', 'createHarvestJobResponse_httpStatus' - The response's http status code.
newCreateHarvestJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateHarvestJobResponse
newCreateHarvestJobResponse pHttpStatus_ =
  CreateHarvestJobResponse'
    { status = Prelude.Nothing,
      s3Destination = Prelude.Nothing,
      channelId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      originEndpointId = Prelude.Nothing,
      endTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the HarvestJob. Consider setting up a CloudWatch
-- Event to listen for HarvestJobs as they succeed or fail. In the event of
-- failure, the CloudWatch Event will include an explanation of why the
-- HarvestJob failed.
createHarvestJobResponse_status :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Status)
createHarvestJobResponse_status = Lens.lens (\CreateHarvestJobResponse' {status} -> status) (\s@CreateHarvestJobResponse' {} a -> s {status = a} :: CreateHarvestJobResponse)

-- | Undocumented member.
createHarvestJobResponse_s3Destination :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe S3Destination)
createHarvestJobResponse_s3Destination = Lens.lens (\CreateHarvestJobResponse' {s3Destination} -> s3Destination) (\s@CreateHarvestJobResponse' {} a -> s {s3Destination = a} :: CreateHarvestJobResponse)

-- | The ID of the Channel that the HarvestJob will harvest from.
createHarvestJobResponse_channelId :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Prelude.Text)
createHarvestJobResponse_channelId = Lens.lens (\CreateHarvestJobResponse' {channelId} -> channelId) (\s@CreateHarvestJobResponse' {} a -> s {channelId = a} :: CreateHarvestJobResponse)

-- | The start of the time-window which will be harvested.
createHarvestJobResponse_startTime :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Prelude.Text)
createHarvestJobResponse_startTime = Lens.lens (\CreateHarvestJobResponse' {startTime} -> startTime) (\s@CreateHarvestJobResponse' {} a -> s {startTime = a} :: CreateHarvestJobResponse)

-- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
createHarvestJobResponse_arn :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Prelude.Text)
createHarvestJobResponse_arn = Lens.lens (\CreateHarvestJobResponse' {arn} -> arn) (\s@CreateHarvestJobResponse' {} a -> s {arn = a} :: CreateHarvestJobResponse)

-- | The ID of the HarvestJob. The ID must be unique within the region and it
-- cannot be changed after the HarvestJob is submitted.
createHarvestJobResponse_id :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Prelude.Text)
createHarvestJobResponse_id = Lens.lens (\CreateHarvestJobResponse' {id} -> id) (\s@CreateHarvestJobResponse' {} a -> s {id = a} :: CreateHarvestJobResponse)

-- | The time the HarvestJob was submitted
createHarvestJobResponse_createdAt :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Prelude.Text)
createHarvestJobResponse_createdAt = Lens.lens (\CreateHarvestJobResponse' {createdAt} -> createdAt) (\s@CreateHarvestJobResponse' {} a -> s {createdAt = a} :: CreateHarvestJobResponse)

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from. This
-- cannot be changed after the HarvestJob is submitted.
createHarvestJobResponse_originEndpointId :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Prelude.Text)
createHarvestJobResponse_originEndpointId = Lens.lens (\CreateHarvestJobResponse' {originEndpointId} -> originEndpointId) (\s@CreateHarvestJobResponse' {} a -> s {originEndpointId = a} :: CreateHarvestJobResponse)

-- | The end of the time-window which will be harvested.
createHarvestJobResponse_endTime :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Prelude.Text)
createHarvestJobResponse_endTime = Lens.lens (\CreateHarvestJobResponse' {endTime} -> endTime) (\s@CreateHarvestJobResponse' {} a -> s {endTime = a} :: CreateHarvestJobResponse)

-- | The response's http status code.
createHarvestJobResponse_httpStatus :: Lens.Lens' CreateHarvestJobResponse Prelude.Int
createHarvestJobResponse_httpStatus = Lens.lens (\CreateHarvestJobResponse' {httpStatus} -> httpStatus) (\s@CreateHarvestJobResponse' {} a -> s {httpStatus = a} :: CreateHarvestJobResponse)

instance Prelude.NFData CreateHarvestJobResponse
