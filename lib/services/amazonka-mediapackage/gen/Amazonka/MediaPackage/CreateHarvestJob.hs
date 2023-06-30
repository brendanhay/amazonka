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
-- Module      : Amazonka.MediaPackage.CreateHarvestJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new HarvestJob record.
module Amazonka.MediaPackage.CreateHarvestJob
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
    createHarvestJobResponse_arn,
    createHarvestJobResponse_channelId,
    createHarvestJobResponse_createdAt,
    createHarvestJobResponse_endTime,
    createHarvestJobResponse_id,
    createHarvestJobResponse_originEndpointId,
    createHarvestJobResponse_s3Destination,
    createHarvestJobResponse_startTime,
    createHarvestJobResponse_status,
    createHarvestJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHarvestJobResponse'
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

instance Prelude.Hashable CreateHarvestJob where
  hashWithSalt _salt CreateHarvestJob' {..} =
    _salt
      `Prelude.hashWithSalt` s3Destination
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` originEndpointId
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` id

instance Prelude.NFData CreateHarvestJob where
  rnf CreateHarvestJob' {..} =
    Prelude.rnf s3Destination
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf originEndpointId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders CreateHarvestJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateHarvestJob where
  toJSON CreateHarvestJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("s3Destination" Data..= s3Destination),
            Prelude.Just ("endTime" Data..= endTime),
            Prelude.Just
              ("originEndpointId" Data..= originEndpointId),
            Prelude.Just ("startTime" Data..= startTime),
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath CreateHarvestJob where
  toPath = Prelude.const "/harvest_jobs"

instance Data.ToQuery CreateHarvestJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateHarvestJobResponse' smart constructor.
data CreateHarvestJobResponse = CreateHarvestJobResponse'
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
-- Create a value of 'CreateHarvestJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createHarvestJobResponse_arn' - The Amazon Resource Name (ARN) assigned to the HarvestJob.
--
-- 'channelId', 'createHarvestJobResponse_channelId' - The ID of the Channel that the HarvestJob will harvest from.
--
-- 'createdAt', 'createHarvestJobResponse_createdAt' - The time the HarvestJob was submitted
--
-- 'endTime', 'createHarvestJobResponse_endTime' - The end of the time-window which will be harvested.
--
-- 'id', 'createHarvestJobResponse_id' - The ID of the HarvestJob. The ID must be unique within the region and it
-- cannot be changed after the HarvestJob is submitted.
--
-- 'originEndpointId', 'createHarvestJobResponse_originEndpointId' - The ID of the OriginEndpoint that the HarvestJob will harvest from. This
-- cannot be changed after the HarvestJob is submitted.
--
-- 's3Destination', 'createHarvestJobResponse_s3Destination' - Undocumented member.
--
-- 'startTime', 'createHarvestJobResponse_startTime' - The start of the time-window which will be harvested.
--
-- 'status', 'createHarvestJobResponse_status' - The current status of the HarvestJob. Consider setting up a CloudWatch
-- Event to listen for HarvestJobs as they succeed or fail. In the event of
-- failure, the CloudWatch Event will include an explanation of why the
-- HarvestJob failed.
--
-- 'httpStatus', 'createHarvestJobResponse_httpStatus' - The response's http status code.
newCreateHarvestJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateHarvestJobResponse
newCreateHarvestJobResponse pHttpStatus_ =
  CreateHarvestJobResponse'
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
createHarvestJobResponse_arn :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Prelude.Text)
createHarvestJobResponse_arn = Lens.lens (\CreateHarvestJobResponse' {arn} -> arn) (\s@CreateHarvestJobResponse' {} a -> s {arn = a} :: CreateHarvestJobResponse)

-- | The ID of the Channel that the HarvestJob will harvest from.
createHarvestJobResponse_channelId :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Prelude.Text)
createHarvestJobResponse_channelId = Lens.lens (\CreateHarvestJobResponse' {channelId} -> channelId) (\s@CreateHarvestJobResponse' {} a -> s {channelId = a} :: CreateHarvestJobResponse)

-- | The time the HarvestJob was submitted
createHarvestJobResponse_createdAt :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Prelude.Text)
createHarvestJobResponse_createdAt = Lens.lens (\CreateHarvestJobResponse' {createdAt} -> createdAt) (\s@CreateHarvestJobResponse' {} a -> s {createdAt = a} :: CreateHarvestJobResponse)

-- | The end of the time-window which will be harvested.
createHarvestJobResponse_endTime :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Prelude.Text)
createHarvestJobResponse_endTime = Lens.lens (\CreateHarvestJobResponse' {endTime} -> endTime) (\s@CreateHarvestJobResponse' {} a -> s {endTime = a} :: CreateHarvestJobResponse)

-- | The ID of the HarvestJob. The ID must be unique within the region and it
-- cannot be changed after the HarvestJob is submitted.
createHarvestJobResponse_id :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Prelude.Text)
createHarvestJobResponse_id = Lens.lens (\CreateHarvestJobResponse' {id} -> id) (\s@CreateHarvestJobResponse' {} a -> s {id = a} :: CreateHarvestJobResponse)

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from. This
-- cannot be changed after the HarvestJob is submitted.
createHarvestJobResponse_originEndpointId :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Prelude.Text)
createHarvestJobResponse_originEndpointId = Lens.lens (\CreateHarvestJobResponse' {originEndpointId} -> originEndpointId) (\s@CreateHarvestJobResponse' {} a -> s {originEndpointId = a} :: CreateHarvestJobResponse)

-- | Undocumented member.
createHarvestJobResponse_s3Destination :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe S3Destination)
createHarvestJobResponse_s3Destination = Lens.lens (\CreateHarvestJobResponse' {s3Destination} -> s3Destination) (\s@CreateHarvestJobResponse' {} a -> s {s3Destination = a} :: CreateHarvestJobResponse)

-- | The start of the time-window which will be harvested.
createHarvestJobResponse_startTime :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Prelude.Text)
createHarvestJobResponse_startTime = Lens.lens (\CreateHarvestJobResponse' {startTime} -> startTime) (\s@CreateHarvestJobResponse' {} a -> s {startTime = a} :: CreateHarvestJobResponse)

-- | The current status of the HarvestJob. Consider setting up a CloudWatch
-- Event to listen for HarvestJobs as they succeed or fail. In the event of
-- failure, the CloudWatch Event will include an explanation of why the
-- HarvestJob failed.
createHarvestJobResponse_status :: Lens.Lens' CreateHarvestJobResponse (Prelude.Maybe Status)
createHarvestJobResponse_status = Lens.lens (\CreateHarvestJobResponse' {status} -> status) (\s@CreateHarvestJobResponse' {} a -> s {status = a} :: CreateHarvestJobResponse)

-- | The response's http status code.
createHarvestJobResponse_httpStatus :: Lens.Lens' CreateHarvestJobResponse Prelude.Int
createHarvestJobResponse_httpStatus = Lens.lens (\CreateHarvestJobResponse' {httpStatus} -> httpStatus) (\s@CreateHarvestJobResponse' {} a -> s {httpStatus = a} :: CreateHarvestJobResponse)

instance Prelude.NFData CreateHarvestJobResponse where
  rnf CreateHarvestJobResponse' {..} =
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
