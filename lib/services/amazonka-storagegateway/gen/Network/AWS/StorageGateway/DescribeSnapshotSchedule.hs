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
-- Module      : Network.AWS.StorageGateway.DescribeSnapshotSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the snapshot schedule for the specified gateway volume. The
-- snapshot schedule information includes intervals at which snapshots are
-- automatically initiated on the volume. This operation is only supported
-- in the cached volume and stored volume types.
module Network.AWS.StorageGateway.DescribeSnapshotSchedule
  ( -- * Creating a Request
    DescribeSnapshotSchedule (..),
    newDescribeSnapshotSchedule,

    -- * Request Lenses
    describeSnapshotSchedule_volumeARN,

    -- * Destructuring the Response
    DescribeSnapshotScheduleResponse (..),
    newDescribeSnapshotScheduleResponse,

    -- * Response Lenses
    describeSnapshotScheduleResponse_startAt,
    describeSnapshotScheduleResponse_volumeARN,
    describeSnapshotScheduleResponse_recurrenceInHours,
    describeSnapshotScheduleResponse_timezone,
    describeSnapshotScheduleResponse_description,
    describeSnapshotScheduleResponse_tags,
    describeSnapshotScheduleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the DescribeSnapshotScheduleInput$VolumeARN of
-- the volume.
--
-- /See:/ 'newDescribeSnapshotSchedule' smart constructor.
data DescribeSnapshotSchedule = DescribeSnapshotSchedule'
  { -- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
    -- operation to return a list of gateway volumes.
    volumeARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSnapshotSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARN', 'describeSnapshotSchedule_volumeARN' - The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
newDescribeSnapshotSchedule ::
  -- | 'volumeARN'
  Prelude.Text ->
  DescribeSnapshotSchedule
newDescribeSnapshotSchedule pVolumeARN_ =
  DescribeSnapshotSchedule' {volumeARN = pVolumeARN_}

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
describeSnapshotSchedule_volumeARN :: Lens.Lens' DescribeSnapshotSchedule Prelude.Text
describeSnapshotSchedule_volumeARN = Lens.lens (\DescribeSnapshotSchedule' {volumeARN} -> volumeARN) (\s@DescribeSnapshotSchedule' {} a -> s {volumeARN = a} :: DescribeSnapshotSchedule)

instance Core.AWSRequest DescribeSnapshotSchedule where
  type
    AWSResponse DescribeSnapshotSchedule =
      DescribeSnapshotScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSnapshotScheduleResponse'
            Prelude.<$> (x Core..?> "StartAt")
            Prelude.<*> (x Core..?> "VolumeARN")
            Prelude.<*> (x Core..?> "RecurrenceInHours")
            Prelude.<*> (x Core..?> "Timezone")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSnapshotSchedule

instance Prelude.NFData DescribeSnapshotSchedule

instance Core.ToHeaders DescribeSnapshotSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeSnapshotSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeSnapshotSchedule where
  toJSON DescribeSnapshotSchedule' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("VolumeARN" Core..= volumeARN)]
      )

instance Core.ToPath DescribeSnapshotSchedule where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeSnapshotSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSnapshotScheduleResponse' smart constructor.
data DescribeSnapshotScheduleResponse = DescribeSnapshotScheduleResponse'
  { -- | The hour of the day at which the snapshot schedule begins represented as
    -- /hh/, where /hh/ is the hour (0 to 23). The hour of the day is in the
    -- time zone of the gateway.
    startAt :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the volume that was specified in the
    -- request.
    volumeARN :: Prelude.Maybe Prelude.Text,
    -- | The number of hours between snapshots.
    recurrenceInHours :: Prelude.Maybe Prelude.Natural,
    -- | A value that indicates the time zone of the gateway.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The snapshot description.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of up to 50 tags assigned to the snapshot schedule, sorted
    -- alphabetically by key name. Each tag is a key-value pair. For a gateway
    -- with more than 10 tags assigned, you can view all tags using the
    -- @ListTagsForResource@ API operation.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSnapshotScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startAt', 'describeSnapshotScheduleResponse_startAt' - The hour of the day at which the snapshot schedule begins represented as
-- /hh/, where /hh/ is the hour (0 to 23). The hour of the day is in the
-- time zone of the gateway.
--
-- 'volumeARN', 'describeSnapshotScheduleResponse_volumeARN' - The Amazon Resource Name (ARN) of the volume that was specified in the
-- request.
--
-- 'recurrenceInHours', 'describeSnapshotScheduleResponse_recurrenceInHours' - The number of hours between snapshots.
--
-- 'timezone', 'describeSnapshotScheduleResponse_timezone' - A value that indicates the time zone of the gateway.
--
-- 'description', 'describeSnapshotScheduleResponse_description' - The snapshot description.
--
-- 'tags', 'describeSnapshotScheduleResponse_tags' - A list of up to 50 tags assigned to the snapshot schedule, sorted
-- alphabetically by key name. Each tag is a key-value pair. For a gateway
-- with more than 10 tags assigned, you can view all tags using the
-- @ListTagsForResource@ API operation.
--
-- 'httpStatus', 'describeSnapshotScheduleResponse_httpStatus' - The response's http status code.
newDescribeSnapshotScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSnapshotScheduleResponse
newDescribeSnapshotScheduleResponse pHttpStatus_ =
  DescribeSnapshotScheduleResponse'
    { startAt =
        Prelude.Nothing,
      volumeARN = Prelude.Nothing,
      recurrenceInHours = Prelude.Nothing,
      timezone = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The hour of the day at which the snapshot schedule begins represented as
-- /hh/, where /hh/ is the hour (0 to 23). The hour of the day is in the
-- time zone of the gateway.
describeSnapshotScheduleResponse_startAt :: Lens.Lens' DescribeSnapshotScheduleResponse (Prelude.Maybe Prelude.Natural)
describeSnapshotScheduleResponse_startAt = Lens.lens (\DescribeSnapshotScheduleResponse' {startAt} -> startAt) (\s@DescribeSnapshotScheduleResponse' {} a -> s {startAt = a} :: DescribeSnapshotScheduleResponse)

-- | The Amazon Resource Name (ARN) of the volume that was specified in the
-- request.
describeSnapshotScheduleResponse_volumeARN :: Lens.Lens' DescribeSnapshotScheduleResponse (Prelude.Maybe Prelude.Text)
describeSnapshotScheduleResponse_volumeARN = Lens.lens (\DescribeSnapshotScheduleResponse' {volumeARN} -> volumeARN) (\s@DescribeSnapshotScheduleResponse' {} a -> s {volumeARN = a} :: DescribeSnapshotScheduleResponse)

-- | The number of hours between snapshots.
describeSnapshotScheduleResponse_recurrenceInHours :: Lens.Lens' DescribeSnapshotScheduleResponse (Prelude.Maybe Prelude.Natural)
describeSnapshotScheduleResponse_recurrenceInHours = Lens.lens (\DescribeSnapshotScheduleResponse' {recurrenceInHours} -> recurrenceInHours) (\s@DescribeSnapshotScheduleResponse' {} a -> s {recurrenceInHours = a} :: DescribeSnapshotScheduleResponse)

-- | A value that indicates the time zone of the gateway.
describeSnapshotScheduleResponse_timezone :: Lens.Lens' DescribeSnapshotScheduleResponse (Prelude.Maybe Prelude.Text)
describeSnapshotScheduleResponse_timezone = Lens.lens (\DescribeSnapshotScheduleResponse' {timezone} -> timezone) (\s@DescribeSnapshotScheduleResponse' {} a -> s {timezone = a} :: DescribeSnapshotScheduleResponse)

-- | The snapshot description.
describeSnapshotScheduleResponse_description :: Lens.Lens' DescribeSnapshotScheduleResponse (Prelude.Maybe Prelude.Text)
describeSnapshotScheduleResponse_description = Lens.lens (\DescribeSnapshotScheduleResponse' {description} -> description) (\s@DescribeSnapshotScheduleResponse' {} a -> s {description = a} :: DescribeSnapshotScheduleResponse)

-- | A list of up to 50 tags assigned to the snapshot schedule, sorted
-- alphabetically by key name. Each tag is a key-value pair. For a gateway
-- with more than 10 tags assigned, you can view all tags using the
-- @ListTagsForResource@ API operation.
describeSnapshotScheduleResponse_tags :: Lens.Lens' DescribeSnapshotScheduleResponse (Prelude.Maybe [Tag])
describeSnapshotScheduleResponse_tags = Lens.lens (\DescribeSnapshotScheduleResponse' {tags} -> tags) (\s@DescribeSnapshotScheduleResponse' {} a -> s {tags = a} :: DescribeSnapshotScheduleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSnapshotScheduleResponse_httpStatus :: Lens.Lens' DescribeSnapshotScheduleResponse Prelude.Int
describeSnapshotScheduleResponse_httpStatus = Lens.lens (\DescribeSnapshotScheduleResponse' {httpStatus} -> httpStatus) (\s@DescribeSnapshotScheduleResponse' {} a -> s {httpStatus = a} :: DescribeSnapshotScheduleResponse)

instance
  Prelude.NFData
    DescribeSnapshotScheduleResponse
