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
-- Module      : Amazonka.StorageGateway.UpdateSnapshotSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a snapshot schedule configured for a gateway volume. This
-- operation is only supported in the cached volume and stored volume
-- gateway types.
--
-- The default snapshot schedule for volume is once every 24 hours,
-- starting at the creation time of the volume. You can use this API to
-- change the snapshot schedule configured for the volume.
--
-- In the request you must identify the gateway volume whose snapshot
-- schedule you want to update, and the schedule information, including
-- when you want the snapshot to begin on a day and the frequency (in
-- hours) of snapshots.
module Amazonka.StorageGateway.UpdateSnapshotSchedule
  ( -- * Creating a Request
    UpdateSnapshotSchedule (..),
    newUpdateSnapshotSchedule,

    -- * Request Lenses
    updateSnapshotSchedule_description,
    updateSnapshotSchedule_tags,
    updateSnapshotSchedule_volumeARN,
    updateSnapshotSchedule_startAt,
    updateSnapshotSchedule_recurrenceInHours,

    -- * Destructuring the Response
    UpdateSnapshotScheduleResponse (..),
    newUpdateSnapshotScheduleResponse,

    -- * Response Lenses
    updateSnapshotScheduleResponse_volumeARN,
    updateSnapshotScheduleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   UpdateSnapshotScheduleInput$Description
--
-- -   UpdateSnapshotScheduleInput$RecurrenceInHours
--
-- -   UpdateSnapshotScheduleInput$StartAt
--
-- -   UpdateSnapshotScheduleInput$VolumeARN
--
-- /See:/ 'newUpdateSnapshotSchedule' smart constructor.
data UpdateSnapshotSchedule = UpdateSnapshotSchedule'
  { -- | Optional description of the snapshot that overwrites the existing
    -- description.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is
    -- a key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
    -- the maximum length for a tag\'s value is 256.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
    -- operation to return a list of gateway volumes.
    volumeARN :: Prelude.Text,
    -- | The hour of the day at which the snapshot schedule begins represented as
    -- /hh/, where /hh/ is the hour (0 to 23). The hour of the day is in the
    -- time zone of the gateway.
    startAt :: Prelude.Natural,
    -- | Frequency of snapshots. Specify the number of hours between snapshots.
    recurrenceInHours :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSnapshotSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateSnapshotSchedule_description' - Optional description of the snapshot that overwrites the existing
-- description.
--
-- 'tags', 'updateSnapshotSchedule_tags' - A list of up to 50 tags that can be assigned to a snapshot. Each tag is
-- a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
--
-- 'volumeARN', 'updateSnapshotSchedule_volumeARN' - The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
--
-- 'startAt', 'updateSnapshotSchedule_startAt' - The hour of the day at which the snapshot schedule begins represented as
-- /hh/, where /hh/ is the hour (0 to 23). The hour of the day is in the
-- time zone of the gateway.
--
-- 'recurrenceInHours', 'updateSnapshotSchedule_recurrenceInHours' - Frequency of snapshots. Specify the number of hours between snapshots.
newUpdateSnapshotSchedule ::
  -- | 'volumeARN'
  Prelude.Text ->
  -- | 'startAt'
  Prelude.Natural ->
  -- | 'recurrenceInHours'
  Prelude.Natural ->
  UpdateSnapshotSchedule
newUpdateSnapshotSchedule
  pVolumeARN_
  pStartAt_
  pRecurrenceInHours_ =
    UpdateSnapshotSchedule'
      { description =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        volumeARN = pVolumeARN_,
        startAt = pStartAt_,
        recurrenceInHours = pRecurrenceInHours_
      }

-- | Optional description of the snapshot that overwrites the existing
-- description.
updateSnapshotSchedule_description :: Lens.Lens' UpdateSnapshotSchedule (Prelude.Maybe Prelude.Text)
updateSnapshotSchedule_description = Lens.lens (\UpdateSnapshotSchedule' {description} -> description) (\s@UpdateSnapshotSchedule' {} a -> s {description = a} :: UpdateSnapshotSchedule)

-- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is
-- a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
updateSnapshotSchedule_tags :: Lens.Lens' UpdateSnapshotSchedule (Prelude.Maybe [Tag])
updateSnapshotSchedule_tags = Lens.lens (\UpdateSnapshotSchedule' {tags} -> tags) (\s@UpdateSnapshotSchedule' {} a -> s {tags = a} :: UpdateSnapshotSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
updateSnapshotSchedule_volumeARN :: Lens.Lens' UpdateSnapshotSchedule Prelude.Text
updateSnapshotSchedule_volumeARN = Lens.lens (\UpdateSnapshotSchedule' {volumeARN} -> volumeARN) (\s@UpdateSnapshotSchedule' {} a -> s {volumeARN = a} :: UpdateSnapshotSchedule)

-- | The hour of the day at which the snapshot schedule begins represented as
-- /hh/, where /hh/ is the hour (0 to 23). The hour of the day is in the
-- time zone of the gateway.
updateSnapshotSchedule_startAt :: Lens.Lens' UpdateSnapshotSchedule Prelude.Natural
updateSnapshotSchedule_startAt = Lens.lens (\UpdateSnapshotSchedule' {startAt} -> startAt) (\s@UpdateSnapshotSchedule' {} a -> s {startAt = a} :: UpdateSnapshotSchedule)

-- | Frequency of snapshots. Specify the number of hours between snapshots.
updateSnapshotSchedule_recurrenceInHours :: Lens.Lens' UpdateSnapshotSchedule Prelude.Natural
updateSnapshotSchedule_recurrenceInHours = Lens.lens (\UpdateSnapshotSchedule' {recurrenceInHours} -> recurrenceInHours) (\s@UpdateSnapshotSchedule' {} a -> s {recurrenceInHours = a} :: UpdateSnapshotSchedule)

instance Core.AWSRequest UpdateSnapshotSchedule where
  type
    AWSResponse UpdateSnapshotSchedule =
      UpdateSnapshotScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSnapshotScheduleResponse'
            Prelude.<$> (x Data..?> "VolumeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSnapshotSchedule where
  hashWithSalt _salt UpdateSnapshotSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` volumeARN
      `Prelude.hashWithSalt` startAt
      `Prelude.hashWithSalt` recurrenceInHours

instance Prelude.NFData UpdateSnapshotSchedule where
  rnf UpdateSnapshotSchedule' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf volumeARN
      `Prelude.seq` Prelude.rnf startAt
      `Prelude.seq` Prelude.rnf recurrenceInHours

instance Data.ToHeaders UpdateSnapshotSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.UpdateSnapshotSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSnapshotSchedule where
  toJSON UpdateSnapshotSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("VolumeARN" Data..= volumeARN),
            Prelude.Just ("StartAt" Data..= startAt),
            Prelude.Just
              ("RecurrenceInHours" Data..= recurrenceInHours)
          ]
      )

instance Data.ToPath UpdateSnapshotSchedule where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateSnapshotSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the updated
-- storage volume.
--
-- /See:/ 'newUpdateSnapshotScheduleResponse' smart constructor.
data UpdateSnapshotScheduleResponse = UpdateSnapshotScheduleResponse'
  { -- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
    -- operation to return a list of gateway volumes.
    volumeARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSnapshotScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARN', 'updateSnapshotScheduleResponse_volumeARN' - The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
--
-- 'httpStatus', 'updateSnapshotScheduleResponse_httpStatus' - The response's http status code.
newUpdateSnapshotScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSnapshotScheduleResponse
newUpdateSnapshotScheduleResponse pHttpStatus_ =
  UpdateSnapshotScheduleResponse'
    { volumeARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
updateSnapshotScheduleResponse_volumeARN :: Lens.Lens' UpdateSnapshotScheduleResponse (Prelude.Maybe Prelude.Text)
updateSnapshotScheduleResponse_volumeARN = Lens.lens (\UpdateSnapshotScheduleResponse' {volumeARN} -> volumeARN) (\s@UpdateSnapshotScheduleResponse' {} a -> s {volumeARN = a} :: UpdateSnapshotScheduleResponse)

-- | The response's http status code.
updateSnapshotScheduleResponse_httpStatus :: Lens.Lens' UpdateSnapshotScheduleResponse Prelude.Int
updateSnapshotScheduleResponse_httpStatus = Lens.lens (\UpdateSnapshotScheduleResponse' {httpStatus} -> httpStatus) (\s@UpdateSnapshotScheduleResponse' {} a -> s {httpStatus = a} :: UpdateSnapshotScheduleResponse)

instance
  Prelude.NFData
    UpdateSnapshotScheduleResponse
  where
  rnf UpdateSnapshotScheduleResponse' {..} =
    Prelude.rnf volumeARN
      `Prelude.seq` Prelude.rnf httpStatus
