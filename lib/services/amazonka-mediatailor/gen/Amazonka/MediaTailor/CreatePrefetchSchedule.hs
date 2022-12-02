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
-- Module      : Amazonka.MediaTailor.CreatePrefetchSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a prefetch schedule for a playback configuration. A prefetch
-- schedule allows you to tell MediaTailor to fetch and prepare certain ads
-- before an ad break happens. For more information about ad prefetching,
-- see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/prefetching-ads.html Using ad prefetching>
-- in the /MediaTailor User Guide/.
module Amazonka.MediaTailor.CreatePrefetchSchedule
  ( -- * Creating a Request
    CreatePrefetchSchedule (..),
    newCreatePrefetchSchedule,

    -- * Request Lenses
    createPrefetchSchedule_streamId,
    createPrefetchSchedule_consumption,
    createPrefetchSchedule_name,
    createPrefetchSchedule_playbackConfigurationName,
    createPrefetchSchedule_retrieval,

    -- * Destructuring the Response
    CreatePrefetchScheduleResponse (..),
    newCreatePrefetchScheduleResponse,

    -- * Response Lenses
    createPrefetchScheduleResponse_name,
    createPrefetchScheduleResponse_arn,
    createPrefetchScheduleResponse_streamId,
    createPrefetchScheduleResponse_retrieval,
    createPrefetchScheduleResponse_playbackConfigurationName,
    createPrefetchScheduleResponse_consumption,
    createPrefetchScheduleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePrefetchSchedule' smart constructor.
data CreatePrefetchSchedule = CreatePrefetchSchedule'
  { -- | An optional stream identifier that MediaTailor uses to prefetch ads for
    -- multiple streams that use the same playback configuration. If @StreamId@
    -- is specified, MediaTailor returns all of the prefetch schedules with an
    -- exact match on @StreamId@. If not specified, MediaTailor returns all of
    -- the prefetch schedules for the playback configuration, regardless of
    -- @StreamId@.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The configuration settings for MediaTailor\'s /consumption/ of the
    -- prefetched ads from the ad decision server. Each consumption
    -- configuration contains an end time and an optional start time that
    -- define the /consumption window/. Prefetch schedules automatically expire
    -- no earlier than seven days after the end time.
    consumption :: PrefetchConsumption,
    -- | The name to assign to the schedule request.
    name :: Prelude.Text,
    -- | The name to assign to the playback configuration.
    playbackConfigurationName :: Prelude.Text,
    -- | The configuration settings for retrieval of prefetched ads from the ad
    -- decision server. Only one set of prefetched ads will be retrieved and
    -- subsequently consumed for each ad break.
    retrieval :: PrefetchRetrieval
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePrefetchSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamId', 'createPrefetchSchedule_streamId' - An optional stream identifier that MediaTailor uses to prefetch ads for
-- multiple streams that use the same playback configuration. If @StreamId@
-- is specified, MediaTailor returns all of the prefetch schedules with an
-- exact match on @StreamId@. If not specified, MediaTailor returns all of
-- the prefetch schedules for the playback configuration, regardless of
-- @StreamId@.
--
-- 'consumption', 'createPrefetchSchedule_consumption' - The configuration settings for MediaTailor\'s /consumption/ of the
-- prefetched ads from the ad decision server. Each consumption
-- configuration contains an end time and an optional start time that
-- define the /consumption window/. Prefetch schedules automatically expire
-- no earlier than seven days after the end time.
--
-- 'name', 'createPrefetchSchedule_name' - The name to assign to the schedule request.
--
-- 'playbackConfigurationName', 'createPrefetchSchedule_playbackConfigurationName' - The name to assign to the playback configuration.
--
-- 'retrieval', 'createPrefetchSchedule_retrieval' - The configuration settings for retrieval of prefetched ads from the ad
-- decision server. Only one set of prefetched ads will be retrieved and
-- subsequently consumed for each ad break.
newCreatePrefetchSchedule ::
  -- | 'consumption'
  PrefetchConsumption ->
  -- | 'name'
  Prelude.Text ->
  -- | 'playbackConfigurationName'
  Prelude.Text ->
  -- | 'retrieval'
  PrefetchRetrieval ->
  CreatePrefetchSchedule
newCreatePrefetchSchedule
  pConsumption_
  pName_
  pPlaybackConfigurationName_
  pRetrieval_ =
    CreatePrefetchSchedule'
      { streamId = Prelude.Nothing,
        consumption = pConsumption_,
        name = pName_,
        playbackConfigurationName =
          pPlaybackConfigurationName_,
        retrieval = pRetrieval_
      }

-- | An optional stream identifier that MediaTailor uses to prefetch ads for
-- multiple streams that use the same playback configuration. If @StreamId@
-- is specified, MediaTailor returns all of the prefetch schedules with an
-- exact match on @StreamId@. If not specified, MediaTailor returns all of
-- the prefetch schedules for the playback configuration, regardless of
-- @StreamId@.
createPrefetchSchedule_streamId :: Lens.Lens' CreatePrefetchSchedule (Prelude.Maybe Prelude.Text)
createPrefetchSchedule_streamId = Lens.lens (\CreatePrefetchSchedule' {streamId} -> streamId) (\s@CreatePrefetchSchedule' {} a -> s {streamId = a} :: CreatePrefetchSchedule)

-- | The configuration settings for MediaTailor\'s /consumption/ of the
-- prefetched ads from the ad decision server. Each consumption
-- configuration contains an end time and an optional start time that
-- define the /consumption window/. Prefetch schedules automatically expire
-- no earlier than seven days after the end time.
createPrefetchSchedule_consumption :: Lens.Lens' CreatePrefetchSchedule PrefetchConsumption
createPrefetchSchedule_consumption = Lens.lens (\CreatePrefetchSchedule' {consumption} -> consumption) (\s@CreatePrefetchSchedule' {} a -> s {consumption = a} :: CreatePrefetchSchedule)

-- | The name to assign to the schedule request.
createPrefetchSchedule_name :: Lens.Lens' CreatePrefetchSchedule Prelude.Text
createPrefetchSchedule_name = Lens.lens (\CreatePrefetchSchedule' {name} -> name) (\s@CreatePrefetchSchedule' {} a -> s {name = a} :: CreatePrefetchSchedule)

-- | The name to assign to the playback configuration.
createPrefetchSchedule_playbackConfigurationName :: Lens.Lens' CreatePrefetchSchedule Prelude.Text
createPrefetchSchedule_playbackConfigurationName = Lens.lens (\CreatePrefetchSchedule' {playbackConfigurationName} -> playbackConfigurationName) (\s@CreatePrefetchSchedule' {} a -> s {playbackConfigurationName = a} :: CreatePrefetchSchedule)

-- | The configuration settings for retrieval of prefetched ads from the ad
-- decision server. Only one set of prefetched ads will be retrieved and
-- subsequently consumed for each ad break.
createPrefetchSchedule_retrieval :: Lens.Lens' CreatePrefetchSchedule PrefetchRetrieval
createPrefetchSchedule_retrieval = Lens.lens (\CreatePrefetchSchedule' {retrieval} -> retrieval) (\s@CreatePrefetchSchedule' {} a -> s {retrieval = a} :: CreatePrefetchSchedule)

instance Core.AWSRequest CreatePrefetchSchedule where
  type
    AWSResponse CreatePrefetchSchedule =
      CreatePrefetchScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePrefetchScheduleResponse'
            Prelude.<$> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "StreamId")
            Prelude.<*> (x Data..?> "Retrieval")
            Prelude.<*> (x Data..?> "PlaybackConfigurationName")
            Prelude.<*> (x Data..?> "Consumption")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePrefetchSchedule where
  hashWithSalt _salt CreatePrefetchSchedule' {..} =
    _salt `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` consumption
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` playbackConfigurationName
      `Prelude.hashWithSalt` retrieval

instance Prelude.NFData CreatePrefetchSchedule where
  rnf CreatePrefetchSchedule' {..} =
    Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf consumption
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf playbackConfigurationName
      `Prelude.seq` Prelude.rnf retrieval

instance Data.ToHeaders CreatePrefetchSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePrefetchSchedule where
  toJSON CreatePrefetchSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamId" Data..=) Prelude.<$> streamId,
            Prelude.Just ("Consumption" Data..= consumption),
            Prelude.Just ("Retrieval" Data..= retrieval)
          ]
      )

instance Data.ToPath CreatePrefetchSchedule where
  toPath CreatePrefetchSchedule' {..} =
    Prelude.mconcat
      [ "/prefetchSchedule/",
        Data.toBS playbackConfigurationName,
        "/",
        Data.toBS name
      ]

instance Data.ToQuery CreatePrefetchSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePrefetchScheduleResponse' smart constructor.
data CreatePrefetchScheduleResponse = CreatePrefetchScheduleResponse'
  { -- | The name to assign to the prefetch schedule.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN to assign to the prefetch schedule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An optional stream identifier that MediaTailor uses to prefetch ads for
    -- multiple streams that use the same playback configuration. If @StreamId@
    -- is specified, MediaTailor returns all of the prefetch schedules with an
    -- exact match on @StreamId@. If not specified, MediaTailor returns all of
    -- the prefetch schedules for the playback configuration, regardless of
    -- @StreamId@.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The configuration settings for retrieval of prefetched ads from the ad
    -- decision server. Only one set of prefetched ads will be retrieved and
    -- subsequently consumed for each ad break.
    retrieval :: Prelude.Maybe PrefetchRetrieval,
    -- | The name to assign to the playback configuration.
    playbackConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The configuration settings for MediaTailor\'s /consumption/ of the
    -- prefetched ads from the ad decision server. Each consumption
    -- configuration contains an end time and an optional start time that
    -- define the /consumption window/. Prefetch schedules automatically expire
    -- no earlier than seven days after the end time.
    consumption :: Prelude.Maybe PrefetchConsumption,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePrefetchScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createPrefetchScheduleResponse_name' - The name to assign to the prefetch schedule.
--
-- 'arn', 'createPrefetchScheduleResponse_arn' - The ARN to assign to the prefetch schedule.
--
-- 'streamId', 'createPrefetchScheduleResponse_streamId' - An optional stream identifier that MediaTailor uses to prefetch ads for
-- multiple streams that use the same playback configuration. If @StreamId@
-- is specified, MediaTailor returns all of the prefetch schedules with an
-- exact match on @StreamId@. If not specified, MediaTailor returns all of
-- the prefetch schedules for the playback configuration, regardless of
-- @StreamId@.
--
-- 'retrieval', 'createPrefetchScheduleResponse_retrieval' - The configuration settings for retrieval of prefetched ads from the ad
-- decision server. Only one set of prefetched ads will be retrieved and
-- subsequently consumed for each ad break.
--
-- 'playbackConfigurationName', 'createPrefetchScheduleResponse_playbackConfigurationName' - The name to assign to the playback configuration.
--
-- 'consumption', 'createPrefetchScheduleResponse_consumption' - The configuration settings for MediaTailor\'s /consumption/ of the
-- prefetched ads from the ad decision server. Each consumption
-- configuration contains an end time and an optional start time that
-- define the /consumption window/. Prefetch schedules automatically expire
-- no earlier than seven days after the end time.
--
-- 'httpStatus', 'createPrefetchScheduleResponse_httpStatus' - The response's http status code.
newCreatePrefetchScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePrefetchScheduleResponse
newCreatePrefetchScheduleResponse pHttpStatus_ =
  CreatePrefetchScheduleResponse'
    { name =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      streamId = Prelude.Nothing,
      retrieval = Prelude.Nothing,
      playbackConfigurationName = Prelude.Nothing,
      consumption = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name to assign to the prefetch schedule.
createPrefetchScheduleResponse_name :: Lens.Lens' CreatePrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
createPrefetchScheduleResponse_name = Lens.lens (\CreatePrefetchScheduleResponse' {name} -> name) (\s@CreatePrefetchScheduleResponse' {} a -> s {name = a} :: CreatePrefetchScheduleResponse)

-- | The ARN to assign to the prefetch schedule.
createPrefetchScheduleResponse_arn :: Lens.Lens' CreatePrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
createPrefetchScheduleResponse_arn = Lens.lens (\CreatePrefetchScheduleResponse' {arn} -> arn) (\s@CreatePrefetchScheduleResponse' {} a -> s {arn = a} :: CreatePrefetchScheduleResponse)

-- | An optional stream identifier that MediaTailor uses to prefetch ads for
-- multiple streams that use the same playback configuration. If @StreamId@
-- is specified, MediaTailor returns all of the prefetch schedules with an
-- exact match on @StreamId@. If not specified, MediaTailor returns all of
-- the prefetch schedules for the playback configuration, regardless of
-- @StreamId@.
createPrefetchScheduleResponse_streamId :: Lens.Lens' CreatePrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
createPrefetchScheduleResponse_streamId = Lens.lens (\CreatePrefetchScheduleResponse' {streamId} -> streamId) (\s@CreatePrefetchScheduleResponse' {} a -> s {streamId = a} :: CreatePrefetchScheduleResponse)

-- | The configuration settings for retrieval of prefetched ads from the ad
-- decision server. Only one set of prefetched ads will be retrieved and
-- subsequently consumed for each ad break.
createPrefetchScheduleResponse_retrieval :: Lens.Lens' CreatePrefetchScheduleResponse (Prelude.Maybe PrefetchRetrieval)
createPrefetchScheduleResponse_retrieval = Lens.lens (\CreatePrefetchScheduleResponse' {retrieval} -> retrieval) (\s@CreatePrefetchScheduleResponse' {} a -> s {retrieval = a} :: CreatePrefetchScheduleResponse)

-- | The name to assign to the playback configuration.
createPrefetchScheduleResponse_playbackConfigurationName :: Lens.Lens' CreatePrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
createPrefetchScheduleResponse_playbackConfigurationName = Lens.lens (\CreatePrefetchScheduleResponse' {playbackConfigurationName} -> playbackConfigurationName) (\s@CreatePrefetchScheduleResponse' {} a -> s {playbackConfigurationName = a} :: CreatePrefetchScheduleResponse)

-- | The configuration settings for MediaTailor\'s /consumption/ of the
-- prefetched ads from the ad decision server. Each consumption
-- configuration contains an end time and an optional start time that
-- define the /consumption window/. Prefetch schedules automatically expire
-- no earlier than seven days after the end time.
createPrefetchScheduleResponse_consumption :: Lens.Lens' CreatePrefetchScheduleResponse (Prelude.Maybe PrefetchConsumption)
createPrefetchScheduleResponse_consumption = Lens.lens (\CreatePrefetchScheduleResponse' {consumption} -> consumption) (\s@CreatePrefetchScheduleResponse' {} a -> s {consumption = a} :: CreatePrefetchScheduleResponse)

-- | The response's http status code.
createPrefetchScheduleResponse_httpStatus :: Lens.Lens' CreatePrefetchScheduleResponse Prelude.Int
createPrefetchScheduleResponse_httpStatus = Lens.lens (\CreatePrefetchScheduleResponse' {httpStatus} -> httpStatus) (\s@CreatePrefetchScheduleResponse' {} a -> s {httpStatus = a} :: CreatePrefetchScheduleResponse)

instance
  Prelude.NFData
    CreatePrefetchScheduleResponse
  where
  rnf CreatePrefetchScheduleResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf retrieval
      `Prelude.seq` Prelude.rnf playbackConfigurationName
      `Prelude.seq` Prelude.rnf consumption
      `Prelude.seq` Prelude.rnf httpStatus
