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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new prefetch schedule for the specified playback
-- configuration.
module Amazonka.MediaTailor.CreatePrefetchSchedule
  ( -- * Creating a Request
    CreatePrefetchSchedule (..),
    newCreatePrefetchSchedule,

    -- * Request Lenses
    createPrefetchSchedule_streamId,
    createPrefetchSchedule_name,
    createPrefetchSchedule_playbackConfigurationName,
    createPrefetchSchedule_consumption,
    createPrefetchSchedule_retrieval,

    -- * Destructuring the Response
    CreatePrefetchScheduleResponse (..),
    newCreatePrefetchScheduleResponse,

    -- * Response Lenses
    createPrefetchScheduleResponse_arn,
    createPrefetchScheduleResponse_playbackConfigurationName,
    createPrefetchScheduleResponse_retrieval,
    createPrefetchScheduleResponse_name,
    createPrefetchScheduleResponse_consumption,
    createPrefetchScheduleResponse_streamId,
    createPrefetchScheduleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePrefetchSchedule' smart constructor.
data CreatePrefetchSchedule = CreatePrefetchSchedule'
  { -- | An optional stream identifier that MediaTailor uses to prefetch ads for
    -- multiple streams that use the same playback configuration. If StreamId
    -- is specified, MediaTailor returns all of the prefetch schedules with an
    -- exact match on StreamId. If not specified, MediaTailor returns all of
    -- the prefetch schedules for the playback configuration, regardless of
    -- StreamId.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the playback configuration.
    name :: Prelude.Text,
    -- | The name of the playback configuration.
    playbackConfigurationName :: Prelude.Text,
    -- | The configuration settings for MediaTailor\'s /consumption/ of the
    -- prefetched ads from the ad decision server. Each consumption
    -- configuration contains an end time and an optional start time that
    -- define the /consumption window/. Prefetch schedules automatically expire
    -- no earlier than seven days after the end time.
    consumption :: PrefetchConsumption,
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
-- multiple streams that use the same playback configuration. If StreamId
-- is specified, MediaTailor returns all of the prefetch schedules with an
-- exact match on StreamId. If not specified, MediaTailor returns all of
-- the prefetch schedules for the playback configuration, regardless of
-- StreamId.
--
-- 'name', 'createPrefetchSchedule_name' - The identifier for the playback configuration.
--
-- 'playbackConfigurationName', 'createPrefetchSchedule_playbackConfigurationName' - The name of the playback configuration.
--
-- 'consumption', 'createPrefetchSchedule_consumption' - The configuration settings for MediaTailor\'s /consumption/ of the
-- prefetched ads from the ad decision server. Each consumption
-- configuration contains an end time and an optional start time that
-- define the /consumption window/. Prefetch schedules automatically expire
-- no earlier than seven days after the end time.
--
-- 'retrieval', 'createPrefetchSchedule_retrieval' - The configuration settings for retrieval of prefetched ads from the ad
-- decision server. Only one set of prefetched ads will be retrieved and
-- subsequently consumed for each ad break.
newCreatePrefetchSchedule ::
  -- | 'name'
  Prelude.Text ->
  -- | 'playbackConfigurationName'
  Prelude.Text ->
  -- | 'consumption'
  PrefetchConsumption ->
  -- | 'retrieval'
  PrefetchRetrieval ->
  CreatePrefetchSchedule
newCreatePrefetchSchedule
  pName_
  pPlaybackConfigurationName_
  pConsumption_
  pRetrieval_ =
    CreatePrefetchSchedule'
      { streamId = Prelude.Nothing,
        name = pName_,
        playbackConfigurationName =
          pPlaybackConfigurationName_,
        consumption = pConsumption_,
        retrieval = pRetrieval_
      }

-- | An optional stream identifier that MediaTailor uses to prefetch ads for
-- multiple streams that use the same playback configuration. If StreamId
-- is specified, MediaTailor returns all of the prefetch schedules with an
-- exact match on StreamId. If not specified, MediaTailor returns all of
-- the prefetch schedules for the playback configuration, regardless of
-- StreamId.
createPrefetchSchedule_streamId :: Lens.Lens' CreatePrefetchSchedule (Prelude.Maybe Prelude.Text)
createPrefetchSchedule_streamId = Lens.lens (\CreatePrefetchSchedule' {streamId} -> streamId) (\s@CreatePrefetchSchedule' {} a -> s {streamId = a} :: CreatePrefetchSchedule)

-- | The identifier for the playback configuration.
createPrefetchSchedule_name :: Lens.Lens' CreatePrefetchSchedule Prelude.Text
createPrefetchSchedule_name = Lens.lens (\CreatePrefetchSchedule' {name} -> name) (\s@CreatePrefetchSchedule' {} a -> s {name = a} :: CreatePrefetchSchedule)

-- | The name of the playback configuration.
createPrefetchSchedule_playbackConfigurationName :: Lens.Lens' CreatePrefetchSchedule Prelude.Text
createPrefetchSchedule_playbackConfigurationName = Lens.lens (\CreatePrefetchSchedule' {playbackConfigurationName} -> playbackConfigurationName) (\s@CreatePrefetchSchedule' {} a -> s {playbackConfigurationName = a} :: CreatePrefetchSchedule)

-- | The configuration settings for MediaTailor\'s /consumption/ of the
-- prefetched ads from the ad decision server. Each consumption
-- configuration contains an end time and an optional start time that
-- define the /consumption window/. Prefetch schedules automatically expire
-- no earlier than seven days after the end time.
createPrefetchSchedule_consumption :: Lens.Lens' CreatePrefetchSchedule PrefetchConsumption
createPrefetchSchedule_consumption = Lens.lens (\CreatePrefetchSchedule' {consumption} -> consumption) (\s@CreatePrefetchSchedule' {} a -> s {consumption = a} :: CreatePrefetchSchedule)

-- | The configuration settings for retrieval of prefetched ads from the ad
-- decision server. Only one set of prefetched ads will be retrieved and
-- subsequently consumed for each ad break.
createPrefetchSchedule_retrieval :: Lens.Lens' CreatePrefetchSchedule PrefetchRetrieval
createPrefetchSchedule_retrieval = Lens.lens (\CreatePrefetchSchedule' {retrieval} -> retrieval) (\s@CreatePrefetchSchedule' {} a -> s {retrieval = a} :: CreatePrefetchSchedule)

instance Core.AWSRequest CreatePrefetchSchedule where
  type
    AWSResponse CreatePrefetchSchedule =
      CreatePrefetchScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePrefetchScheduleResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "PlaybackConfigurationName")
            Prelude.<*> (x Core..?> "Retrieval")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Consumption")
            Prelude.<*> (x Core..?> "StreamId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePrefetchSchedule where
  hashWithSalt _salt CreatePrefetchSchedule' {..} =
    _salt `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` playbackConfigurationName
      `Prelude.hashWithSalt` consumption
      `Prelude.hashWithSalt` retrieval

instance Prelude.NFData CreatePrefetchSchedule where
  rnf CreatePrefetchSchedule' {..} =
    Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf playbackConfigurationName
      `Prelude.seq` Prelude.rnf consumption
      `Prelude.seq` Prelude.rnf retrieval

instance Core.ToHeaders CreatePrefetchSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePrefetchSchedule where
  toJSON CreatePrefetchSchedule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StreamId" Core..=) Prelude.<$> streamId,
            Prelude.Just ("Consumption" Core..= consumption),
            Prelude.Just ("Retrieval" Core..= retrieval)
          ]
      )

instance Core.ToPath CreatePrefetchSchedule where
  toPath CreatePrefetchSchedule' {..} =
    Prelude.mconcat
      [ "/prefetchSchedule/",
        Core.toBS playbackConfigurationName,
        "/",
        Core.toBS name
      ]

instance Core.ToQuery CreatePrefetchSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePrefetchScheduleResponse' smart constructor.
data CreatePrefetchScheduleResponse = CreatePrefetchScheduleResponse'
  { -- | The Amazon Resource Name (ARN) of the prefetch schedule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the playback configuration to create the prefetch schedule
    -- for.
    playbackConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains settings for prefetch retrieval from the ad
    -- decision server (ADS).
    retrieval :: Prelude.Maybe PrefetchRetrieval,
    -- | The name of the prefetch schedule. The name must be unique among all
    -- prefetch schedules that are associated with the specified playback
    -- configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | Consumption settings determine how, and when, MediaTailor places the
    -- prefetched ads into ad breaks. Ad consumption occurs within a span of
    -- time that you define, called a /consumption window/. You can designate
    -- which ad breaks that MediaTailor fills with prefetch ads by setting
    -- avail matching criteria.
    consumption :: Prelude.Maybe PrefetchConsumption,
    -- | An optional stream identifier that you can specify in order to prefetch
    -- for multiple streams that use the same playback configuration.
    streamId :: Prelude.Maybe Prelude.Text,
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
-- 'arn', 'createPrefetchScheduleResponse_arn' - The Amazon Resource Name (ARN) of the prefetch schedule.
--
-- 'playbackConfigurationName', 'createPrefetchScheduleResponse_playbackConfigurationName' - The name of the playback configuration to create the prefetch schedule
-- for.
--
-- 'retrieval', 'createPrefetchScheduleResponse_retrieval' - A complex type that contains settings for prefetch retrieval from the ad
-- decision server (ADS).
--
-- 'name', 'createPrefetchScheduleResponse_name' - The name of the prefetch schedule. The name must be unique among all
-- prefetch schedules that are associated with the specified playback
-- configuration.
--
-- 'consumption', 'createPrefetchScheduleResponse_consumption' - Consumption settings determine how, and when, MediaTailor places the
-- prefetched ads into ad breaks. Ad consumption occurs within a span of
-- time that you define, called a /consumption window/. You can designate
-- which ad breaks that MediaTailor fills with prefetch ads by setting
-- avail matching criteria.
--
-- 'streamId', 'createPrefetchScheduleResponse_streamId' - An optional stream identifier that you can specify in order to prefetch
-- for multiple streams that use the same playback configuration.
--
-- 'httpStatus', 'createPrefetchScheduleResponse_httpStatus' - The response's http status code.
newCreatePrefetchScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePrefetchScheduleResponse
newCreatePrefetchScheduleResponse pHttpStatus_ =
  CreatePrefetchScheduleResponse'
    { arn =
        Prelude.Nothing,
      playbackConfigurationName = Prelude.Nothing,
      retrieval = Prelude.Nothing,
      name = Prelude.Nothing,
      consumption = Prelude.Nothing,
      streamId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the prefetch schedule.
createPrefetchScheduleResponse_arn :: Lens.Lens' CreatePrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
createPrefetchScheduleResponse_arn = Lens.lens (\CreatePrefetchScheduleResponse' {arn} -> arn) (\s@CreatePrefetchScheduleResponse' {} a -> s {arn = a} :: CreatePrefetchScheduleResponse)

-- | The name of the playback configuration to create the prefetch schedule
-- for.
createPrefetchScheduleResponse_playbackConfigurationName :: Lens.Lens' CreatePrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
createPrefetchScheduleResponse_playbackConfigurationName = Lens.lens (\CreatePrefetchScheduleResponse' {playbackConfigurationName} -> playbackConfigurationName) (\s@CreatePrefetchScheduleResponse' {} a -> s {playbackConfigurationName = a} :: CreatePrefetchScheduleResponse)

-- | A complex type that contains settings for prefetch retrieval from the ad
-- decision server (ADS).
createPrefetchScheduleResponse_retrieval :: Lens.Lens' CreatePrefetchScheduleResponse (Prelude.Maybe PrefetchRetrieval)
createPrefetchScheduleResponse_retrieval = Lens.lens (\CreatePrefetchScheduleResponse' {retrieval} -> retrieval) (\s@CreatePrefetchScheduleResponse' {} a -> s {retrieval = a} :: CreatePrefetchScheduleResponse)

-- | The name of the prefetch schedule. The name must be unique among all
-- prefetch schedules that are associated with the specified playback
-- configuration.
createPrefetchScheduleResponse_name :: Lens.Lens' CreatePrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
createPrefetchScheduleResponse_name = Lens.lens (\CreatePrefetchScheduleResponse' {name} -> name) (\s@CreatePrefetchScheduleResponse' {} a -> s {name = a} :: CreatePrefetchScheduleResponse)

-- | Consumption settings determine how, and when, MediaTailor places the
-- prefetched ads into ad breaks. Ad consumption occurs within a span of
-- time that you define, called a /consumption window/. You can designate
-- which ad breaks that MediaTailor fills with prefetch ads by setting
-- avail matching criteria.
createPrefetchScheduleResponse_consumption :: Lens.Lens' CreatePrefetchScheduleResponse (Prelude.Maybe PrefetchConsumption)
createPrefetchScheduleResponse_consumption = Lens.lens (\CreatePrefetchScheduleResponse' {consumption} -> consumption) (\s@CreatePrefetchScheduleResponse' {} a -> s {consumption = a} :: CreatePrefetchScheduleResponse)

-- | An optional stream identifier that you can specify in order to prefetch
-- for multiple streams that use the same playback configuration.
createPrefetchScheduleResponse_streamId :: Lens.Lens' CreatePrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
createPrefetchScheduleResponse_streamId = Lens.lens (\CreatePrefetchScheduleResponse' {streamId} -> streamId) (\s@CreatePrefetchScheduleResponse' {} a -> s {streamId = a} :: CreatePrefetchScheduleResponse)

-- | The response's http status code.
createPrefetchScheduleResponse_httpStatus :: Lens.Lens' CreatePrefetchScheduleResponse Prelude.Int
createPrefetchScheduleResponse_httpStatus = Lens.lens (\CreatePrefetchScheduleResponse' {httpStatus} -> httpStatus) (\s@CreatePrefetchScheduleResponse' {} a -> s {httpStatus = a} :: CreatePrefetchScheduleResponse)

instance
  Prelude.NFData
    CreatePrefetchScheduleResponse
  where
  rnf CreatePrefetchScheduleResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf playbackConfigurationName
      `Prelude.seq` Prelude.rnf retrieval
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf consumption
      `Prelude.seq` Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf httpStatus
