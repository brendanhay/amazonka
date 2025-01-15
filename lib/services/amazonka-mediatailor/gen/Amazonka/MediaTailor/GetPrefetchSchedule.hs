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
-- Module      : Amazonka.MediaTailor.GetPrefetchSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a prefetch schedule for a playback configuration. A prefetch
-- schedule allows you to tell MediaTailor to fetch and prepare certain ads
-- before an ad break happens. For more information about ad prefetching,
-- see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/prefetching-ads.html Using ad prefetching>
-- in the /MediaTailor User Guide/.
module Amazonka.MediaTailor.GetPrefetchSchedule
  ( -- * Creating a Request
    GetPrefetchSchedule (..),
    newGetPrefetchSchedule,

    -- * Request Lenses
    getPrefetchSchedule_name,
    getPrefetchSchedule_playbackConfigurationName,

    -- * Destructuring the Response
    GetPrefetchScheduleResponse (..),
    newGetPrefetchScheduleResponse,

    -- * Response Lenses
    getPrefetchScheduleResponse_arn,
    getPrefetchScheduleResponse_consumption,
    getPrefetchScheduleResponse_name,
    getPrefetchScheduleResponse_playbackConfigurationName,
    getPrefetchScheduleResponse_retrieval,
    getPrefetchScheduleResponse_streamId,
    getPrefetchScheduleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPrefetchSchedule' smart constructor.
data GetPrefetchSchedule = GetPrefetchSchedule'
  { -- | The name of the prefetch schedule. The name must be unique among all
    -- prefetch schedules that are associated with the specified playback
    -- configuration.
    name :: Prelude.Text,
    -- | Returns information about the prefetch schedule for a specific playback
    -- configuration. If you call @GetPrefetchSchedule@ on an expired prefetch
    -- schedule, MediaTailor returns an HTTP 404 status code.
    playbackConfigurationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPrefetchSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getPrefetchSchedule_name' - The name of the prefetch schedule. The name must be unique among all
-- prefetch schedules that are associated with the specified playback
-- configuration.
--
-- 'playbackConfigurationName', 'getPrefetchSchedule_playbackConfigurationName' - Returns information about the prefetch schedule for a specific playback
-- configuration. If you call @GetPrefetchSchedule@ on an expired prefetch
-- schedule, MediaTailor returns an HTTP 404 status code.
newGetPrefetchSchedule ::
  -- | 'name'
  Prelude.Text ->
  -- | 'playbackConfigurationName'
  Prelude.Text ->
  GetPrefetchSchedule
newGetPrefetchSchedule
  pName_
  pPlaybackConfigurationName_ =
    GetPrefetchSchedule'
      { name = pName_,
        playbackConfigurationName =
          pPlaybackConfigurationName_
      }

-- | The name of the prefetch schedule. The name must be unique among all
-- prefetch schedules that are associated with the specified playback
-- configuration.
getPrefetchSchedule_name :: Lens.Lens' GetPrefetchSchedule Prelude.Text
getPrefetchSchedule_name = Lens.lens (\GetPrefetchSchedule' {name} -> name) (\s@GetPrefetchSchedule' {} a -> s {name = a} :: GetPrefetchSchedule)

-- | Returns information about the prefetch schedule for a specific playback
-- configuration. If you call @GetPrefetchSchedule@ on an expired prefetch
-- schedule, MediaTailor returns an HTTP 404 status code.
getPrefetchSchedule_playbackConfigurationName :: Lens.Lens' GetPrefetchSchedule Prelude.Text
getPrefetchSchedule_playbackConfigurationName = Lens.lens (\GetPrefetchSchedule' {playbackConfigurationName} -> playbackConfigurationName) (\s@GetPrefetchSchedule' {} a -> s {playbackConfigurationName = a} :: GetPrefetchSchedule)

instance Core.AWSRequest GetPrefetchSchedule where
  type
    AWSResponse GetPrefetchSchedule =
      GetPrefetchScheduleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPrefetchScheduleResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Consumption")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "PlaybackConfigurationName")
            Prelude.<*> (x Data..?> "Retrieval")
            Prelude.<*> (x Data..?> "StreamId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPrefetchSchedule where
  hashWithSalt _salt GetPrefetchSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` playbackConfigurationName

instance Prelude.NFData GetPrefetchSchedule where
  rnf GetPrefetchSchedule' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf playbackConfigurationName

instance Data.ToHeaders GetPrefetchSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetPrefetchSchedule where
  toPath GetPrefetchSchedule' {..} =
    Prelude.mconcat
      [ "/prefetchSchedule/",
        Data.toBS playbackConfigurationName,
        "/",
        Data.toBS name
      ]

instance Data.ToQuery GetPrefetchSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPrefetchScheduleResponse' smart constructor.
data GetPrefetchScheduleResponse = GetPrefetchScheduleResponse'
  { -- | The Amazon Resource Name (ARN) of the prefetch schedule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Consumption settings determine how, and when, MediaTailor places the
    -- prefetched ads into ad breaks. Ad consumption occurs within a span of
    -- time that you define, called a /consumption window/. You can designate
    -- which ad breaks that MediaTailor fills with prefetch ads by setting
    -- avail matching criteria.
    consumption :: Prelude.Maybe PrefetchConsumption,
    -- | The name of the prefetch schedule. The name must be unique among all
    -- prefetch schedules that are associated with the specified playback
    -- configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the playback configuration to create the prefetch schedule
    -- for.
    playbackConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains settings for prefetch retrieval from the ad
    -- decision server (ADS).
    retrieval :: Prelude.Maybe PrefetchRetrieval,
    -- | An optional stream identifier that you can specify in order to prefetch
    -- for multiple streams that use the same playback configuration.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPrefetchScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getPrefetchScheduleResponse_arn' - The Amazon Resource Name (ARN) of the prefetch schedule.
--
-- 'consumption', 'getPrefetchScheduleResponse_consumption' - Consumption settings determine how, and when, MediaTailor places the
-- prefetched ads into ad breaks. Ad consumption occurs within a span of
-- time that you define, called a /consumption window/. You can designate
-- which ad breaks that MediaTailor fills with prefetch ads by setting
-- avail matching criteria.
--
-- 'name', 'getPrefetchScheduleResponse_name' - The name of the prefetch schedule. The name must be unique among all
-- prefetch schedules that are associated with the specified playback
-- configuration.
--
-- 'playbackConfigurationName', 'getPrefetchScheduleResponse_playbackConfigurationName' - The name of the playback configuration to create the prefetch schedule
-- for.
--
-- 'retrieval', 'getPrefetchScheduleResponse_retrieval' - A complex type that contains settings for prefetch retrieval from the ad
-- decision server (ADS).
--
-- 'streamId', 'getPrefetchScheduleResponse_streamId' - An optional stream identifier that you can specify in order to prefetch
-- for multiple streams that use the same playback configuration.
--
-- 'httpStatus', 'getPrefetchScheduleResponse_httpStatus' - The response's http status code.
newGetPrefetchScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPrefetchScheduleResponse
newGetPrefetchScheduleResponse pHttpStatus_ =
  GetPrefetchScheduleResponse'
    { arn = Prelude.Nothing,
      consumption = Prelude.Nothing,
      name = Prelude.Nothing,
      playbackConfigurationName = Prelude.Nothing,
      retrieval = Prelude.Nothing,
      streamId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the prefetch schedule.
getPrefetchScheduleResponse_arn :: Lens.Lens' GetPrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
getPrefetchScheduleResponse_arn = Lens.lens (\GetPrefetchScheduleResponse' {arn} -> arn) (\s@GetPrefetchScheduleResponse' {} a -> s {arn = a} :: GetPrefetchScheduleResponse)

-- | Consumption settings determine how, and when, MediaTailor places the
-- prefetched ads into ad breaks. Ad consumption occurs within a span of
-- time that you define, called a /consumption window/. You can designate
-- which ad breaks that MediaTailor fills with prefetch ads by setting
-- avail matching criteria.
getPrefetchScheduleResponse_consumption :: Lens.Lens' GetPrefetchScheduleResponse (Prelude.Maybe PrefetchConsumption)
getPrefetchScheduleResponse_consumption = Lens.lens (\GetPrefetchScheduleResponse' {consumption} -> consumption) (\s@GetPrefetchScheduleResponse' {} a -> s {consumption = a} :: GetPrefetchScheduleResponse)

-- | The name of the prefetch schedule. The name must be unique among all
-- prefetch schedules that are associated with the specified playback
-- configuration.
getPrefetchScheduleResponse_name :: Lens.Lens' GetPrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
getPrefetchScheduleResponse_name = Lens.lens (\GetPrefetchScheduleResponse' {name} -> name) (\s@GetPrefetchScheduleResponse' {} a -> s {name = a} :: GetPrefetchScheduleResponse)

-- | The name of the playback configuration to create the prefetch schedule
-- for.
getPrefetchScheduleResponse_playbackConfigurationName :: Lens.Lens' GetPrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
getPrefetchScheduleResponse_playbackConfigurationName = Lens.lens (\GetPrefetchScheduleResponse' {playbackConfigurationName} -> playbackConfigurationName) (\s@GetPrefetchScheduleResponse' {} a -> s {playbackConfigurationName = a} :: GetPrefetchScheduleResponse)

-- | A complex type that contains settings for prefetch retrieval from the ad
-- decision server (ADS).
getPrefetchScheduleResponse_retrieval :: Lens.Lens' GetPrefetchScheduleResponse (Prelude.Maybe PrefetchRetrieval)
getPrefetchScheduleResponse_retrieval = Lens.lens (\GetPrefetchScheduleResponse' {retrieval} -> retrieval) (\s@GetPrefetchScheduleResponse' {} a -> s {retrieval = a} :: GetPrefetchScheduleResponse)

-- | An optional stream identifier that you can specify in order to prefetch
-- for multiple streams that use the same playback configuration.
getPrefetchScheduleResponse_streamId :: Lens.Lens' GetPrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
getPrefetchScheduleResponse_streamId = Lens.lens (\GetPrefetchScheduleResponse' {streamId} -> streamId) (\s@GetPrefetchScheduleResponse' {} a -> s {streamId = a} :: GetPrefetchScheduleResponse)

-- | The response's http status code.
getPrefetchScheduleResponse_httpStatus :: Lens.Lens' GetPrefetchScheduleResponse Prelude.Int
getPrefetchScheduleResponse_httpStatus = Lens.lens (\GetPrefetchScheduleResponse' {httpStatus} -> httpStatus) (\s@GetPrefetchScheduleResponse' {} a -> s {httpStatus = a} :: GetPrefetchScheduleResponse)

instance Prelude.NFData GetPrefetchScheduleResponse where
  rnf GetPrefetchScheduleResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf consumption `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf playbackConfigurationName `Prelude.seq`
            Prelude.rnf retrieval `Prelude.seq`
              Prelude.rnf streamId `Prelude.seq`
                Prelude.rnf httpStatus
