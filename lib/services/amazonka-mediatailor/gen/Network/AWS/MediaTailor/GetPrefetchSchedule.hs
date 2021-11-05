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
-- Module      : Network.AWS.MediaTailor.GetPrefetchSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the prefetch schedule for a specific playback
-- configuration. If you call GetPrefetchSchedule on an expired prefetch
-- schedule, MediaTailor returns an HTTP 404 status code.
module Network.AWS.MediaTailor.GetPrefetchSchedule
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
    getPrefetchScheduleResponse_playbackConfigurationName,
    getPrefetchScheduleResponse_retrieval,
    getPrefetchScheduleResponse_name,
    getPrefetchScheduleResponse_consumption,
    getPrefetchScheduleResponse_streamId,
    getPrefetchScheduleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaTailor.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPrefetchSchedule' smart constructor.
data GetPrefetchSchedule = GetPrefetchSchedule'
  { -- | The identifier for the playback configuration.
    name :: Prelude.Text,
    -- | The name of the playback configuration.
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
-- 'name', 'getPrefetchSchedule_name' - The identifier for the playback configuration.
--
-- 'playbackConfigurationName', 'getPrefetchSchedule_playbackConfigurationName' - The name of the playback configuration.
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

-- | The identifier for the playback configuration.
getPrefetchSchedule_name :: Lens.Lens' GetPrefetchSchedule Prelude.Text
getPrefetchSchedule_name = Lens.lens (\GetPrefetchSchedule' {name} -> name) (\s@GetPrefetchSchedule' {} a -> s {name = a} :: GetPrefetchSchedule)

-- | The name of the playback configuration.
getPrefetchSchedule_playbackConfigurationName :: Lens.Lens' GetPrefetchSchedule Prelude.Text
getPrefetchSchedule_playbackConfigurationName = Lens.lens (\GetPrefetchSchedule' {playbackConfigurationName} -> playbackConfigurationName) (\s@GetPrefetchSchedule' {} a -> s {playbackConfigurationName = a} :: GetPrefetchSchedule)

instance Core.AWSRequest GetPrefetchSchedule where
  type
    AWSResponse GetPrefetchSchedule =
      GetPrefetchScheduleResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPrefetchScheduleResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "PlaybackConfigurationName")
            Prelude.<*> (x Core..?> "Retrieval")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Consumption")
            Prelude.<*> (x Core..?> "StreamId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPrefetchSchedule

instance Prelude.NFData GetPrefetchSchedule

instance Core.ToHeaders GetPrefetchSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetPrefetchSchedule where
  toPath GetPrefetchSchedule' {..} =
    Prelude.mconcat
      [ "/prefetchSchedule/",
        Core.toBS playbackConfigurationName,
        "/",
        Core.toBS name
      ]

instance Core.ToQuery GetPrefetchSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPrefetchScheduleResponse' smart constructor.
data GetPrefetchScheduleResponse = GetPrefetchScheduleResponse'
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
-- Create a value of 'GetPrefetchScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getPrefetchScheduleResponse_arn' - The Amazon Resource Name (ARN) of the prefetch schedule.
--
-- 'playbackConfigurationName', 'getPrefetchScheduleResponse_playbackConfigurationName' - The name of the playback configuration to create the prefetch schedule
-- for.
--
-- 'retrieval', 'getPrefetchScheduleResponse_retrieval' - A complex type that contains settings for prefetch retrieval from the ad
-- decision server (ADS).
--
-- 'name', 'getPrefetchScheduleResponse_name' - The name of the prefetch schedule. The name must be unique among all
-- prefetch schedules that are associated with the specified playback
-- configuration.
--
-- 'consumption', 'getPrefetchScheduleResponse_consumption' - Consumption settings determine how, and when, MediaTailor places the
-- prefetched ads into ad breaks. Ad consumption occurs within a span of
-- time that you define, called a /consumption window/. You can designate
-- which ad breaks that MediaTailor fills with prefetch ads by setting
-- avail matching criteria.
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
      playbackConfigurationName = Prelude.Nothing,
      retrieval = Prelude.Nothing,
      name = Prelude.Nothing,
      consumption = Prelude.Nothing,
      streamId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the prefetch schedule.
getPrefetchScheduleResponse_arn :: Lens.Lens' GetPrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
getPrefetchScheduleResponse_arn = Lens.lens (\GetPrefetchScheduleResponse' {arn} -> arn) (\s@GetPrefetchScheduleResponse' {} a -> s {arn = a} :: GetPrefetchScheduleResponse)

-- | The name of the playback configuration to create the prefetch schedule
-- for.
getPrefetchScheduleResponse_playbackConfigurationName :: Lens.Lens' GetPrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
getPrefetchScheduleResponse_playbackConfigurationName = Lens.lens (\GetPrefetchScheduleResponse' {playbackConfigurationName} -> playbackConfigurationName) (\s@GetPrefetchScheduleResponse' {} a -> s {playbackConfigurationName = a} :: GetPrefetchScheduleResponse)

-- | A complex type that contains settings for prefetch retrieval from the ad
-- decision server (ADS).
getPrefetchScheduleResponse_retrieval :: Lens.Lens' GetPrefetchScheduleResponse (Prelude.Maybe PrefetchRetrieval)
getPrefetchScheduleResponse_retrieval = Lens.lens (\GetPrefetchScheduleResponse' {retrieval} -> retrieval) (\s@GetPrefetchScheduleResponse' {} a -> s {retrieval = a} :: GetPrefetchScheduleResponse)

-- | The name of the prefetch schedule. The name must be unique among all
-- prefetch schedules that are associated with the specified playback
-- configuration.
getPrefetchScheduleResponse_name :: Lens.Lens' GetPrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
getPrefetchScheduleResponse_name = Lens.lens (\GetPrefetchScheduleResponse' {name} -> name) (\s@GetPrefetchScheduleResponse' {} a -> s {name = a} :: GetPrefetchScheduleResponse)

-- | Consumption settings determine how, and when, MediaTailor places the
-- prefetched ads into ad breaks. Ad consumption occurs within a span of
-- time that you define, called a /consumption window/. You can designate
-- which ad breaks that MediaTailor fills with prefetch ads by setting
-- avail matching criteria.
getPrefetchScheduleResponse_consumption :: Lens.Lens' GetPrefetchScheduleResponse (Prelude.Maybe PrefetchConsumption)
getPrefetchScheduleResponse_consumption = Lens.lens (\GetPrefetchScheduleResponse' {consumption} -> consumption) (\s@GetPrefetchScheduleResponse' {} a -> s {consumption = a} :: GetPrefetchScheduleResponse)

-- | An optional stream identifier that you can specify in order to prefetch
-- for multiple streams that use the same playback configuration.
getPrefetchScheduleResponse_streamId :: Lens.Lens' GetPrefetchScheduleResponse (Prelude.Maybe Prelude.Text)
getPrefetchScheduleResponse_streamId = Lens.lens (\GetPrefetchScheduleResponse' {streamId} -> streamId) (\s@GetPrefetchScheduleResponse' {} a -> s {streamId = a} :: GetPrefetchScheduleResponse)

-- | The response's http status code.
getPrefetchScheduleResponse_httpStatus :: Lens.Lens' GetPrefetchScheduleResponse Prelude.Int
getPrefetchScheduleResponse_httpStatus = Lens.lens (\GetPrefetchScheduleResponse' {httpStatus} -> httpStatus) (\s@GetPrefetchScheduleResponse' {} a -> s {httpStatus = a} :: GetPrefetchScheduleResponse)

instance Prelude.NFData GetPrefetchScheduleResponse
