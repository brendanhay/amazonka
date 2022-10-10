{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaTailor.Types.PrefetchSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.PrefetchSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types.PrefetchConsumption
import Amazonka.MediaTailor.Types.PrefetchRetrieval
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains prefetch schedule information.
--
-- /See:/ 'newPrefetchSchedule' smart constructor.
data PrefetchSchedule = PrefetchSchedule'
  { -- | An optional stream identifier that you can specify in order to prefetch
    -- for multiple streams that use the same playback configuration.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains settings for prefetch retrieval from the ad
    -- decision server (ADS).
    retrieval :: PrefetchRetrieval,
    -- | Consumption settings determine how, and when, MediaTailor places the
    -- prefetched ads into ad breaks. Ad consumption occurs within a span of
    -- time that you define, called a /consumption window/. You can designate
    -- which ad breaks that MediaTailor fills with prefetch ads by setting
    -- avail matching criteria.
    consumption :: PrefetchConsumption,
    -- | The Amazon Resource Name (ARN) of the prefetch schedule.
    arn :: Prelude.Text,
    -- | The name of the playback configuration to create the prefetch schedule
    -- for.
    playbackConfigurationName :: Prelude.Text,
    -- | The name of the prefetch schedule. The name must be unique among all
    -- prefetch schedules that are associated with the specified playback
    -- configuration.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrefetchSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamId', 'prefetchSchedule_streamId' - An optional stream identifier that you can specify in order to prefetch
-- for multiple streams that use the same playback configuration.
--
-- 'retrieval', 'prefetchSchedule_retrieval' - A complex type that contains settings for prefetch retrieval from the ad
-- decision server (ADS).
--
-- 'consumption', 'prefetchSchedule_consumption' - Consumption settings determine how, and when, MediaTailor places the
-- prefetched ads into ad breaks. Ad consumption occurs within a span of
-- time that you define, called a /consumption window/. You can designate
-- which ad breaks that MediaTailor fills with prefetch ads by setting
-- avail matching criteria.
--
-- 'arn', 'prefetchSchedule_arn' - The Amazon Resource Name (ARN) of the prefetch schedule.
--
-- 'playbackConfigurationName', 'prefetchSchedule_playbackConfigurationName' - The name of the playback configuration to create the prefetch schedule
-- for.
--
-- 'name', 'prefetchSchedule_name' - The name of the prefetch schedule. The name must be unique among all
-- prefetch schedules that are associated with the specified playback
-- configuration.
newPrefetchSchedule ::
  -- | 'retrieval'
  PrefetchRetrieval ->
  -- | 'consumption'
  PrefetchConsumption ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'playbackConfigurationName'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  PrefetchSchedule
newPrefetchSchedule
  pRetrieval_
  pConsumption_
  pArn_
  pPlaybackConfigurationName_
  pName_ =
    PrefetchSchedule'
      { streamId = Prelude.Nothing,
        retrieval = pRetrieval_,
        consumption = pConsumption_,
        arn = pArn_,
        playbackConfigurationName =
          pPlaybackConfigurationName_,
        name = pName_
      }

-- | An optional stream identifier that you can specify in order to prefetch
-- for multiple streams that use the same playback configuration.
prefetchSchedule_streamId :: Lens.Lens' PrefetchSchedule (Prelude.Maybe Prelude.Text)
prefetchSchedule_streamId = Lens.lens (\PrefetchSchedule' {streamId} -> streamId) (\s@PrefetchSchedule' {} a -> s {streamId = a} :: PrefetchSchedule)

-- | A complex type that contains settings for prefetch retrieval from the ad
-- decision server (ADS).
prefetchSchedule_retrieval :: Lens.Lens' PrefetchSchedule PrefetchRetrieval
prefetchSchedule_retrieval = Lens.lens (\PrefetchSchedule' {retrieval} -> retrieval) (\s@PrefetchSchedule' {} a -> s {retrieval = a} :: PrefetchSchedule)

-- | Consumption settings determine how, and when, MediaTailor places the
-- prefetched ads into ad breaks. Ad consumption occurs within a span of
-- time that you define, called a /consumption window/. You can designate
-- which ad breaks that MediaTailor fills with prefetch ads by setting
-- avail matching criteria.
prefetchSchedule_consumption :: Lens.Lens' PrefetchSchedule PrefetchConsumption
prefetchSchedule_consumption = Lens.lens (\PrefetchSchedule' {consumption} -> consumption) (\s@PrefetchSchedule' {} a -> s {consumption = a} :: PrefetchSchedule)

-- | The Amazon Resource Name (ARN) of the prefetch schedule.
prefetchSchedule_arn :: Lens.Lens' PrefetchSchedule Prelude.Text
prefetchSchedule_arn = Lens.lens (\PrefetchSchedule' {arn} -> arn) (\s@PrefetchSchedule' {} a -> s {arn = a} :: PrefetchSchedule)

-- | The name of the playback configuration to create the prefetch schedule
-- for.
prefetchSchedule_playbackConfigurationName :: Lens.Lens' PrefetchSchedule Prelude.Text
prefetchSchedule_playbackConfigurationName = Lens.lens (\PrefetchSchedule' {playbackConfigurationName} -> playbackConfigurationName) (\s@PrefetchSchedule' {} a -> s {playbackConfigurationName = a} :: PrefetchSchedule)

-- | The name of the prefetch schedule. The name must be unique among all
-- prefetch schedules that are associated with the specified playback
-- configuration.
prefetchSchedule_name :: Lens.Lens' PrefetchSchedule Prelude.Text
prefetchSchedule_name = Lens.lens (\PrefetchSchedule' {name} -> name) (\s@PrefetchSchedule' {} a -> s {name = a} :: PrefetchSchedule)

instance Core.FromJSON PrefetchSchedule where
  parseJSON =
    Core.withObject
      "PrefetchSchedule"
      ( \x ->
          PrefetchSchedule'
            Prelude.<$> (x Core..:? "StreamId")
            Prelude.<*> (x Core..: "Retrieval")
            Prelude.<*> (x Core..: "Consumption")
            Prelude.<*> (x Core..: "Arn")
            Prelude.<*> (x Core..: "PlaybackConfigurationName")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable PrefetchSchedule where
  hashWithSalt _salt PrefetchSchedule' {..} =
    _salt `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` retrieval
      `Prelude.hashWithSalt` consumption
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` playbackConfigurationName
      `Prelude.hashWithSalt` name

instance Prelude.NFData PrefetchSchedule where
  rnf PrefetchSchedule' {..} =
    Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf retrieval
      `Prelude.seq` Prelude.rnf consumption
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf playbackConfigurationName
      `Prelude.seq` Prelude.rnf name
