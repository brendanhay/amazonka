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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.PrefetchSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types.PrefetchConsumption
import Amazonka.MediaTailor.Types.PrefetchRetrieval
import qualified Amazonka.Prelude as Prelude

-- | A prefetch schedule allows you to tell MediaTailor to fetch and prepare
-- certain ads before an ad break happens. For more information about ad
-- prefetching, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/prefetching-ads.html Using ad prefetching>
-- in the /MediaTailor User Guide/.
--
-- /See:/ 'newPrefetchSchedule' smart constructor.
data PrefetchSchedule = PrefetchSchedule'
  { -- | An optional stream identifier that you can specify in order to prefetch
    -- for multiple streams that use the same playback configuration.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the prefetch schedule.
    arn :: Prelude.Text,
    -- | Consumption settings determine how, and when, MediaTailor places the
    -- prefetched ads into ad breaks. Ad consumption occurs within a span of
    -- time that you define, called a /consumption window/. You can designate
    -- which ad breaks that MediaTailor fills with prefetch ads by setting
    -- avail matching criteria.
    consumption :: PrefetchConsumption,
    -- | The name of the prefetch schedule. The name must be unique among all
    -- prefetch schedules that are associated with the specified playback
    -- configuration.
    name :: Prelude.Text,
    -- | The name of the playback configuration to create the prefetch schedule
    -- for.
    playbackConfigurationName :: Prelude.Text,
    -- | A complex type that contains settings for prefetch retrieval from the ad
    -- decision server (ADS).
    retrieval :: PrefetchRetrieval
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
-- 'arn', 'prefetchSchedule_arn' - The Amazon Resource Name (ARN) of the prefetch schedule.
--
-- 'consumption', 'prefetchSchedule_consumption' - Consumption settings determine how, and when, MediaTailor places the
-- prefetched ads into ad breaks. Ad consumption occurs within a span of
-- time that you define, called a /consumption window/. You can designate
-- which ad breaks that MediaTailor fills with prefetch ads by setting
-- avail matching criteria.
--
-- 'name', 'prefetchSchedule_name' - The name of the prefetch schedule. The name must be unique among all
-- prefetch schedules that are associated with the specified playback
-- configuration.
--
-- 'playbackConfigurationName', 'prefetchSchedule_playbackConfigurationName' - The name of the playback configuration to create the prefetch schedule
-- for.
--
-- 'retrieval', 'prefetchSchedule_retrieval' - A complex type that contains settings for prefetch retrieval from the ad
-- decision server (ADS).
newPrefetchSchedule ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'consumption'
  PrefetchConsumption ->
  -- | 'name'
  Prelude.Text ->
  -- | 'playbackConfigurationName'
  Prelude.Text ->
  -- | 'retrieval'
  PrefetchRetrieval ->
  PrefetchSchedule
newPrefetchSchedule
  pArn_
  pConsumption_
  pName_
  pPlaybackConfigurationName_
  pRetrieval_ =
    PrefetchSchedule'
      { streamId = Prelude.Nothing,
        arn = pArn_,
        consumption = pConsumption_,
        name = pName_,
        playbackConfigurationName =
          pPlaybackConfigurationName_,
        retrieval = pRetrieval_
      }

-- | An optional stream identifier that you can specify in order to prefetch
-- for multiple streams that use the same playback configuration.
prefetchSchedule_streamId :: Lens.Lens' PrefetchSchedule (Prelude.Maybe Prelude.Text)
prefetchSchedule_streamId = Lens.lens (\PrefetchSchedule' {streamId} -> streamId) (\s@PrefetchSchedule' {} a -> s {streamId = a} :: PrefetchSchedule)

-- | The Amazon Resource Name (ARN) of the prefetch schedule.
prefetchSchedule_arn :: Lens.Lens' PrefetchSchedule Prelude.Text
prefetchSchedule_arn = Lens.lens (\PrefetchSchedule' {arn} -> arn) (\s@PrefetchSchedule' {} a -> s {arn = a} :: PrefetchSchedule)

-- | Consumption settings determine how, and when, MediaTailor places the
-- prefetched ads into ad breaks. Ad consumption occurs within a span of
-- time that you define, called a /consumption window/. You can designate
-- which ad breaks that MediaTailor fills with prefetch ads by setting
-- avail matching criteria.
prefetchSchedule_consumption :: Lens.Lens' PrefetchSchedule PrefetchConsumption
prefetchSchedule_consumption = Lens.lens (\PrefetchSchedule' {consumption} -> consumption) (\s@PrefetchSchedule' {} a -> s {consumption = a} :: PrefetchSchedule)

-- | The name of the prefetch schedule. The name must be unique among all
-- prefetch schedules that are associated with the specified playback
-- configuration.
prefetchSchedule_name :: Lens.Lens' PrefetchSchedule Prelude.Text
prefetchSchedule_name = Lens.lens (\PrefetchSchedule' {name} -> name) (\s@PrefetchSchedule' {} a -> s {name = a} :: PrefetchSchedule)

-- | The name of the playback configuration to create the prefetch schedule
-- for.
prefetchSchedule_playbackConfigurationName :: Lens.Lens' PrefetchSchedule Prelude.Text
prefetchSchedule_playbackConfigurationName = Lens.lens (\PrefetchSchedule' {playbackConfigurationName} -> playbackConfigurationName) (\s@PrefetchSchedule' {} a -> s {playbackConfigurationName = a} :: PrefetchSchedule)

-- | A complex type that contains settings for prefetch retrieval from the ad
-- decision server (ADS).
prefetchSchedule_retrieval :: Lens.Lens' PrefetchSchedule PrefetchRetrieval
prefetchSchedule_retrieval = Lens.lens (\PrefetchSchedule' {retrieval} -> retrieval) (\s@PrefetchSchedule' {} a -> s {retrieval = a} :: PrefetchSchedule)

instance Data.FromJSON PrefetchSchedule where
  parseJSON =
    Data.withObject
      "PrefetchSchedule"
      ( \x ->
          PrefetchSchedule'
            Prelude.<$> (x Data..:? "StreamId")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "Consumption")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "PlaybackConfigurationName")
            Prelude.<*> (x Data..: "Retrieval")
      )

instance Prelude.Hashable PrefetchSchedule where
  hashWithSalt _salt PrefetchSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` consumption
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` playbackConfigurationName
      `Prelude.hashWithSalt` retrieval

instance Prelude.NFData PrefetchSchedule where
  rnf PrefetchSchedule' {..} =
    Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf consumption
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf playbackConfigurationName
      `Prelude.seq` Prelude.rnf retrieval
