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
-- Module      : Amazonka.KinesisVideoArchivedMedia.Types.HLSFragmentSelector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoArchivedMedia.Types.HLSFragmentSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideoArchivedMedia.Types.HLSFragmentSelectorType
import Amazonka.KinesisVideoArchivedMedia.Types.HLSTimestampRange
import qualified Amazonka.Prelude as Prelude

-- | Contains the range of timestamps for the requested media, and the source
-- of the timestamps.
--
-- /See:/ 'newHLSFragmentSelector' smart constructor.
data HLSFragmentSelector = HLSFragmentSelector'
  { -- | The source of the timestamps for the requested media.
    --
    -- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and
    -- GetHLSStreamingSessionURLInput$PlaybackMode is @ON_DEMAND@ or
    -- @LIVE_REPLAY@, the first fragment ingested with a producer timestamp
    -- within the specified FragmentSelector$TimestampRange is included in the
    -- media playlist. In addition, the fragments with producer timestamps
    -- within the @TimestampRange@ ingested immediately following the first
    -- fragment (up to the
    -- GetHLSStreamingSessionURLInput$MaxMediaPlaylistFragmentResults value)
    -- are included.
    --
    -- Fragments that have duplicate producer timestamps are deduplicated. This
    -- means that if producers are producing a stream of fragments with
    -- producer timestamps that are approximately equal to the true clock time,
    -- the HLS media playlists will contain all of the fragments within the
    -- requested timestamp range. If some fragments are ingested within the
    -- same time range and very different points in time, only the oldest
    -- ingested collection of fragments are returned.
    --
    -- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and
    -- GetHLSStreamingSessionURLInput$PlaybackMode is @LIVE@, the producer
    -- timestamps are used in the MP4 fragments and for deduplication. But the
    -- most recently ingested fragments based on server timestamps are included
    -- in the HLS media playlist. This means that even if fragments ingested in
    -- the past have producer timestamps with values now, they are not included
    -- in the HLS media playlist.
    --
    -- The default is @SERVER_TIMESTAMP@.
    fragmentSelectorType :: Prelude.Maybe HLSFragmentSelectorType,
    -- | The start and end of the timestamp range for the requested media.
    --
    -- This value should not be present if @PlaybackType@ is @LIVE@.
    timestampRange :: Prelude.Maybe HLSTimestampRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HLSFragmentSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fragmentSelectorType', 'hLSFragmentSelector_fragmentSelectorType' - The source of the timestamps for the requested media.
--
-- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and
-- GetHLSStreamingSessionURLInput$PlaybackMode is @ON_DEMAND@ or
-- @LIVE_REPLAY@, the first fragment ingested with a producer timestamp
-- within the specified FragmentSelector$TimestampRange is included in the
-- media playlist. In addition, the fragments with producer timestamps
-- within the @TimestampRange@ ingested immediately following the first
-- fragment (up to the
-- GetHLSStreamingSessionURLInput$MaxMediaPlaylistFragmentResults value)
-- are included.
--
-- Fragments that have duplicate producer timestamps are deduplicated. This
-- means that if producers are producing a stream of fragments with
-- producer timestamps that are approximately equal to the true clock time,
-- the HLS media playlists will contain all of the fragments within the
-- requested timestamp range. If some fragments are ingested within the
-- same time range and very different points in time, only the oldest
-- ingested collection of fragments are returned.
--
-- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and
-- GetHLSStreamingSessionURLInput$PlaybackMode is @LIVE@, the producer
-- timestamps are used in the MP4 fragments and for deduplication. But the
-- most recently ingested fragments based on server timestamps are included
-- in the HLS media playlist. This means that even if fragments ingested in
-- the past have producer timestamps with values now, they are not included
-- in the HLS media playlist.
--
-- The default is @SERVER_TIMESTAMP@.
--
-- 'timestampRange', 'hLSFragmentSelector_timestampRange' - The start and end of the timestamp range for the requested media.
--
-- This value should not be present if @PlaybackType@ is @LIVE@.
newHLSFragmentSelector ::
  HLSFragmentSelector
newHLSFragmentSelector =
  HLSFragmentSelector'
    { fragmentSelectorType =
        Prelude.Nothing,
      timestampRange = Prelude.Nothing
    }

-- | The source of the timestamps for the requested media.
--
-- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and
-- GetHLSStreamingSessionURLInput$PlaybackMode is @ON_DEMAND@ or
-- @LIVE_REPLAY@, the first fragment ingested with a producer timestamp
-- within the specified FragmentSelector$TimestampRange is included in the
-- media playlist. In addition, the fragments with producer timestamps
-- within the @TimestampRange@ ingested immediately following the first
-- fragment (up to the
-- GetHLSStreamingSessionURLInput$MaxMediaPlaylistFragmentResults value)
-- are included.
--
-- Fragments that have duplicate producer timestamps are deduplicated. This
-- means that if producers are producing a stream of fragments with
-- producer timestamps that are approximately equal to the true clock time,
-- the HLS media playlists will contain all of the fragments within the
-- requested timestamp range. If some fragments are ingested within the
-- same time range and very different points in time, only the oldest
-- ingested collection of fragments are returned.
--
-- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and
-- GetHLSStreamingSessionURLInput$PlaybackMode is @LIVE@, the producer
-- timestamps are used in the MP4 fragments and for deduplication. But the
-- most recently ingested fragments based on server timestamps are included
-- in the HLS media playlist. This means that even if fragments ingested in
-- the past have producer timestamps with values now, they are not included
-- in the HLS media playlist.
--
-- The default is @SERVER_TIMESTAMP@.
hLSFragmentSelector_fragmentSelectorType :: Lens.Lens' HLSFragmentSelector (Prelude.Maybe HLSFragmentSelectorType)
hLSFragmentSelector_fragmentSelectorType = Lens.lens (\HLSFragmentSelector' {fragmentSelectorType} -> fragmentSelectorType) (\s@HLSFragmentSelector' {} a -> s {fragmentSelectorType = a} :: HLSFragmentSelector)

-- | The start and end of the timestamp range for the requested media.
--
-- This value should not be present if @PlaybackType@ is @LIVE@.
hLSFragmentSelector_timestampRange :: Lens.Lens' HLSFragmentSelector (Prelude.Maybe HLSTimestampRange)
hLSFragmentSelector_timestampRange = Lens.lens (\HLSFragmentSelector' {timestampRange} -> timestampRange) (\s@HLSFragmentSelector' {} a -> s {timestampRange = a} :: HLSFragmentSelector)

instance Prelude.Hashable HLSFragmentSelector where
  hashWithSalt _salt HLSFragmentSelector' {..} =
    _salt `Prelude.hashWithSalt` fragmentSelectorType
      `Prelude.hashWithSalt` timestampRange

instance Prelude.NFData HLSFragmentSelector where
  rnf HLSFragmentSelector' {..} =
    Prelude.rnf fragmentSelectorType
      `Prelude.seq` Prelude.rnf timestampRange

instance Data.ToJSON HLSFragmentSelector where
  toJSON HLSFragmentSelector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FragmentSelectorType" Data..=)
              Prelude.<$> fragmentSelectorType,
            ("TimestampRange" Data..=)
              Prelude.<$> timestampRange
          ]
      )
