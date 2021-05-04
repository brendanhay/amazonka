{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelector where

import Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHTimestampRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the range of timestamps for the requested media, and the source
-- of the timestamps.
--
-- /See:/ 'newDASHFragmentSelector' smart constructor.
data DASHFragmentSelector = DASHFragmentSelector'
  { -- | The source of the timestamps for the requested media.
    --
    -- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and
    -- GetDASHStreamingSessionURLInput$PlaybackMode is @ON_DEMAND@ or
    -- @LIVE_REPLAY@, the first fragment ingested with a producer timestamp
    -- within the specified FragmentSelector$TimestampRange is included in the
    -- media playlist. In addition, the fragments with producer timestamps
    -- within the @TimestampRange@ ingested immediately following the first
    -- fragment (up to the
    -- GetDASHStreamingSessionURLInput$MaxManifestFragmentResults value) are
    -- included.
    --
    -- Fragments that have duplicate producer timestamps are deduplicated. This
    -- means that if producers are producing a stream of fragments with
    -- producer timestamps that are approximately equal to the true clock time,
    -- the MPEG-DASH manifest will contain all of the fragments within the
    -- requested timestamp range. If some fragments are ingested within the
    -- same time range and very different points in time, only the oldest
    -- ingested collection of fragments are returned.
    --
    -- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and
    -- GetDASHStreamingSessionURLInput$PlaybackMode is @LIVE@, the producer
    -- timestamps are used in the MP4 fragments and for deduplication. But the
    -- most recently ingested fragments based on server timestamps are included
    -- in the MPEG-DASH manifest. This means that even if fragments ingested in
    -- the past have producer timestamps with values now, they are not included
    -- in the HLS media playlist.
    --
    -- The default is @SERVER_TIMESTAMP@.
    fragmentSelectorType :: Prelude.Maybe DASHFragmentSelectorType,
    -- | The start and end of the timestamp range for the requested media.
    --
    -- This value should not be present if @PlaybackType@ is @LIVE@.
    timestampRange :: Prelude.Maybe DASHTimestampRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DASHFragmentSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fragmentSelectorType', 'dASHFragmentSelector_fragmentSelectorType' - The source of the timestamps for the requested media.
--
-- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and
-- GetDASHStreamingSessionURLInput$PlaybackMode is @ON_DEMAND@ or
-- @LIVE_REPLAY@, the first fragment ingested with a producer timestamp
-- within the specified FragmentSelector$TimestampRange is included in the
-- media playlist. In addition, the fragments with producer timestamps
-- within the @TimestampRange@ ingested immediately following the first
-- fragment (up to the
-- GetDASHStreamingSessionURLInput$MaxManifestFragmentResults value) are
-- included.
--
-- Fragments that have duplicate producer timestamps are deduplicated. This
-- means that if producers are producing a stream of fragments with
-- producer timestamps that are approximately equal to the true clock time,
-- the MPEG-DASH manifest will contain all of the fragments within the
-- requested timestamp range. If some fragments are ingested within the
-- same time range and very different points in time, only the oldest
-- ingested collection of fragments are returned.
--
-- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and
-- GetDASHStreamingSessionURLInput$PlaybackMode is @LIVE@, the producer
-- timestamps are used in the MP4 fragments and for deduplication. But the
-- most recently ingested fragments based on server timestamps are included
-- in the MPEG-DASH manifest. This means that even if fragments ingested in
-- the past have producer timestamps with values now, they are not included
-- in the HLS media playlist.
--
-- The default is @SERVER_TIMESTAMP@.
--
-- 'timestampRange', 'dASHFragmentSelector_timestampRange' - The start and end of the timestamp range for the requested media.
--
-- This value should not be present if @PlaybackType@ is @LIVE@.
newDASHFragmentSelector ::
  DASHFragmentSelector
newDASHFragmentSelector =
  DASHFragmentSelector'
    { fragmentSelectorType =
        Prelude.Nothing,
      timestampRange = Prelude.Nothing
    }

-- | The source of the timestamps for the requested media.
--
-- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and
-- GetDASHStreamingSessionURLInput$PlaybackMode is @ON_DEMAND@ or
-- @LIVE_REPLAY@, the first fragment ingested with a producer timestamp
-- within the specified FragmentSelector$TimestampRange is included in the
-- media playlist. In addition, the fragments with producer timestamps
-- within the @TimestampRange@ ingested immediately following the first
-- fragment (up to the
-- GetDASHStreamingSessionURLInput$MaxManifestFragmentResults value) are
-- included.
--
-- Fragments that have duplicate producer timestamps are deduplicated. This
-- means that if producers are producing a stream of fragments with
-- producer timestamps that are approximately equal to the true clock time,
-- the MPEG-DASH manifest will contain all of the fragments within the
-- requested timestamp range. If some fragments are ingested within the
-- same time range and very different points in time, only the oldest
-- ingested collection of fragments are returned.
--
-- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and
-- GetDASHStreamingSessionURLInput$PlaybackMode is @LIVE@, the producer
-- timestamps are used in the MP4 fragments and for deduplication. But the
-- most recently ingested fragments based on server timestamps are included
-- in the MPEG-DASH manifest. This means that even if fragments ingested in
-- the past have producer timestamps with values now, they are not included
-- in the HLS media playlist.
--
-- The default is @SERVER_TIMESTAMP@.
dASHFragmentSelector_fragmentSelectorType :: Lens.Lens' DASHFragmentSelector (Prelude.Maybe DASHFragmentSelectorType)
dASHFragmentSelector_fragmentSelectorType = Lens.lens (\DASHFragmentSelector' {fragmentSelectorType} -> fragmentSelectorType) (\s@DASHFragmentSelector' {} a -> s {fragmentSelectorType = a} :: DASHFragmentSelector)

-- | The start and end of the timestamp range for the requested media.
--
-- This value should not be present if @PlaybackType@ is @LIVE@.
dASHFragmentSelector_timestampRange :: Lens.Lens' DASHFragmentSelector (Prelude.Maybe DASHTimestampRange)
dASHFragmentSelector_timestampRange = Lens.lens (\DASHFragmentSelector' {timestampRange} -> timestampRange) (\s@DASHFragmentSelector' {} a -> s {timestampRange = a} :: DASHFragmentSelector)

instance Prelude.Hashable DASHFragmentSelector

instance Prelude.NFData DASHFragmentSelector

instance Prelude.ToJSON DASHFragmentSelector where
  toJSON DASHFragmentSelector' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("FragmentSelectorType" Prelude..=)
              Prelude.<$> fragmentSelectorType,
            ("TimestampRange" Prelude..=)
              Prelude.<$> timestampRange
          ]
      )
