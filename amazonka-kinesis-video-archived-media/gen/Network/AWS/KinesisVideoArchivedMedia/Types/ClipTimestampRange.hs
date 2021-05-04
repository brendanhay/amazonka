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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The range of timestamps for which to return fragments.
--
-- The values in the ClipTimestampRange are @inclusive@. Fragments that
-- begin before the start time but continue past it, or fragments that
-- begin before the end time but continue past it, are included in the
-- session.
--
-- /See:/ 'newClipTimestampRange' smart constructor.
data ClipTimestampRange = ClipTimestampRange'
  { -- | The starting timestamp in the range of timestamps for which to return
    -- fragments.
    --
    -- This value is inclusive. Fragments that start before the
    -- @StartTimestamp@ and continue past it are included in the session. If
    -- @FragmentSelectorType@ is @SERVER_TIMESTAMP@, the @StartTimestamp@ must
    -- be later than the stream head.
    startTimestamp :: Prelude.POSIX,
    -- | The end of the timestamp range for the requested media.
    --
    -- This value must be within 3 hours of the specified @StartTimestamp@, and
    -- it must be later than the @StartTimestamp@ value. If
    -- @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@, this value
    -- must be in the past.
    --
    -- This value is inclusive. The @EndTimestamp@ is compared to the
    -- (starting) timestamp of the fragment. Fragments that start before the
    -- @EndTimestamp@ value and continue past it are included in the session.
    endTimestamp :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ClipTimestampRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTimestamp', 'clipTimestampRange_startTimestamp' - The starting timestamp in the range of timestamps for which to return
-- fragments.
--
-- This value is inclusive. Fragments that start before the
-- @StartTimestamp@ and continue past it are included in the session. If
-- @FragmentSelectorType@ is @SERVER_TIMESTAMP@, the @StartTimestamp@ must
-- be later than the stream head.
--
-- 'endTimestamp', 'clipTimestampRange_endTimestamp' - The end of the timestamp range for the requested media.
--
-- This value must be within 3 hours of the specified @StartTimestamp@, and
-- it must be later than the @StartTimestamp@ value. If
-- @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@, this value
-- must be in the past.
--
-- This value is inclusive. The @EndTimestamp@ is compared to the
-- (starting) timestamp of the fragment. Fragments that start before the
-- @EndTimestamp@ value and continue past it are included in the session.
newClipTimestampRange ::
  -- | 'startTimestamp'
  Prelude.UTCTime ->
  -- | 'endTimestamp'
  Prelude.UTCTime ->
  ClipTimestampRange
newClipTimestampRange pStartTimestamp_ pEndTimestamp_ =
  ClipTimestampRange'
    { startTimestamp =
        Prelude._Time Lens.# pStartTimestamp_,
      endTimestamp = Prelude._Time Lens.# pEndTimestamp_
    }

-- | The starting timestamp in the range of timestamps for which to return
-- fragments.
--
-- This value is inclusive. Fragments that start before the
-- @StartTimestamp@ and continue past it are included in the session. If
-- @FragmentSelectorType@ is @SERVER_TIMESTAMP@, the @StartTimestamp@ must
-- be later than the stream head.
clipTimestampRange_startTimestamp :: Lens.Lens' ClipTimestampRange Prelude.UTCTime
clipTimestampRange_startTimestamp = Lens.lens (\ClipTimestampRange' {startTimestamp} -> startTimestamp) (\s@ClipTimestampRange' {} a -> s {startTimestamp = a} :: ClipTimestampRange) Prelude.. Prelude._Time

-- | The end of the timestamp range for the requested media.
--
-- This value must be within 3 hours of the specified @StartTimestamp@, and
-- it must be later than the @StartTimestamp@ value. If
-- @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@, this value
-- must be in the past.
--
-- This value is inclusive. The @EndTimestamp@ is compared to the
-- (starting) timestamp of the fragment. Fragments that start before the
-- @EndTimestamp@ value and continue past it are included in the session.
clipTimestampRange_endTimestamp :: Lens.Lens' ClipTimestampRange Prelude.UTCTime
clipTimestampRange_endTimestamp = Lens.lens (\ClipTimestampRange' {endTimestamp} -> endTimestamp) (\s@ClipTimestampRange' {} a -> s {endTimestamp = a} :: ClipTimestampRange) Prelude.. Prelude._Time

instance Prelude.Hashable ClipTimestampRange

instance Prelude.NFData ClipTimestampRange

instance Prelude.ToJSON ClipTimestampRange where
  toJSON ClipTimestampRange' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("StartTimestamp" Prelude..= startTimestamp),
            Prelude.Just
              ("EndTimestamp" Prelude..= endTimestamp)
          ]
      )
