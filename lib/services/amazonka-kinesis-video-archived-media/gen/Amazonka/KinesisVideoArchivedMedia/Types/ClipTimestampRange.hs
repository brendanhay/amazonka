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
-- Module      : Amazonka.KinesisVideoArchivedMedia.Types.ClipTimestampRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoArchivedMedia.Types.ClipTimestampRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The range of timestamps for which to return fragments.
--
-- /See:/ 'newClipTimestampRange' smart constructor.
data ClipTimestampRange = ClipTimestampRange'
  { -- | The starting timestamp in the range of timestamps for which to return
    -- fragments.
    --
    -- Only fragments that start exactly at or after @StartTimestamp@ are
    -- included in the session. Fragments that start before @StartTimestamp@
    -- and continue past it aren\'t included in the session. If
    -- @FragmentSelectorType@ is @SERVER_TIMESTAMP@, the @StartTimestamp@ must
    -- be later than the stream head.
    startTimestamp :: Data.POSIX,
    -- | The end of the timestamp range for the requested media.
    --
    -- This value must be within 24 hours of the specified @StartTimestamp@,
    -- and it must be later than the @StartTimestamp@ value. If
    -- @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@, this value
    -- must be in the past.
    --
    -- This value is inclusive. The @EndTimestamp@ is compared to the
    -- (starting) timestamp of the fragment. Fragments that start before the
    -- @EndTimestamp@ value and continue past it are included in the session.
    endTimestamp :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- Only fragments that start exactly at or after @StartTimestamp@ are
-- included in the session. Fragments that start before @StartTimestamp@
-- and continue past it aren\'t included in the session. If
-- @FragmentSelectorType@ is @SERVER_TIMESTAMP@, the @StartTimestamp@ must
-- be later than the stream head.
--
-- 'endTimestamp', 'clipTimestampRange_endTimestamp' - The end of the timestamp range for the requested media.
--
-- This value must be within 24 hours of the specified @StartTimestamp@,
-- and it must be later than the @StartTimestamp@ value. If
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
        Data._Time Lens.# pStartTimestamp_,
      endTimestamp = Data._Time Lens.# pEndTimestamp_
    }

-- | The starting timestamp in the range of timestamps for which to return
-- fragments.
--
-- Only fragments that start exactly at or after @StartTimestamp@ are
-- included in the session. Fragments that start before @StartTimestamp@
-- and continue past it aren\'t included in the session. If
-- @FragmentSelectorType@ is @SERVER_TIMESTAMP@, the @StartTimestamp@ must
-- be later than the stream head.
clipTimestampRange_startTimestamp :: Lens.Lens' ClipTimestampRange Prelude.UTCTime
clipTimestampRange_startTimestamp = Lens.lens (\ClipTimestampRange' {startTimestamp} -> startTimestamp) (\s@ClipTimestampRange' {} a -> s {startTimestamp = a} :: ClipTimestampRange) Prelude.. Data._Time

-- | The end of the timestamp range for the requested media.
--
-- This value must be within 24 hours of the specified @StartTimestamp@,
-- and it must be later than the @StartTimestamp@ value. If
-- @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@, this value
-- must be in the past.
--
-- This value is inclusive. The @EndTimestamp@ is compared to the
-- (starting) timestamp of the fragment. Fragments that start before the
-- @EndTimestamp@ value and continue past it are included in the session.
clipTimestampRange_endTimestamp :: Lens.Lens' ClipTimestampRange Prelude.UTCTime
clipTimestampRange_endTimestamp = Lens.lens (\ClipTimestampRange' {endTimestamp} -> endTimestamp) (\s@ClipTimestampRange' {} a -> s {endTimestamp = a} :: ClipTimestampRange) Prelude.. Data._Time

instance Prelude.Hashable ClipTimestampRange where
  hashWithSalt _salt ClipTimestampRange' {..} =
    _salt
      `Prelude.hashWithSalt` startTimestamp
      `Prelude.hashWithSalt` endTimestamp

instance Prelude.NFData ClipTimestampRange where
  rnf ClipTimestampRange' {..} =
    Prelude.rnf startTimestamp
      `Prelude.seq` Prelude.rnf endTimestamp

instance Data.ToJSON ClipTimestampRange where
  toJSON ClipTimestampRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("StartTimestamp" Data..= startTimestamp),
            Prelude.Just ("EndTimestamp" Data..= endTimestamp)
          ]
      )
