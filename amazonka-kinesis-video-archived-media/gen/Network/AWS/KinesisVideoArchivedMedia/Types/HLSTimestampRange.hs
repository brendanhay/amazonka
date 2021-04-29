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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.HLSTimestampRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.HLSTimestampRange where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The start and end of the timestamp range for the requested media.
--
-- This value should not be present if @PlaybackType@ is @LIVE@.
--
-- The values in the @HLSTimestampRange@ are inclusive. Fragments that
-- begin before the start time but continue past it, or fragments that
-- begin before the end time but continue past it, are included in the
-- session.
--
-- /See:/ 'newHLSTimestampRange' smart constructor.
data HLSTimestampRange = HLSTimestampRange'
  { -- | The end of the timestamp range for the requested media. This value must
    -- be within 3 hours of the specified @StartTimestamp@, and it must be
    -- later than the @StartTimestamp@ value.
    --
    -- If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@, this
    -- value must be in the past.
    --
    -- The @EndTimestamp@ value is required for @ON_DEMAND@ mode, but optional
    -- for @LIVE_REPLAY@ mode. If the @EndTimestamp@ is not set for
    -- @LIVE_REPLAY@ mode then the session will continue to include newly
    -- ingested fragments until the session expires.
    --
    -- This value is inclusive. The @EndTimestamp@ is compared to the
    -- (starting) timestamp of the fragment. Fragments that start before the
    -- @EndTimestamp@ value and continue past it are included in the session.
    endTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The start of the timestamp range for the requested media.
    --
    -- If the @HLSTimestampRange@ value is specified, the @StartTimestamp@
    -- value is required.
    --
    -- This value is inclusive. Fragments that start before the
    -- @StartTimestamp@ and continue past it are included in the session. If
    -- @FragmentSelectorType@ is @SERVER_TIMESTAMP@, the @StartTimestamp@ must
    -- be later than the stream head.
    startTimestamp :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HLSTimestampRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTimestamp', 'hLSTimestampRange_endTimestamp' - The end of the timestamp range for the requested media. This value must
-- be within 3 hours of the specified @StartTimestamp@, and it must be
-- later than the @StartTimestamp@ value.
--
-- If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@, this
-- value must be in the past.
--
-- The @EndTimestamp@ value is required for @ON_DEMAND@ mode, but optional
-- for @LIVE_REPLAY@ mode. If the @EndTimestamp@ is not set for
-- @LIVE_REPLAY@ mode then the session will continue to include newly
-- ingested fragments until the session expires.
--
-- This value is inclusive. The @EndTimestamp@ is compared to the
-- (starting) timestamp of the fragment. Fragments that start before the
-- @EndTimestamp@ value and continue past it are included in the session.
--
-- 'startTimestamp', 'hLSTimestampRange_startTimestamp' - The start of the timestamp range for the requested media.
--
-- If the @HLSTimestampRange@ value is specified, the @StartTimestamp@
-- value is required.
--
-- This value is inclusive. Fragments that start before the
-- @StartTimestamp@ and continue past it are included in the session. If
-- @FragmentSelectorType@ is @SERVER_TIMESTAMP@, the @StartTimestamp@ must
-- be later than the stream head.
newHLSTimestampRange ::
  HLSTimestampRange
newHLSTimestampRange =
  HLSTimestampRange'
    { endTimestamp = Prelude.Nothing,
      startTimestamp = Prelude.Nothing
    }

-- | The end of the timestamp range for the requested media. This value must
-- be within 3 hours of the specified @StartTimestamp@, and it must be
-- later than the @StartTimestamp@ value.
--
-- If @FragmentSelectorType@ for the request is @SERVER_TIMESTAMP@, this
-- value must be in the past.
--
-- The @EndTimestamp@ value is required for @ON_DEMAND@ mode, but optional
-- for @LIVE_REPLAY@ mode. If the @EndTimestamp@ is not set for
-- @LIVE_REPLAY@ mode then the session will continue to include newly
-- ingested fragments until the session expires.
--
-- This value is inclusive. The @EndTimestamp@ is compared to the
-- (starting) timestamp of the fragment. Fragments that start before the
-- @EndTimestamp@ value and continue past it are included in the session.
hLSTimestampRange_endTimestamp :: Lens.Lens' HLSTimestampRange (Prelude.Maybe Prelude.UTCTime)
hLSTimestampRange_endTimestamp = Lens.lens (\HLSTimestampRange' {endTimestamp} -> endTimestamp) (\s@HLSTimestampRange' {} a -> s {endTimestamp = a} :: HLSTimestampRange) Prelude.. Lens.mapping Prelude._Time

-- | The start of the timestamp range for the requested media.
--
-- If the @HLSTimestampRange@ value is specified, the @StartTimestamp@
-- value is required.
--
-- This value is inclusive. Fragments that start before the
-- @StartTimestamp@ and continue past it are included in the session. If
-- @FragmentSelectorType@ is @SERVER_TIMESTAMP@, the @StartTimestamp@ must
-- be later than the stream head.
hLSTimestampRange_startTimestamp :: Lens.Lens' HLSTimestampRange (Prelude.Maybe Prelude.UTCTime)
hLSTimestampRange_startTimestamp = Lens.lens (\HLSTimestampRange' {startTimestamp} -> startTimestamp) (\s@HLSTimestampRange' {} a -> s {startTimestamp = a} :: HLSTimestampRange) Prelude.. Lens.mapping Prelude._Time

instance Prelude.Hashable HLSTimestampRange

instance Prelude.NFData HLSTimestampRange

instance Prelude.ToJSON HLSTimestampRange where
  toJSON HLSTimestampRange' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EndTimestamp" Prelude..=)
              Prelude.<$> endTimestamp,
            ("StartTimestamp" Prelude..=)
              Prelude.<$> startTimestamp
          ]
      )
