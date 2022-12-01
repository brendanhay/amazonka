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
-- Module      : Amazonka.KinesisVideoArchivedMedia.Types.HLSTimestampRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoArchivedMedia.Types.HLSTimestampRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The start and end of the timestamp range for the requested media.
--
-- This value should not be present if @PlaybackType@ is @LIVE@.
--
-- /See:/ 'newHLSTimestampRange' smart constructor.
data HLSTimestampRange = HLSTimestampRange'
  { -- | The end of the timestamp range for the requested media. This value must
    -- be within 24 hours of the specified @StartTimestamp@, and it must be
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
    endTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The start of the timestamp range for the requested media.
    --
    -- If the @HLSTimestampRange@ value is specified, the @StartTimestamp@
    -- value is required.
    --
    -- Only fragments that start exactly at or after @StartTimestamp@ are
    -- included in the session. Fragments that start before @StartTimestamp@
    -- and continue past it aren\'t included in the session. If
    -- @FragmentSelectorType@ is @SERVER_TIMESTAMP@, the @StartTimestamp@ must
    -- be later than the stream head.
    startTimestamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HLSTimestampRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTimestamp', 'hLSTimestampRange_endTimestamp' - The end of the timestamp range for the requested media. This value must
-- be within 24 hours of the specified @StartTimestamp@, and it must be
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
-- Only fragments that start exactly at or after @StartTimestamp@ are
-- included in the session. Fragments that start before @StartTimestamp@
-- and continue past it aren\'t included in the session. If
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
-- be within 24 hours of the specified @StartTimestamp@, and it must be
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
hLSTimestampRange_endTimestamp = Lens.lens (\HLSTimestampRange' {endTimestamp} -> endTimestamp) (\s@HLSTimestampRange' {} a -> s {endTimestamp = a} :: HLSTimestampRange) Prelude.. Lens.mapping Core._Time

-- | The start of the timestamp range for the requested media.
--
-- If the @HLSTimestampRange@ value is specified, the @StartTimestamp@
-- value is required.
--
-- Only fragments that start exactly at or after @StartTimestamp@ are
-- included in the session. Fragments that start before @StartTimestamp@
-- and continue past it aren\'t included in the session. If
-- @FragmentSelectorType@ is @SERVER_TIMESTAMP@, the @StartTimestamp@ must
-- be later than the stream head.
hLSTimestampRange_startTimestamp :: Lens.Lens' HLSTimestampRange (Prelude.Maybe Prelude.UTCTime)
hLSTimestampRange_startTimestamp = Lens.lens (\HLSTimestampRange' {startTimestamp} -> startTimestamp) (\s@HLSTimestampRange' {} a -> s {startTimestamp = a} :: HLSTimestampRange) Prelude.. Lens.mapping Core._Time

instance Prelude.Hashable HLSTimestampRange where
  hashWithSalt _salt HLSTimestampRange' {..} =
    _salt `Prelude.hashWithSalt` endTimestamp
      `Prelude.hashWithSalt` startTimestamp

instance Prelude.NFData HLSTimestampRange where
  rnf HLSTimestampRange' {..} =
    Prelude.rnf endTimestamp
      `Prelude.seq` Prelude.rnf startTimestamp

instance Core.ToJSON HLSTimestampRange where
  toJSON HLSTimestampRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EndTimestamp" Core..=) Prelude.<$> endTimestamp,
            ("StartTimestamp" Core..=)
              Prelude.<$> startTimestamp
          ]
      )
