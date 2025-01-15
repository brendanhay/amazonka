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
-- Module      : Amazonka.KinesisVideoArchivedMedia.Types.DASHTimestampRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoArchivedMedia.Types.DASHTimestampRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The start and end of the timestamp range for the requested media.
--
-- This value should not be present if @PlaybackType@ is @LIVE@.
--
-- The values in @DASHimestampRange@ are inclusive. Fragments that start
-- exactly at or after the start time are included in the session.
-- Fragments that start before the start time and continue past it are not
-- included in the session.
--
-- /See:/ 'newDASHTimestampRange' smart constructor.
data DASHTimestampRange = DASHTimestampRange'
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
    endTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The start of the timestamp range for the requested media.
    --
    -- If the @DASHTimestampRange@ value is specified, the @StartTimestamp@
    -- value is required.
    --
    -- Only fragments that start exactly at or after @StartTimestamp@ are
    -- included in the session. Fragments that start before @StartTimestamp@
    -- and continue past it aren\'t included in the session. If
    -- @FragmentSelectorType@ is @SERVER_TIMESTAMP@, the @StartTimestamp@ must
    -- be later than the stream head.
    startTimestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DASHTimestampRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTimestamp', 'dASHTimestampRange_endTimestamp' - The end of the timestamp range for the requested media. This value must
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
-- 'startTimestamp', 'dASHTimestampRange_startTimestamp' - The start of the timestamp range for the requested media.
--
-- If the @DASHTimestampRange@ value is specified, the @StartTimestamp@
-- value is required.
--
-- Only fragments that start exactly at or after @StartTimestamp@ are
-- included in the session. Fragments that start before @StartTimestamp@
-- and continue past it aren\'t included in the session. If
-- @FragmentSelectorType@ is @SERVER_TIMESTAMP@, the @StartTimestamp@ must
-- be later than the stream head.
newDASHTimestampRange ::
  DASHTimestampRange
newDASHTimestampRange =
  DASHTimestampRange'
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
dASHTimestampRange_endTimestamp :: Lens.Lens' DASHTimestampRange (Prelude.Maybe Prelude.UTCTime)
dASHTimestampRange_endTimestamp = Lens.lens (\DASHTimestampRange' {endTimestamp} -> endTimestamp) (\s@DASHTimestampRange' {} a -> s {endTimestamp = a} :: DASHTimestampRange) Prelude.. Lens.mapping Data._Time

-- | The start of the timestamp range for the requested media.
--
-- If the @DASHTimestampRange@ value is specified, the @StartTimestamp@
-- value is required.
--
-- Only fragments that start exactly at or after @StartTimestamp@ are
-- included in the session. Fragments that start before @StartTimestamp@
-- and continue past it aren\'t included in the session. If
-- @FragmentSelectorType@ is @SERVER_TIMESTAMP@, the @StartTimestamp@ must
-- be later than the stream head.
dASHTimestampRange_startTimestamp :: Lens.Lens' DASHTimestampRange (Prelude.Maybe Prelude.UTCTime)
dASHTimestampRange_startTimestamp = Lens.lens (\DASHTimestampRange' {startTimestamp} -> startTimestamp) (\s@DASHTimestampRange' {} a -> s {startTimestamp = a} :: DASHTimestampRange) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable DASHTimestampRange where
  hashWithSalt _salt DASHTimestampRange' {..} =
    _salt
      `Prelude.hashWithSalt` endTimestamp
      `Prelude.hashWithSalt` startTimestamp

instance Prelude.NFData DASHTimestampRange where
  rnf DASHTimestampRange' {..} =
    Prelude.rnf endTimestamp `Prelude.seq`
      Prelude.rnf startTimestamp

instance Data.ToJSON DASHTimestampRange where
  toJSON DASHTimestampRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTimestamp" Data..=) Prelude.<$> endTimestamp,
            ("StartTimestamp" Data..=)
              Prelude.<$> startTimestamp
          ]
      )
