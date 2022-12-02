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
-- Module      : Amazonka.MediaLive.Types.MultiplexSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MultiplexSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains configuration for a Multiplex event
--
-- /See:/ 'newMultiplexSettings' smart constructor.
data MultiplexSettings = MultiplexSettings'
  { -- | Transport stream reserved bit rate.
    transportStreamReservedBitrate :: Prelude.Maybe Prelude.Natural,
    -- | Maximum video buffer delay in milliseconds.
    maximumVideoBufferDelayMilliseconds :: Prelude.Maybe Prelude.Natural,
    -- | Transport stream bit rate.
    transportStreamBitrate :: Prelude.Natural,
    -- | Transport stream ID.
    transportStreamId :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiplexSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transportStreamReservedBitrate', 'multiplexSettings_transportStreamReservedBitrate' - Transport stream reserved bit rate.
--
-- 'maximumVideoBufferDelayMilliseconds', 'multiplexSettings_maximumVideoBufferDelayMilliseconds' - Maximum video buffer delay in milliseconds.
--
-- 'transportStreamBitrate', 'multiplexSettings_transportStreamBitrate' - Transport stream bit rate.
--
-- 'transportStreamId', 'multiplexSettings_transportStreamId' - Transport stream ID.
newMultiplexSettings ::
  -- | 'transportStreamBitrate'
  Prelude.Natural ->
  -- | 'transportStreamId'
  Prelude.Natural ->
  MultiplexSettings
newMultiplexSettings
  pTransportStreamBitrate_
  pTransportStreamId_ =
    MultiplexSettings'
      { transportStreamReservedBitrate =
          Prelude.Nothing,
        maximumVideoBufferDelayMilliseconds =
          Prelude.Nothing,
        transportStreamBitrate = pTransportStreamBitrate_,
        transportStreamId = pTransportStreamId_
      }

-- | Transport stream reserved bit rate.
multiplexSettings_transportStreamReservedBitrate :: Lens.Lens' MultiplexSettings (Prelude.Maybe Prelude.Natural)
multiplexSettings_transportStreamReservedBitrate = Lens.lens (\MultiplexSettings' {transportStreamReservedBitrate} -> transportStreamReservedBitrate) (\s@MultiplexSettings' {} a -> s {transportStreamReservedBitrate = a} :: MultiplexSettings)

-- | Maximum video buffer delay in milliseconds.
multiplexSettings_maximumVideoBufferDelayMilliseconds :: Lens.Lens' MultiplexSettings (Prelude.Maybe Prelude.Natural)
multiplexSettings_maximumVideoBufferDelayMilliseconds = Lens.lens (\MultiplexSettings' {maximumVideoBufferDelayMilliseconds} -> maximumVideoBufferDelayMilliseconds) (\s@MultiplexSettings' {} a -> s {maximumVideoBufferDelayMilliseconds = a} :: MultiplexSettings)

-- | Transport stream bit rate.
multiplexSettings_transportStreamBitrate :: Lens.Lens' MultiplexSettings Prelude.Natural
multiplexSettings_transportStreamBitrate = Lens.lens (\MultiplexSettings' {transportStreamBitrate} -> transportStreamBitrate) (\s@MultiplexSettings' {} a -> s {transportStreamBitrate = a} :: MultiplexSettings)

-- | Transport stream ID.
multiplexSettings_transportStreamId :: Lens.Lens' MultiplexSettings Prelude.Natural
multiplexSettings_transportStreamId = Lens.lens (\MultiplexSettings' {transportStreamId} -> transportStreamId) (\s@MultiplexSettings' {} a -> s {transportStreamId = a} :: MultiplexSettings)

instance Data.FromJSON MultiplexSettings where
  parseJSON =
    Data.withObject
      "MultiplexSettings"
      ( \x ->
          MultiplexSettings'
            Prelude.<$> (x Data..:? "transportStreamReservedBitrate")
            Prelude.<*> (x Data..:? "maximumVideoBufferDelayMilliseconds")
            Prelude.<*> (x Data..: "transportStreamBitrate")
            Prelude.<*> (x Data..: "transportStreamId")
      )

instance Prelude.Hashable MultiplexSettings where
  hashWithSalt _salt MultiplexSettings' {..} =
    _salt
      `Prelude.hashWithSalt` transportStreamReservedBitrate
      `Prelude.hashWithSalt` maximumVideoBufferDelayMilliseconds
      `Prelude.hashWithSalt` transportStreamBitrate
      `Prelude.hashWithSalt` transportStreamId

instance Prelude.NFData MultiplexSettings where
  rnf MultiplexSettings' {..} =
    Prelude.rnf transportStreamReservedBitrate
      `Prelude.seq` Prelude.rnf maximumVideoBufferDelayMilliseconds
      `Prelude.seq` Prelude.rnf transportStreamBitrate
      `Prelude.seq` Prelude.rnf transportStreamId

instance Data.ToJSON MultiplexSettings where
  toJSON MultiplexSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("transportStreamReservedBitrate" Data..=)
              Prelude.<$> transportStreamReservedBitrate,
            ("maximumVideoBufferDelayMilliseconds" Data..=)
              Prelude.<$> maximumVideoBufferDelayMilliseconds,
            Prelude.Just
              ( "transportStreamBitrate"
                  Data..= transportStreamBitrate
              ),
            Prelude.Just
              ("transportStreamId" Data..= transportStreamId)
          ]
      )
