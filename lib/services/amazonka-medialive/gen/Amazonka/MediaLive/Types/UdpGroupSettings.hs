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
-- Module      : Amazonka.MediaLive.Types.UdpGroupSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.UdpGroupSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.InputLossActionForUdpOut
import Amazonka.MediaLive.Types.UdpTimedMetadataId3Frame
import qualified Amazonka.Prelude as Prelude

-- | Udp Group Settings
--
-- /See:/ 'newUdpGroupSettings' smart constructor.
data UdpGroupSettings = UdpGroupSettings'
  { -- | Specifies behavior of last resort when input video is lost, and no more
    -- backup inputs are available. When dropTs is selected the entire
    -- transport stream will stop being emitted. When dropProgram is selected
    -- the program can be dropped from the transport stream (and replaced with
    -- null packets to meet the TS bitrate requirement). Or, when emitProgram
    -- is chosen the transport stream will continue to be produced normally
    -- with repeat frames, black frames, or slate frames substituted for the
    -- absent input video.
    inputLossAction :: Prelude.Maybe InputLossActionForUdpOut,
    -- | Indicates ID3 frame that has the timecode.
    timedMetadataId3Frame :: Prelude.Maybe UdpTimedMetadataId3Frame,
    -- | Timed Metadata interval in seconds.
    timedMetadataId3Period :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UdpGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputLossAction', 'udpGroupSettings_inputLossAction' - Specifies behavior of last resort when input video is lost, and no more
-- backup inputs are available. When dropTs is selected the entire
-- transport stream will stop being emitted. When dropProgram is selected
-- the program can be dropped from the transport stream (and replaced with
-- null packets to meet the TS bitrate requirement). Or, when emitProgram
-- is chosen the transport stream will continue to be produced normally
-- with repeat frames, black frames, or slate frames substituted for the
-- absent input video.
--
-- 'timedMetadataId3Frame', 'udpGroupSettings_timedMetadataId3Frame' - Indicates ID3 frame that has the timecode.
--
-- 'timedMetadataId3Period', 'udpGroupSettings_timedMetadataId3Period' - Timed Metadata interval in seconds.
newUdpGroupSettings ::
  UdpGroupSettings
newUdpGroupSettings =
  UdpGroupSettings'
    { inputLossAction =
        Prelude.Nothing,
      timedMetadataId3Frame = Prelude.Nothing,
      timedMetadataId3Period = Prelude.Nothing
    }

-- | Specifies behavior of last resort when input video is lost, and no more
-- backup inputs are available. When dropTs is selected the entire
-- transport stream will stop being emitted. When dropProgram is selected
-- the program can be dropped from the transport stream (and replaced with
-- null packets to meet the TS bitrate requirement). Or, when emitProgram
-- is chosen the transport stream will continue to be produced normally
-- with repeat frames, black frames, or slate frames substituted for the
-- absent input video.
udpGroupSettings_inputLossAction :: Lens.Lens' UdpGroupSettings (Prelude.Maybe InputLossActionForUdpOut)
udpGroupSettings_inputLossAction = Lens.lens (\UdpGroupSettings' {inputLossAction} -> inputLossAction) (\s@UdpGroupSettings' {} a -> s {inputLossAction = a} :: UdpGroupSettings)

-- | Indicates ID3 frame that has the timecode.
udpGroupSettings_timedMetadataId3Frame :: Lens.Lens' UdpGroupSettings (Prelude.Maybe UdpTimedMetadataId3Frame)
udpGroupSettings_timedMetadataId3Frame = Lens.lens (\UdpGroupSettings' {timedMetadataId3Frame} -> timedMetadataId3Frame) (\s@UdpGroupSettings' {} a -> s {timedMetadataId3Frame = a} :: UdpGroupSettings)

-- | Timed Metadata interval in seconds.
udpGroupSettings_timedMetadataId3Period :: Lens.Lens' UdpGroupSettings (Prelude.Maybe Prelude.Natural)
udpGroupSettings_timedMetadataId3Period = Lens.lens (\UdpGroupSettings' {timedMetadataId3Period} -> timedMetadataId3Period) (\s@UdpGroupSettings' {} a -> s {timedMetadataId3Period = a} :: UdpGroupSettings)

instance Data.FromJSON UdpGroupSettings where
  parseJSON =
    Data.withObject
      "UdpGroupSettings"
      ( \x ->
          UdpGroupSettings'
            Prelude.<$> (x Data..:? "inputLossAction")
            Prelude.<*> (x Data..:? "timedMetadataId3Frame")
            Prelude.<*> (x Data..:? "timedMetadataId3Period")
      )

instance Prelude.Hashable UdpGroupSettings where
  hashWithSalt _salt UdpGroupSettings' {..} =
    _salt
      `Prelude.hashWithSalt` inputLossAction
      `Prelude.hashWithSalt` timedMetadataId3Frame
      `Prelude.hashWithSalt` timedMetadataId3Period

instance Prelude.NFData UdpGroupSettings where
  rnf UdpGroupSettings' {..} =
    Prelude.rnf inputLossAction `Prelude.seq`
      Prelude.rnf timedMetadataId3Frame `Prelude.seq`
        Prelude.rnf timedMetadataId3Period

instance Data.ToJSON UdpGroupSettings where
  toJSON UdpGroupSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("inputLossAction" Data..=)
              Prelude.<$> inputLossAction,
            ("timedMetadataId3Frame" Data..=)
              Prelude.<$> timedMetadataId3Frame,
            ("timedMetadataId3Period" Data..=)
              Prelude.<$> timedMetadataId3Period
          ]
      )
