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
-- Module      : Network.AWS.MediaLive.Types.UdpGroupSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.UdpGroupSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputLossActionForUdpOut
import Network.AWS.MediaLive.Types.UdpTimedMetadataId3Frame
import qualified Network.AWS.Prelude as Prelude

-- | Udp Group Settings
--
-- /See:/ 'newUdpGroupSettings' smart constructor.
data UdpGroupSettings = UdpGroupSettings'
  { -- | Timed Metadata interval in seconds.
    timedMetadataId3Period :: Prelude.Maybe Prelude.Natural,
    -- | Indicates ID3 frame that has the timecode.
    timedMetadataId3Frame :: Prelude.Maybe UdpTimedMetadataId3Frame,
    -- | Specifies behavior of last resort when input video is lost, and no more
    -- backup inputs are available. When dropTs is selected the entire
    -- transport stream will stop being emitted. When dropProgram is selected
    -- the program can be dropped from the transport stream (and replaced with
    -- null packets to meet the TS bitrate requirement). Or, when emitProgram
    -- is chosen the transport stream will continue to be produced normally
    -- with repeat frames, black frames, or slate frames substituted for the
    -- absent input video.
    inputLossAction :: Prelude.Maybe InputLossActionForUdpOut
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
-- 'timedMetadataId3Period', 'udpGroupSettings_timedMetadataId3Period' - Timed Metadata interval in seconds.
--
-- 'timedMetadataId3Frame', 'udpGroupSettings_timedMetadataId3Frame' - Indicates ID3 frame that has the timecode.
--
-- 'inputLossAction', 'udpGroupSettings_inputLossAction' - Specifies behavior of last resort when input video is lost, and no more
-- backup inputs are available. When dropTs is selected the entire
-- transport stream will stop being emitted. When dropProgram is selected
-- the program can be dropped from the transport stream (and replaced with
-- null packets to meet the TS bitrate requirement). Or, when emitProgram
-- is chosen the transport stream will continue to be produced normally
-- with repeat frames, black frames, or slate frames substituted for the
-- absent input video.
newUdpGroupSettings ::
  UdpGroupSettings
newUdpGroupSettings =
  UdpGroupSettings'
    { timedMetadataId3Period =
        Prelude.Nothing,
      timedMetadataId3Frame = Prelude.Nothing,
      inputLossAction = Prelude.Nothing
    }

-- | Timed Metadata interval in seconds.
udpGroupSettings_timedMetadataId3Period :: Lens.Lens' UdpGroupSettings (Prelude.Maybe Prelude.Natural)
udpGroupSettings_timedMetadataId3Period = Lens.lens (\UdpGroupSettings' {timedMetadataId3Period} -> timedMetadataId3Period) (\s@UdpGroupSettings' {} a -> s {timedMetadataId3Period = a} :: UdpGroupSettings)

-- | Indicates ID3 frame that has the timecode.
udpGroupSettings_timedMetadataId3Frame :: Lens.Lens' UdpGroupSettings (Prelude.Maybe UdpTimedMetadataId3Frame)
udpGroupSettings_timedMetadataId3Frame = Lens.lens (\UdpGroupSettings' {timedMetadataId3Frame} -> timedMetadataId3Frame) (\s@UdpGroupSettings' {} a -> s {timedMetadataId3Frame = a} :: UdpGroupSettings)

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

instance Core.FromJSON UdpGroupSettings where
  parseJSON =
    Core.withObject
      "UdpGroupSettings"
      ( \x ->
          UdpGroupSettings'
            Prelude.<$> (x Core..:? "timedMetadataId3Period")
            Prelude.<*> (x Core..:? "timedMetadataId3Frame")
            Prelude.<*> (x Core..:? "inputLossAction")
      )

instance Prelude.Hashable UdpGroupSettings

instance Prelude.NFData UdpGroupSettings

instance Core.ToJSON UdpGroupSettings where
  toJSON UdpGroupSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("timedMetadataId3Period" Core..=)
              Prelude.<$> timedMetadataId3Period,
            ("timedMetadataId3Frame" Core..=)
              Prelude.<$> timedMetadataId3Frame,
            ("inputLossAction" Core..=)
              Prelude.<$> inputLossAction
          ]
      )
