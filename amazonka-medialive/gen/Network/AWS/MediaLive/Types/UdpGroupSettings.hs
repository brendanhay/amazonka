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

-- | Udp Group Settings
--
-- /See:/ 'newUdpGroupSettings' smart constructor.
data UdpGroupSettings = UdpGroupSettings'
  { -- | Timed Metadata interval in seconds.
    timedMetadataId3Period :: Core.Maybe Core.Natural,
    -- | Indicates ID3 frame that has the timecode.
    timedMetadataId3Frame :: Core.Maybe UdpTimedMetadataId3Frame,
    -- | Specifies behavior of last resort when input video is lost, and no more
    -- backup inputs are available. When dropTs is selected the entire
    -- transport stream will stop being emitted. When dropProgram is selected
    -- the program can be dropped from the transport stream (and replaced with
    -- null packets to meet the TS bitrate requirement). Or, when emitProgram
    -- is chosen the transport stream will continue to be produced normally
    -- with repeat frames, black frames, or slate frames substituted for the
    -- absent input video.
    inputLossAction :: Core.Maybe InputLossActionForUdpOut
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      timedMetadataId3Frame = Core.Nothing,
      inputLossAction = Core.Nothing
    }

-- | Timed Metadata interval in seconds.
udpGroupSettings_timedMetadataId3Period :: Lens.Lens' UdpGroupSettings (Core.Maybe Core.Natural)
udpGroupSettings_timedMetadataId3Period = Lens.lens (\UdpGroupSettings' {timedMetadataId3Period} -> timedMetadataId3Period) (\s@UdpGroupSettings' {} a -> s {timedMetadataId3Period = a} :: UdpGroupSettings)

-- | Indicates ID3 frame that has the timecode.
udpGroupSettings_timedMetadataId3Frame :: Lens.Lens' UdpGroupSettings (Core.Maybe UdpTimedMetadataId3Frame)
udpGroupSettings_timedMetadataId3Frame = Lens.lens (\UdpGroupSettings' {timedMetadataId3Frame} -> timedMetadataId3Frame) (\s@UdpGroupSettings' {} a -> s {timedMetadataId3Frame = a} :: UdpGroupSettings)

-- | Specifies behavior of last resort when input video is lost, and no more
-- backup inputs are available. When dropTs is selected the entire
-- transport stream will stop being emitted. When dropProgram is selected
-- the program can be dropped from the transport stream (and replaced with
-- null packets to meet the TS bitrate requirement). Or, when emitProgram
-- is chosen the transport stream will continue to be produced normally
-- with repeat frames, black frames, or slate frames substituted for the
-- absent input video.
udpGroupSettings_inputLossAction :: Lens.Lens' UdpGroupSettings (Core.Maybe InputLossActionForUdpOut)
udpGroupSettings_inputLossAction = Lens.lens (\UdpGroupSettings' {inputLossAction} -> inputLossAction) (\s@UdpGroupSettings' {} a -> s {inputLossAction = a} :: UdpGroupSettings)

instance Core.FromJSON UdpGroupSettings where
  parseJSON =
    Core.withObject
      "UdpGroupSettings"
      ( \x ->
          UdpGroupSettings'
            Core.<$> (x Core..:? "timedMetadataId3Period")
            Core.<*> (x Core..:? "timedMetadataId3Frame")
            Core.<*> (x Core..:? "inputLossAction")
      )

instance Core.Hashable UdpGroupSettings

instance Core.NFData UdpGroupSettings

instance Core.ToJSON UdpGroupSettings where
  toJSON UdpGroupSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("timedMetadataId3Period" Core..=)
              Core.<$> timedMetadataId3Period,
            ("timedMetadataId3Frame" Core..=)
              Core.<$> timedMetadataId3Frame,
            ("inputLossAction" Core..=)
              Core.<$> inputLossAction
          ]
      )
