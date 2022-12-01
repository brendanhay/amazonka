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
-- Module      : Amazonka.MediaLive.Types.UdpOutputSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.UdpOutputSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.FecOutputSettings
import Amazonka.MediaLive.Types.OutputLocationRef
import Amazonka.MediaLive.Types.UdpContainerSettings
import qualified Amazonka.Prelude as Prelude

-- | Udp Output Settings
--
-- /See:/ 'newUdpOutputSettings' smart constructor.
data UdpOutputSettings = UdpOutputSettings'
  { -- | Settings for enabling and adjusting Forward Error Correction on UDP
    -- outputs.
    fecOutputSettings :: Prelude.Maybe FecOutputSettings,
    -- | UDP output buffering in milliseconds. Larger values increase latency
    -- through the transcoder but simultaneously assist the transcoder in
    -- maintaining a constant, low-jitter UDP\/RTP output while accommodating
    -- clock recovery, input switching, input disruptions, picture reordering,
    -- etc.
    bufferMsec :: Prelude.Maybe Prelude.Natural,
    -- | Destination address and port number for RTP or UDP packets. Can be
    -- unicast or multicast RTP or UDP (eg. rtp:\/\/239.10.10.10:5001 or
    -- udp:\/\/10.100.100.100:5002).
    destination :: OutputLocationRef,
    containerSettings :: UdpContainerSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UdpOutputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fecOutputSettings', 'udpOutputSettings_fecOutputSettings' - Settings for enabling and adjusting Forward Error Correction on UDP
-- outputs.
--
-- 'bufferMsec', 'udpOutputSettings_bufferMsec' - UDP output buffering in milliseconds. Larger values increase latency
-- through the transcoder but simultaneously assist the transcoder in
-- maintaining a constant, low-jitter UDP\/RTP output while accommodating
-- clock recovery, input switching, input disruptions, picture reordering,
-- etc.
--
-- 'destination', 'udpOutputSettings_destination' - Destination address and port number for RTP or UDP packets. Can be
-- unicast or multicast RTP or UDP (eg. rtp:\/\/239.10.10.10:5001 or
-- udp:\/\/10.100.100.100:5002).
--
-- 'containerSettings', 'udpOutputSettings_containerSettings' - Undocumented member.
newUdpOutputSettings ::
  -- | 'destination'
  OutputLocationRef ->
  -- | 'containerSettings'
  UdpContainerSettings ->
  UdpOutputSettings
newUdpOutputSettings
  pDestination_
  pContainerSettings_ =
    UdpOutputSettings'
      { fecOutputSettings =
          Prelude.Nothing,
        bufferMsec = Prelude.Nothing,
        destination = pDestination_,
        containerSettings = pContainerSettings_
      }

-- | Settings for enabling and adjusting Forward Error Correction on UDP
-- outputs.
udpOutputSettings_fecOutputSettings :: Lens.Lens' UdpOutputSettings (Prelude.Maybe FecOutputSettings)
udpOutputSettings_fecOutputSettings = Lens.lens (\UdpOutputSettings' {fecOutputSettings} -> fecOutputSettings) (\s@UdpOutputSettings' {} a -> s {fecOutputSettings = a} :: UdpOutputSettings)

-- | UDP output buffering in milliseconds. Larger values increase latency
-- through the transcoder but simultaneously assist the transcoder in
-- maintaining a constant, low-jitter UDP\/RTP output while accommodating
-- clock recovery, input switching, input disruptions, picture reordering,
-- etc.
udpOutputSettings_bufferMsec :: Lens.Lens' UdpOutputSettings (Prelude.Maybe Prelude.Natural)
udpOutputSettings_bufferMsec = Lens.lens (\UdpOutputSettings' {bufferMsec} -> bufferMsec) (\s@UdpOutputSettings' {} a -> s {bufferMsec = a} :: UdpOutputSettings)

-- | Destination address and port number for RTP or UDP packets. Can be
-- unicast or multicast RTP or UDP (eg. rtp:\/\/239.10.10.10:5001 or
-- udp:\/\/10.100.100.100:5002).
udpOutputSettings_destination :: Lens.Lens' UdpOutputSettings OutputLocationRef
udpOutputSettings_destination = Lens.lens (\UdpOutputSettings' {destination} -> destination) (\s@UdpOutputSettings' {} a -> s {destination = a} :: UdpOutputSettings)

-- | Undocumented member.
udpOutputSettings_containerSettings :: Lens.Lens' UdpOutputSettings UdpContainerSettings
udpOutputSettings_containerSettings = Lens.lens (\UdpOutputSettings' {containerSettings} -> containerSettings) (\s@UdpOutputSettings' {} a -> s {containerSettings = a} :: UdpOutputSettings)

instance Core.FromJSON UdpOutputSettings where
  parseJSON =
    Core.withObject
      "UdpOutputSettings"
      ( \x ->
          UdpOutputSettings'
            Prelude.<$> (x Core..:? "fecOutputSettings")
            Prelude.<*> (x Core..:? "bufferMsec")
            Prelude.<*> (x Core..: "destination")
            Prelude.<*> (x Core..: "containerSettings")
      )

instance Prelude.Hashable UdpOutputSettings where
  hashWithSalt _salt UdpOutputSettings' {..} =
    _salt `Prelude.hashWithSalt` fecOutputSettings
      `Prelude.hashWithSalt` bufferMsec
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` containerSettings

instance Prelude.NFData UdpOutputSettings where
  rnf UdpOutputSettings' {..} =
    Prelude.rnf fecOutputSettings
      `Prelude.seq` Prelude.rnf bufferMsec
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf containerSettings

instance Core.ToJSON UdpOutputSettings where
  toJSON UdpOutputSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("fecOutputSettings" Core..=)
              Prelude.<$> fecOutputSettings,
            ("bufferMsec" Core..=) Prelude.<$> bufferMsec,
            Prelude.Just ("destination" Core..= destination),
            Prelude.Just
              ("containerSettings" Core..= containerSettings)
          ]
      )
