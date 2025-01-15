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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.UdpOutputSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.FecOutputSettings
import Amazonka.MediaLive.Types.OutputLocationRef
import Amazonka.MediaLive.Types.UdpContainerSettings
import qualified Amazonka.Prelude as Prelude

-- | Udp Output Settings
--
-- /See:/ 'newUdpOutputSettings' smart constructor.
data UdpOutputSettings = UdpOutputSettings'
  { -- | UDP output buffering in milliseconds. Larger values increase latency
    -- through the transcoder but simultaneously assist the transcoder in
    -- maintaining a constant, low-jitter UDP\/RTP output while accommodating
    -- clock recovery, input switching, input disruptions, picture reordering,
    -- etc.
    bufferMsec :: Prelude.Maybe Prelude.Natural,
    -- | Settings for enabling and adjusting Forward Error Correction on UDP
    -- outputs.
    fecOutputSettings :: Prelude.Maybe FecOutputSettings,
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
-- 'bufferMsec', 'udpOutputSettings_bufferMsec' - UDP output buffering in milliseconds. Larger values increase latency
-- through the transcoder but simultaneously assist the transcoder in
-- maintaining a constant, low-jitter UDP\/RTP output while accommodating
-- clock recovery, input switching, input disruptions, picture reordering,
-- etc.
--
-- 'fecOutputSettings', 'udpOutputSettings_fecOutputSettings' - Settings for enabling and adjusting Forward Error Correction on UDP
-- outputs.
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
      { bufferMsec = Prelude.Nothing,
        fecOutputSettings = Prelude.Nothing,
        destination = pDestination_,
        containerSettings = pContainerSettings_
      }

-- | UDP output buffering in milliseconds. Larger values increase latency
-- through the transcoder but simultaneously assist the transcoder in
-- maintaining a constant, low-jitter UDP\/RTP output while accommodating
-- clock recovery, input switching, input disruptions, picture reordering,
-- etc.
udpOutputSettings_bufferMsec :: Lens.Lens' UdpOutputSettings (Prelude.Maybe Prelude.Natural)
udpOutputSettings_bufferMsec = Lens.lens (\UdpOutputSettings' {bufferMsec} -> bufferMsec) (\s@UdpOutputSettings' {} a -> s {bufferMsec = a} :: UdpOutputSettings)

-- | Settings for enabling and adjusting Forward Error Correction on UDP
-- outputs.
udpOutputSettings_fecOutputSettings :: Lens.Lens' UdpOutputSettings (Prelude.Maybe FecOutputSettings)
udpOutputSettings_fecOutputSettings = Lens.lens (\UdpOutputSettings' {fecOutputSettings} -> fecOutputSettings) (\s@UdpOutputSettings' {} a -> s {fecOutputSettings = a} :: UdpOutputSettings)

-- | Destination address and port number for RTP or UDP packets. Can be
-- unicast or multicast RTP or UDP (eg. rtp:\/\/239.10.10.10:5001 or
-- udp:\/\/10.100.100.100:5002).
udpOutputSettings_destination :: Lens.Lens' UdpOutputSettings OutputLocationRef
udpOutputSettings_destination = Lens.lens (\UdpOutputSettings' {destination} -> destination) (\s@UdpOutputSettings' {} a -> s {destination = a} :: UdpOutputSettings)

-- | Undocumented member.
udpOutputSettings_containerSettings :: Lens.Lens' UdpOutputSettings UdpContainerSettings
udpOutputSettings_containerSettings = Lens.lens (\UdpOutputSettings' {containerSettings} -> containerSettings) (\s@UdpOutputSettings' {} a -> s {containerSettings = a} :: UdpOutputSettings)

instance Data.FromJSON UdpOutputSettings where
  parseJSON =
    Data.withObject
      "UdpOutputSettings"
      ( \x ->
          UdpOutputSettings'
            Prelude.<$> (x Data..:? "bufferMsec")
            Prelude.<*> (x Data..:? "fecOutputSettings")
            Prelude.<*> (x Data..: "destination")
            Prelude.<*> (x Data..: "containerSettings")
      )

instance Prelude.Hashable UdpOutputSettings where
  hashWithSalt _salt UdpOutputSettings' {..} =
    _salt
      `Prelude.hashWithSalt` bufferMsec
      `Prelude.hashWithSalt` fecOutputSettings
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` containerSettings

instance Prelude.NFData UdpOutputSettings where
  rnf UdpOutputSettings' {..} =
    Prelude.rnf bufferMsec `Prelude.seq`
      Prelude.rnf fecOutputSettings `Prelude.seq`
        Prelude.rnf destination `Prelude.seq`
          Prelude.rnf containerSettings

instance Data.ToJSON UdpOutputSettings where
  toJSON UdpOutputSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bufferMsec" Data..=) Prelude.<$> bufferMsec,
            ("fecOutputSettings" Data..=)
              Prelude.<$> fecOutputSettings,
            Prelude.Just ("destination" Data..= destination),
            Prelude.Just
              ("containerSettings" Data..= containerSettings)
          ]
      )
