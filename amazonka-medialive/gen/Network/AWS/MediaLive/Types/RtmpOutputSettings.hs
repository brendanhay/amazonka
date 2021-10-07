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
-- Module      : Network.AWS.MediaLive.Types.RtmpOutputSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpOutputSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.OutputLocationRef
import Network.AWS.MediaLive.Types.RtmpOutputCertificateMode
import qualified Network.AWS.Prelude as Prelude

-- | Rtmp Output Settings
--
-- /See:/ 'newRtmpOutputSettings' smart constructor.
data RtmpOutputSettings = RtmpOutputSettings'
  { -- | If set to verifyAuthenticity, verify the tls certificate chain to a
    -- trusted Certificate Authority (CA). This will cause rtmps outputs with
    -- self-signed certificates to fail.
    certificateMode :: Prelude.Maybe RtmpOutputCertificateMode,
    -- | Number of retry attempts.
    numRetries :: Prelude.Maybe Prelude.Natural,
    -- | Number of seconds to wait before retrying a connection to the Flash
    -- Media server if the connection is lost.
    connectionRetryInterval :: Prelude.Maybe Prelude.Natural,
    -- | The RTMP endpoint excluding the stream name (eg.
    -- rtmp:\/\/host\/appname). For connection to Akamai, a username and
    -- password must be supplied. URI fields accept format identifiers.
    destination :: OutputLocationRef
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RtmpOutputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateMode', 'rtmpOutputSettings_certificateMode' - If set to verifyAuthenticity, verify the tls certificate chain to a
-- trusted Certificate Authority (CA). This will cause rtmps outputs with
-- self-signed certificates to fail.
--
-- 'numRetries', 'rtmpOutputSettings_numRetries' - Number of retry attempts.
--
-- 'connectionRetryInterval', 'rtmpOutputSettings_connectionRetryInterval' - Number of seconds to wait before retrying a connection to the Flash
-- Media server if the connection is lost.
--
-- 'destination', 'rtmpOutputSettings_destination' - The RTMP endpoint excluding the stream name (eg.
-- rtmp:\/\/host\/appname). For connection to Akamai, a username and
-- password must be supplied. URI fields accept format identifiers.
newRtmpOutputSettings ::
  -- | 'destination'
  OutputLocationRef ->
  RtmpOutputSettings
newRtmpOutputSettings pDestination_ =
  RtmpOutputSettings'
    { certificateMode =
        Prelude.Nothing,
      numRetries = Prelude.Nothing,
      connectionRetryInterval = Prelude.Nothing,
      destination = pDestination_
    }

-- | If set to verifyAuthenticity, verify the tls certificate chain to a
-- trusted Certificate Authority (CA). This will cause rtmps outputs with
-- self-signed certificates to fail.
rtmpOutputSettings_certificateMode :: Lens.Lens' RtmpOutputSettings (Prelude.Maybe RtmpOutputCertificateMode)
rtmpOutputSettings_certificateMode = Lens.lens (\RtmpOutputSettings' {certificateMode} -> certificateMode) (\s@RtmpOutputSettings' {} a -> s {certificateMode = a} :: RtmpOutputSettings)

-- | Number of retry attempts.
rtmpOutputSettings_numRetries :: Lens.Lens' RtmpOutputSettings (Prelude.Maybe Prelude.Natural)
rtmpOutputSettings_numRetries = Lens.lens (\RtmpOutputSettings' {numRetries} -> numRetries) (\s@RtmpOutputSettings' {} a -> s {numRetries = a} :: RtmpOutputSettings)

-- | Number of seconds to wait before retrying a connection to the Flash
-- Media server if the connection is lost.
rtmpOutputSettings_connectionRetryInterval :: Lens.Lens' RtmpOutputSettings (Prelude.Maybe Prelude.Natural)
rtmpOutputSettings_connectionRetryInterval = Lens.lens (\RtmpOutputSettings' {connectionRetryInterval} -> connectionRetryInterval) (\s@RtmpOutputSettings' {} a -> s {connectionRetryInterval = a} :: RtmpOutputSettings)

-- | The RTMP endpoint excluding the stream name (eg.
-- rtmp:\/\/host\/appname). For connection to Akamai, a username and
-- password must be supplied. URI fields accept format identifiers.
rtmpOutputSettings_destination :: Lens.Lens' RtmpOutputSettings OutputLocationRef
rtmpOutputSettings_destination = Lens.lens (\RtmpOutputSettings' {destination} -> destination) (\s@RtmpOutputSettings' {} a -> s {destination = a} :: RtmpOutputSettings)

instance Core.FromJSON RtmpOutputSettings where
  parseJSON =
    Core.withObject
      "RtmpOutputSettings"
      ( \x ->
          RtmpOutputSettings'
            Prelude.<$> (x Core..:? "certificateMode")
            Prelude.<*> (x Core..:? "numRetries")
            Prelude.<*> (x Core..:? "connectionRetryInterval")
            Prelude.<*> (x Core..: "destination")
      )

instance Prelude.Hashable RtmpOutputSettings

instance Prelude.NFData RtmpOutputSettings

instance Core.ToJSON RtmpOutputSettings where
  toJSON RtmpOutputSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("certificateMode" Core..=)
              Prelude.<$> certificateMode,
            ("numRetries" Core..=) Prelude.<$> numRetries,
            ("connectionRetryInterval" Core..=)
              Prelude.<$> connectionRetryInterval,
            Prelude.Just ("destination" Core..= destination)
          ]
      )
