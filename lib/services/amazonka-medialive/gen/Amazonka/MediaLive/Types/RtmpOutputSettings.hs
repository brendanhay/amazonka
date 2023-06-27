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
-- Module      : Amazonka.MediaLive.Types.RtmpOutputSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.RtmpOutputSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.OutputLocationRef
import Amazonka.MediaLive.Types.RtmpOutputCertificateMode
import qualified Amazonka.Prelude as Prelude

-- | Rtmp Output Settings
--
-- /See:/ 'newRtmpOutputSettings' smart constructor.
data RtmpOutputSettings = RtmpOutputSettings'
  { -- | If set to verifyAuthenticity, verify the tls certificate chain to a
    -- trusted Certificate Authority (CA). This will cause rtmps outputs with
    -- self-signed certificates to fail.
    certificateMode :: Prelude.Maybe RtmpOutputCertificateMode,
    -- | Number of seconds to wait before retrying a connection to the Flash
    -- Media server if the connection is lost.
    connectionRetryInterval :: Prelude.Maybe Prelude.Natural,
    -- | Number of retry attempts.
    numRetries :: Prelude.Maybe Prelude.Natural,
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
-- 'connectionRetryInterval', 'rtmpOutputSettings_connectionRetryInterval' - Number of seconds to wait before retrying a connection to the Flash
-- Media server if the connection is lost.
--
-- 'numRetries', 'rtmpOutputSettings_numRetries' - Number of retry attempts.
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
      connectionRetryInterval = Prelude.Nothing,
      numRetries = Prelude.Nothing,
      destination = pDestination_
    }

-- | If set to verifyAuthenticity, verify the tls certificate chain to a
-- trusted Certificate Authority (CA). This will cause rtmps outputs with
-- self-signed certificates to fail.
rtmpOutputSettings_certificateMode :: Lens.Lens' RtmpOutputSettings (Prelude.Maybe RtmpOutputCertificateMode)
rtmpOutputSettings_certificateMode = Lens.lens (\RtmpOutputSettings' {certificateMode} -> certificateMode) (\s@RtmpOutputSettings' {} a -> s {certificateMode = a} :: RtmpOutputSettings)

-- | Number of seconds to wait before retrying a connection to the Flash
-- Media server if the connection is lost.
rtmpOutputSettings_connectionRetryInterval :: Lens.Lens' RtmpOutputSettings (Prelude.Maybe Prelude.Natural)
rtmpOutputSettings_connectionRetryInterval = Lens.lens (\RtmpOutputSettings' {connectionRetryInterval} -> connectionRetryInterval) (\s@RtmpOutputSettings' {} a -> s {connectionRetryInterval = a} :: RtmpOutputSettings)

-- | Number of retry attempts.
rtmpOutputSettings_numRetries :: Lens.Lens' RtmpOutputSettings (Prelude.Maybe Prelude.Natural)
rtmpOutputSettings_numRetries = Lens.lens (\RtmpOutputSettings' {numRetries} -> numRetries) (\s@RtmpOutputSettings' {} a -> s {numRetries = a} :: RtmpOutputSettings)

-- | The RTMP endpoint excluding the stream name (eg.
-- rtmp:\/\/host\/appname). For connection to Akamai, a username and
-- password must be supplied. URI fields accept format identifiers.
rtmpOutputSettings_destination :: Lens.Lens' RtmpOutputSettings OutputLocationRef
rtmpOutputSettings_destination = Lens.lens (\RtmpOutputSettings' {destination} -> destination) (\s@RtmpOutputSettings' {} a -> s {destination = a} :: RtmpOutputSettings)

instance Data.FromJSON RtmpOutputSettings where
  parseJSON =
    Data.withObject
      "RtmpOutputSettings"
      ( \x ->
          RtmpOutputSettings'
            Prelude.<$> (x Data..:? "certificateMode")
            Prelude.<*> (x Data..:? "connectionRetryInterval")
            Prelude.<*> (x Data..:? "numRetries")
            Prelude.<*> (x Data..: "destination")
      )

instance Prelude.Hashable RtmpOutputSettings where
  hashWithSalt _salt RtmpOutputSettings' {..} =
    _salt
      `Prelude.hashWithSalt` certificateMode
      `Prelude.hashWithSalt` connectionRetryInterval
      `Prelude.hashWithSalt` numRetries
      `Prelude.hashWithSalt` destination

instance Prelude.NFData RtmpOutputSettings where
  rnf RtmpOutputSettings' {..} =
    Prelude.rnf certificateMode
      `Prelude.seq` Prelude.rnf connectionRetryInterval
      `Prelude.seq` Prelude.rnf numRetries
      `Prelude.seq` Prelude.rnf destination

instance Data.ToJSON RtmpOutputSettings where
  toJSON RtmpOutputSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("certificateMode" Data..=)
              Prelude.<$> certificateMode,
            ("connectionRetryInterval" Data..=)
              Prelude.<$> connectionRetryInterval,
            ("numRetries" Data..=) Prelude.<$> numRetries,
            Prelude.Just ("destination" Data..= destination)
          ]
      )
