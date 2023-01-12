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
-- Module      : Amazonka.MediaLive.Types.NetworkInputSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.NetworkInputSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.HlsInputSettings
import Amazonka.MediaLive.Types.NetworkInputServerValidation
import qualified Amazonka.Prelude as Prelude

-- | Network source to transcode. Must be accessible to the Elemental Live
-- node that is running the live event through a network connection.
--
-- /See:/ 'newNetworkInputSettings' smart constructor.
data NetworkInputSettings = NetworkInputSettings'
  { -- | Specifies HLS input settings when the uri is for a HLS manifest.
    hlsInputSettings :: Prelude.Maybe HlsInputSettings,
    -- | Check HTTPS server certificates. When set to checkCryptographyOnly,
    -- cryptography in the certificate will be checked, but not the server\'s
    -- name. Certain subdomains (notably S3 buckets that use dots in the bucket
    -- name) do not strictly match the corresponding certificate\'s wildcard
    -- pattern and would otherwise cause the event to error. This setting is
    -- ignored for protocols that do not use https.
    serverValidation :: Prelude.Maybe NetworkInputServerValidation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hlsInputSettings', 'networkInputSettings_hlsInputSettings' - Specifies HLS input settings when the uri is for a HLS manifest.
--
-- 'serverValidation', 'networkInputSettings_serverValidation' - Check HTTPS server certificates. When set to checkCryptographyOnly,
-- cryptography in the certificate will be checked, but not the server\'s
-- name. Certain subdomains (notably S3 buckets that use dots in the bucket
-- name) do not strictly match the corresponding certificate\'s wildcard
-- pattern and would otherwise cause the event to error. This setting is
-- ignored for protocols that do not use https.
newNetworkInputSettings ::
  NetworkInputSettings
newNetworkInputSettings =
  NetworkInputSettings'
    { hlsInputSettings =
        Prelude.Nothing,
      serverValidation = Prelude.Nothing
    }

-- | Specifies HLS input settings when the uri is for a HLS manifest.
networkInputSettings_hlsInputSettings :: Lens.Lens' NetworkInputSettings (Prelude.Maybe HlsInputSettings)
networkInputSettings_hlsInputSettings = Lens.lens (\NetworkInputSettings' {hlsInputSettings} -> hlsInputSettings) (\s@NetworkInputSettings' {} a -> s {hlsInputSettings = a} :: NetworkInputSettings)

-- | Check HTTPS server certificates. When set to checkCryptographyOnly,
-- cryptography in the certificate will be checked, but not the server\'s
-- name. Certain subdomains (notably S3 buckets that use dots in the bucket
-- name) do not strictly match the corresponding certificate\'s wildcard
-- pattern and would otherwise cause the event to error. This setting is
-- ignored for protocols that do not use https.
networkInputSettings_serverValidation :: Lens.Lens' NetworkInputSettings (Prelude.Maybe NetworkInputServerValidation)
networkInputSettings_serverValidation = Lens.lens (\NetworkInputSettings' {serverValidation} -> serverValidation) (\s@NetworkInputSettings' {} a -> s {serverValidation = a} :: NetworkInputSettings)

instance Data.FromJSON NetworkInputSettings where
  parseJSON =
    Data.withObject
      "NetworkInputSettings"
      ( \x ->
          NetworkInputSettings'
            Prelude.<$> (x Data..:? "hlsInputSettings")
            Prelude.<*> (x Data..:? "serverValidation")
      )

instance Prelude.Hashable NetworkInputSettings where
  hashWithSalt _salt NetworkInputSettings' {..} =
    _salt `Prelude.hashWithSalt` hlsInputSettings
      `Prelude.hashWithSalt` serverValidation

instance Prelude.NFData NetworkInputSettings where
  rnf NetworkInputSettings' {..} =
    Prelude.rnf hlsInputSettings
      `Prelude.seq` Prelude.rnf serverValidation

instance Data.ToJSON NetworkInputSettings where
  toJSON NetworkInputSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("hlsInputSettings" Data..=)
              Prelude.<$> hlsInputSettings,
            ("serverValidation" Data..=)
              Prelude.<$> serverValidation
          ]
      )
