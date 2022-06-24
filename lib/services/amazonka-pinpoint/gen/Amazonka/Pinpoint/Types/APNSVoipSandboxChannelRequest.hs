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
-- Module      : Amazonka.Pinpoint.Types.APNSVoipSandboxChannelRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.APNSVoipSandboxChannelRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the status and settings of the APNs (Apple Push Notification
-- service) VoIP sandbox channel for an application.
--
-- /See:/ 'newAPNSVoipSandboxChannelRequest' smart constructor.
data APNSVoipSandboxChannelRequest = APNSVoipSandboxChannelRequest'
  { -- | The private key for the APNs client certificate that you want Amazon
    -- Pinpoint to use to communicate with the APNs sandbox environment.
    privateKey :: Prelude.Maybe Prelude.Text,
    -- | The identifier that\'s assigned to your Apple developer account team.
    -- This identifier is used for APNs tokens.
    teamId :: Prelude.Maybe Prelude.Text,
    -- | The key identifier that\'s assigned to your APNs signing key, if you
    -- want Amazon Pinpoint to communicate with the APNs sandbox environment by
    -- using APNs tokens.
    tokenKeyId :: Prelude.Maybe Prelude.Text,
    -- | The APNs client certificate that you received from Apple, if you want
    -- Amazon Pinpoint to communicate with the APNs sandbox environment by
    -- using an APNs certificate.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the APNs VoIP sandbox channel is enabled for the
    -- application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The authentication key to use for APNs tokens.
    tokenKey :: Prelude.Maybe Prelude.Text,
    -- | The bundle identifier that\'s assigned to your iOS app. This identifier
    -- is used for APNs tokens.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The default authentication method that you want Amazon Pinpoint to use
    -- when authenticating with the APNs sandbox environment for this channel,
    -- key or certificate.
    defaultAuthenticationMethod :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'APNSVoipSandboxChannelRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateKey', 'aPNSVoipSandboxChannelRequest_privateKey' - The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with the APNs sandbox environment.
--
-- 'teamId', 'aPNSVoipSandboxChannelRequest_teamId' - The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
--
-- 'tokenKeyId', 'aPNSVoipSandboxChannelRequest_tokenKeyId' - The key identifier that\'s assigned to your APNs signing key, if you
-- want Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using APNs tokens.
--
-- 'certificate', 'aPNSVoipSandboxChannelRequest_certificate' - The APNs client certificate that you received from Apple, if you want
-- Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using an APNs certificate.
--
-- 'enabled', 'aPNSVoipSandboxChannelRequest_enabled' - Specifies whether the APNs VoIP sandbox channel is enabled for the
-- application.
--
-- 'tokenKey', 'aPNSVoipSandboxChannelRequest_tokenKey' - The authentication key to use for APNs tokens.
--
-- 'bundleId', 'aPNSVoipSandboxChannelRequest_bundleId' - The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
--
-- 'defaultAuthenticationMethod', 'aPNSVoipSandboxChannelRequest_defaultAuthenticationMethod' - The default authentication method that you want Amazon Pinpoint to use
-- when authenticating with the APNs sandbox environment for this channel,
-- key or certificate.
newAPNSVoipSandboxChannelRequest ::
  APNSVoipSandboxChannelRequest
newAPNSVoipSandboxChannelRequest =
  APNSVoipSandboxChannelRequest'
    { privateKey =
        Prelude.Nothing,
      teamId = Prelude.Nothing,
      tokenKeyId = Prelude.Nothing,
      certificate = Prelude.Nothing,
      enabled = Prelude.Nothing,
      tokenKey = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      defaultAuthenticationMethod =
        Prelude.Nothing
    }

-- | The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with the APNs sandbox environment.
aPNSVoipSandboxChannelRequest_privateKey :: Lens.Lens' APNSVoipSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelRequest_privateKey = Lens.lens (\APNSVoipSandboxChannelRequest' {privateKey} -> privateKey) (\s@APNSVoipSandboxChannelRequest' {} a -> s {privateKey = a} :: APNSVoipSandboxChannelRequest)

-- | The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
aPNSVoipSandboxChannelRequest_teamId :: Lens.Lens' APNSVoipSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelRequest_teamId = Lens.lens (\APNSVoipSandboxChannelRequest' {teamId} -> teamId) (\s@APNSVoipSandboxChannelRequest' {} a -> s {teamId = a} :: APNSVoipSandboxChannelRequest)

-- | The key identifier that\'s assigned to your APNs signing key, if you
-- want Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using APNs tokens.
aPNSVoipSandboxChannelRequest_tokenKeyId :: Lens.Lens' APNSVoipSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelRequest_tokenKeyId = Lens.lens (\APNSVoipSandboxChannelRequest' {tokenKeyId} -> tokenKeyId) (\s@APNSVoipSandboxChannelRequest' {} a -> s {tokenKeyId = a} :: APNSVoipSandboxChannelRequest)

-- | The APNs client certificate that you received from Apple, if you want
-- Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using an APNs certificate.
aPNSVoipSandboxChannelRequest_certificate :: Lens.Lens' APNSVoipSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelRequest_certificate = Lens.lens (\APNSVoipSandboxChannelRequest' {certificate} -> certificate) (\s@APNSVoipSandboxChannelRequest' {} a -> s {certificate = a} :: APNSVoipSandboxChannelRequest)

-- | Specifies whether the APNs VoIP sandbox channel is enabled for the
-- application.
aPNSVoipSandboxChannelRequest_enabled :: Lens.Lens' APNSVoipSandboxChannelRequest (Prelude.Maybe Prelude.Bool)
aPNSVoipSandboxChannelRequest_enabled = Lens.lens (\APNSVoipSandboxChannelRequest' {enabled} -> enabled) (\s@APNSVoipSandboxChannelRequest' {} a -> s {enabled = a} :: APNSVoipSandboxChannelRequest)

-- | The authentication key to use for APNs tokens.
aPNSVoipSandboxChannelRequest_tokenKey :: Lens.Lens' APNSVoipSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelRequest_tokenKey = Lens.lens (\APNSVoipSandboxChannelRequest' {tokenKey} -> tokenKey) (\s@APNSVoipSandboxChannelRequest' {} a -> s {tokenKey = a} :: APNSVoipSandboxChannelRequest)

-- | The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
aPNSVoipSandboxChannelRequest_bundleId :: Lens.Lens' APNSVoipSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelRequest_bundleId = Lens.lens (\APNSVoipSandboxChannelRequest' {bundleId} -> bundleId) (\s@APNSVoipSandboxChannelRequest' {} a -> s {bundleId = a} :: APNSVoipSandboxChannelRequest)

-- | The default authentication method that you want Amazon Pinpoint to use
-- when authenticating with the APNs sandbox environment for this channel,
-- key or certificate.
aPNSVoipSandboxChannelRequest_defaultAuthenticationMethod :: Lens.Lens' APNSVoipSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelRequest_defaultAuthenticationMethod = Lens.lens (\APNSVoipSandboxChannelRequest' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSVoipSandboxChannelRequest' {} a -> s {defaultAuthenticationMethod = a} :: APNSVoipSandboxChannelRequest)

instance
  Prelude.Hashable
    APNSVoipSandboxChannelRequest
  where
  hashWithSalt _salt APNSVoipSandboxChannelRequest' {..} =
    _salt `Prelude.hashWithSalt` privateKey
      `Prelude.hashWithSalt` teamId
      `Prelude.hashWithSalt` tokenKeyId
      `Prelude.hashWithSalt` certificate
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` tokenKey
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` defaultAuthenticationMethod

instance Prelude.NFData APNSVoipSandboxChannelRequest where
  rnf APNSVoipSandboxChannelRequest' {..} =
    Prelude.rnf privateKey
      `Prelude.seq` Prelude.rnf teamId
      `Prelude.seq` Prelude.rnf tokenKeyId
      `Prelude.seq` Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf tokenKey
      `Prelude.seq` Prelude.rnf bundleId
      `Prelude.seq` Prelude.rnf defaultAuthenticationMethod

instance Core.ToJSON APNSVoipSandboxChannelRequest where
  toJSON APNSVoipSandboxChannelRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PrivateKey" Core..=) Prelude.<$> privateKey,
            ("TeamId" Core..=) Prelude.<$> teamId,
            ("TokenKeyId" Core..=) Prelude.<$> tokenKeyId,
            ("Certificate" Core..=) Prelude.<$> certificate,
            ("Enabled" Core..=) Prelude.<$> enabled,
            ("TokenKey" Core..=) Prelude.<$> tokenKey,
            ("BundleId" Core..=) Prelude.<$> bundleId,
            ("DefaultAuthenticationMethod" Core..=)
              Prelude.<$> defaultAuthenticationMethod
          ]
      )
