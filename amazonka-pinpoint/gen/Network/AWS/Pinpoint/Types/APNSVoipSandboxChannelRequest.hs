{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the status and settings of the APNs (Apple Push Notification
-- service) VoIP sandbox channel for an application.
--
-- /See:/ 'newAPNSVoipSandboxChannelRequest' smart constructor.
data APNSVoipSandboxChannelRequest = APNSVoipSandboxChannelRequest'
  { -- | The default authentication method that you want Amazon Pinpoint to use
    -- when authenticating with the APNs sandbox environment for this channel,
    -- key or certificate.
    defaultAuthenticationMethod :: Prelude.Maybe Prelude.Text,
    -- | The authentication key to use for APNs tokens.
    tokenKey :: Prelude.Maybe Prelude.Text,
    -- | The bundle identifier that\'s assigned to your iOS app. This identifier
    -- is used for APNs tokens.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The identifier that\'s assigned to your Apple developer account team.
    -- This identifier is used for APNs tokens.
    teamId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the APNs VoIP sandbox channel is enabled for the
    -- application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The private key for the APNs client certificate that you want Amazon
    -- Pinpoint to use to communicate with the APNs sandbox environment.
    privateKey :: Prelude.Maybe Prelude.Text,
    -- | The key identifier that\'s assigned to your APNs signing key, if you
    -- want Amazon Pinpoint to communicate with the APNs sandbox environment by
    -- using APNs tokens.
    tokenKeyId :: Prelude.Maybe Prelude.Text,
    -- | The APNs client certificate that you received from Apple, if you want
    -- Amazon Pinpoint to communicate with the APNs sandbox environment by
    -- using an APNs certificate.
    certificate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'APNSVoipSandboxChannelRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultAuthenticationMethod', 'aPNSVoipSandboxChannelRequest_defaultAuthenticationMethod' - The default authentication method that you want Amazon Pinpoint to use
-- when authenticating with the APNs sandbox environment for this channel,
-- key or certificate.
--
-- 'tokenKey', 'aPNSVoipSandboxChannelRequest_tokenKey' - The authentication key to use for APNs tokens.
--
-- 'bundleId', 'aPNSVoipSandboxChannelRequest_bundleId' - The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
--
-- 'teamId', 'aPNSVoipSandboxChannelRequest_teamId' - The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
--
-- 'enabled', 'aPNSVoipSandboxChannelRequest_enabled' - Specifies whether the APNs VoIP sandbox channel is enabled for the
-- application.
--
-- 'privateKey', 'aPNSVoipSandboxChannelRequest_privateKey' - The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with the APNs sandbox environment.
--
-- 'tokenKeyId', 'aPNSVoipSandboxChannelRequest_tokenKeyId' - The key identifier that\'s assigned to your APNs signing key, if you
-- want Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using APNs tokens.
--
-- 'certificate', 'aPNSVoipSandboxChannelRequest_certificate' - The APNs client certificate that you received from Apple, if you want
-- Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using an APNs certificate.
newAPNSVoipSandboxChannelRequest ::
  APNSVoipSandboxChannelRequest
newAPNSVoipSandboxChannelRequest =
  APNSVoipSandboxChannelRequest'
    { defaultAuthenticationMethod =
        Prelude.Nothing,
      tokenKey = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      teamId = Prelude.Nothing,
      enabled = Prelude.Nothing,
      privateKey = Prelude.Nothing,
      tokenKeyId = Prelude.Nothing,
      certificate = Prelude.Nothing
    }

-- | The default authentication method that you want Amazon Pinpoint to use
-- when authenticating with the APNs sandbox environment for this channel,
-- key or certificate.
aPNSVoipSandboxChannelRequest_defaultAuthenticationMethod :: Lens.Lens' APNSVoipSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelRequest_defaultAuthenticationMethod = Lens.lens (\APNSVoipSandboxChannelRequest' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSVoipSandboxChannelRequest' {} a -> s {defaultAuthenticationMethod = a} :: APNSVoipSandboxChannelRequest)

-- | The authentication key to use for APNs tokens.
aPNSVoipSandboxChannelRequest_tokenKey :: Lens.Lens' APNSVoipSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelRequest_tokenKey = Lens.lens (\APNSVoipSandboxChannelRequest' {tokenKey} -> tokenKey) (\s@APNSVoipSandboxChannelRequest' {} a -> s {tokenKey = a} :: APNSVoipSandboxChannelRequest)

-- | The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
aPNSVoipSandboxChannelRequest_bundleId :: Lens.Lens' APNSVoipSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelRequest_bundleId = Lens.lens (\APNSVoipSandboxChannelRequest' {bundleId} -> bundleId) (\s@APNSVoipSandboxChannelRequest' {} a -> s {bundleId = a} :: APNSVoipSandboxChannelRequest)

-- | The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
aPNSVoipSandboxChannelRequest_teamId :: Lens.Lens' APNSVoipSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelRequest_teamId = Lens.lens (\APNSVoipSandboxChannelRequest' {teamId} -> teamId) (\s@APNSVoipSandboxChannelRequest' {} a -> s {teamId = a} :: APNSVoipSandboxChannelRequest)

-- | Specifies whether the APNs VoIP sandbox channel is enabled for the
-- application.
aPNSVoipSandboxChannelRequest_enabled :: Lens.Lens' APNSVoipSandboxChannelRequest (Prelude.Maybe Prelude.Bool)
aPNSVoipSandboxChannelRequest_enabled = Lens.lens (\APNSVoipSandboxChannelRequest' {enabled} -> enabled) (\s@APNSVoipSandboxChannelRequest' {} a -> s {enabled = a} :: APNSVoipSandboxChannelRequest)

-- | The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with the APNs sandbox environment.
aPNSVoipSandboxChannelRequest_privateKey :: Lens.Lens' APNSVoipSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelRequest_privateKey = Lens.lens (\APNSVoipSandboxChannelRequest' {privateKey} -> privateKey) (\s@APNSVoipSandboxChannelRequest' {} a -> s {privateKey = a} :: APNSVoipSandboxChannelRequest)

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

instance
  Prelude.Hashable
    APNSVoipSandboxChannelRequest

instance Prelude.NFData APNSVoipSandboxChannelRequest

instance Prelude.ToJSON APNSVoipSandboxChannelRequest where
  toJSON APNSVoipSandboxChannelRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DefaultAuthenticationMethod" Prelude..=)
              Prelude.<$> defaultAuthenticationMethod,
            ("TokenKey" Prelude..=) Prelude.<$> tokenKey,
            ("BundleId" Prelude..=) Prelude.<$> bundleId,
            ("TeamId" Prelude..=) Prelude.<$> teamId,
            ("Enabled" Prelude..=) Prelude.<$> enabled,
            ("PrivateKey" Prelude..=) Prelude.<$> privateKey,
            ("TokenKeyId" Prelude..=) Prelude.<$> tokenKeyId,
            ("Certificate" Prelude..=) Prelude.<$> certificate
          ]
      )
