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
-- Module      : Network.AWS.Pinpoint.Types.APNSSandboxChannelRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSSandboxChannelRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the status and settings of the APNs (Apple Push Notification
-- service) sandbox channel for an application.
--
-- /See:/ 'newAPNSSandboxChannelRequest' smart constructor.
data APNSSandboxChannelRequest = APNSSandboxChannelRequest'
  { -- | The default authentication method that you want Amazon Pinpoint to use
    -- when authenticating with the APNs sandbox environment, key or
    -- certificate.
    defaultAuthenticationMethod :: Prelude.Maybe Prelude.Text,
    -- | The authentication key to use for APNs tokens.
    tokenKey :: Prelude.Maybe Prelude.Text,
    -- | The bundle identifier that\'s assigned to your iOS app. This identifier
    -- is used for APNs tokens.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The identifier that\'s assigned to your Apple developer account team.
    -- This identifier is used for APNs tokens.
    teamId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to enable the APNs sandbox channel for the
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
-- Create a value of 'APNSSandboxChannelRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultAuthenticationMethod', 'aPNSSandboxChannelRequest_defaultAuthenticationMethod' - The default authentication method that you want Amazon Pinpoint to use
-- when authenticating with the APNs sandbox environment, key or
-- certificate.
--
-- 'tokenKey', 'aPNSSandboxChannelRequest_tokenKey' - The authentication key to use for APNs tokens.
--
-- 'bundleId', 'aPNSSandboxChannelRequest_bundleId' - The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
--
-- 'teamId', 'aPNSSandboxChannelRequest_teamId' - The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
--
-- 'enabled', 'aPNSSandboxChannelRequest_enabled' - Specifies whether to enable the APNs sandbox channel for the
-- application.
--
-- 'privateKey', 'aPNSSandboxChannelRequest_privateKey' - The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with the APNs sandbox environment.
--
-- 'tokenKeyId', 'aPNSSandboxChannelRequest_tokenKeyId' - The key identifier that\'s assigned to your APNs signing key, if you
-- want Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using APNs tokens.
--
-- 'certificate', 'aPNSSandboxChannelRequest_certificate' - The APNs client certificate that you received from Apple, if you want
-- Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using an APNs certificate.
newAPNSSandboxChannelRequest ::
  APNSSandboxChannelRequest
newAPNSSandboxChannelRequest =
  APNSSandboxChannelRequest'
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
-- when authenticating with the APNs sandbox environment, key or
-- certificate.
aPNSSandboxChannelRequest_defaultAuthenticationMethod :: Lens.Lens' APNSSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelRequest_defaultAuthenticationMethod = Lens.lens (\APNSSandboxChannelRequest' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSSandboxChannelRequest' {} a -> s {defaultAuthenticationMethod = a} :: APNSSandboxChannelRequest)

-- | The authentication key to use for APNs tokens.
aPNSSandboxChannelRequest_tokenKey :: Lens.Lens' APNSSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelRequest_tokenKey = Lens.lens (\APNSSandboxChannelRequest' {tokenKey} -> tokenKey) (\s@APNSSandboxChannelRequest' {} a -> s {tokenKey = a} :: APNSSandboxChannelRequest)

-- | The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
aPNSSandboxChannelRequest_bundleId :: Lens.Lens' APNSSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelRequest_bundleId = Lens.lens (\APNSSandboxChannelRequest' {bundleId} -> bundleId) (\s@APNSSandboxChannelRequest' {} a -> s {bundleId = a} :: APNSSandboxChannelRequest)

-- | The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
aPNSSandboxChannelRequest_teamId :: Lens.Lens' APNSSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelRequest_teamId = Lens.lens (\APNSSandboxChannelRequest' {teamId} -> teamId) (\s@APNSSandboxChannelRequest' {} a -> s {teamId = a} :: APNSSandboxChannelRequest)

-- | Specifies whether to enable the APNs sandbox channel for the
-- application.
aPNSSandboxChannelRequest_enabled :: Lens.Lens' APNSSandboxChannelRequest (Prelude.Maybe Prelude.Bool)
aPNSSandboxChannelRequest_enabled = Lens.lens (\APNSSandboxChannelRequest' {enabled} -> enabled) (\s@APNSSandboxChannelRequest' {} a -> s {enabled = a} :: APNSSandboxChannelRequest)

-- | The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with the APNs sandbox environment.
aPNSSandboxChannelRequest_privateKey :: Lens.Lens' APNSSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelRequest_privateKey = Lens.lens (\APNSSandboxChannelRequest' {privateKey} -> privateKey) (\s@APNSSandboxChannelRequest' {} a -> s {privateKey = a} :: APNSSandboxChannelRequest)

-- | The key identifier that\'s assigned to your APNs signing key, if you
-- want Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using APNs tokens.
aPNSSandboxChannelRequest_tokenKeyId :: Lens.Lens' APNSSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelRequest_tokenKeyId = Lens.lens (\APNSSandboxChannelRequest' {tokenKeyId} -> tokenKeyId) (\s@APNSSandboxChannelRequest' {} a -> s {tokenKeyId = a} :: APNSSandboxChannelRequest)

-- | The APNs client certificate that you received from Apple, if you want
-- Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using an APNs certificate.
aPNSSandboxChannelRequest_certificate :: Lens.Lens' APNSSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelRequest_certificate = Lens.lens (\APNSSandboxChannelRequest' {certificate} -> certificate) (\s@APNSSandboxChannelRequest' {} a -> s {certificate = a} :: APNSSandboxChannelRequest)

instance Prelude.Hashable APNSSandboxChannelRequest

instance Prelude.NFData APNSSandboxChannelRequest

instance Prelude.ToJSON APNSSandboxChannelRequest where
  toJSON APNSSandboxChannelRequest' {..} =
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
