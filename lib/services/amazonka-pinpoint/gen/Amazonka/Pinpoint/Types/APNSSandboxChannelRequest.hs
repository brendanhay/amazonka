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
-- Module      : Amazonka.Pinpoint.Types.APNSSandboxChannelRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.APNSSandboxChannelRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the status and settings of the APNs (Apple Push Notification
-- service) sandbox channel for an application.
--
-- /See:/ 'newAPNSSandboxChannelRequest' smart constructor.
data APNSSandboxChannelRequest = APNSSandboxChannelRequest'
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
    -- | Specifies whether to enable the APNs sandbox channel for the
    -- application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The authentication key to use for APNs tokens.
    tokenKey :: Prelude.Maybe Prelude.Text,
    -- | The bundle identifier that\'s assigned to your iOS app. This identifier
    -- is used for APNs tokens.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The default authentication method that you want Amazon Pinpoint to use
    -- when authenticating with the APNs sandbox environment, key or
    -- certificate.
    defaultAuthenticationMethod :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'APNSSandboxChannelRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateKey', 'aPNSSandboxChannelRequest_privateKey' - The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with the APNs sandbox environment.
--
-- 'teamId', 'aPNSSandboxChannelRequest_teamId' - The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
--
-- 'tokenKeyId', 'aPNSSandboxChannelRequest_tokenKeyId' - The key identifier that\'s assigned to your APNs signing key, if you
-- want Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using APNs tokens.
--
-- 'certificate', 'aPNSSandboxChannelRequest_certificate' - The APNs client certificate that you received from Apple, if you want
-- Amazon Pinpoint to communicate with the APNs sandbox environment by
-- using an APNs certificate.
--
-- 'enabled', 'aPNSSandboxChannelRequest_enabled' - Specifies whether to enable the APNs sandbox channel for the
-- application.
--
-- 'tokenKey', 'aPNSSandboxChannelRequest_tokenKey' - The authentication key to use for APNs tokens.
--
-- 'bundleId', 'aPNSSandboxChannelRequest_bundleId' - The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
--
-- 'defaultAuthenticationMethod', 'aPNSSandboxChannelRequest_defaultAuthenticationMethod' - The default authentication method that you want Amazon Pinpoint to use
-- when authenticating with the APNs sandbox environment, key or
-- certificate.
newAPNSSandboxChannelRequest ::
  APNSSandboxChannelRequest
newAPNSSandboxChannelRequest =
  APNSSandboxChannelRequest'
    { privateKey =
        Prelude.Nothing,
      teamId = Prelude.Nothing,
      tokenKeyId = Prelude.Nothing,
      certificate = Prelude.Nothing,
      enabled = Prelude.Nothing,
      tokenKey = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      defaultAuthenticationMethod = Prelude.Nothing
    }

-- | The private key for the APNs client certificate that you want Amazon
-- Pinpoint to use to communicate with the APNs sandbox environment.
aPNSSandboxChannelRequest_privateKey :: Lens.Lens' APNSSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelRequest_privateKey = Lens.lens (\APNSSandboxChannelRequest' {privateKey} -> privateKey) (\s@APNSSandboxChannelRequest' {} a -> s {privateKey = a} :: APNSSandboxChannelRequest)

-- | The identifier that\'s assigned to your Apple developer account team.
-- This identifier is used for APNs tokens.
aPNSSandboxChannelRequest_teamId :: Lens.Lens' APNSSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelRequest_teamId = Lens.lens (\APNSSandboxChannelRequest' {teamId} -> teamId) (\s@APNSSandboxChannelRequest' {} a -> s {teamId = a} :: APNSSandboxChannelRequest)

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

-- | Specifies whether to enable the APNs sandbox channel for the
-- application.
aPNSSandboxChannelRequest_enabled :: Lens.Lens' APNSSandboxChannelRequest (Prelude.Maybe Prelude.Bool)
aPNSSandboxChannelRequest_enabled = Lens.lens (\APNSSandboxChannelRequest' {enabled} -> enabled) (\s@APNSSandboxChannelRequest' {} a -> s {enabled = a} :: APNSSandboxChannelRequest)

-- | The authentication key to use for APNs tokens.
aPNSSandboxChannelRequest_tokenKey :: Lens.Lens' APNSSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelRequest_tokenKey = Lens.lens (\APNSSandboxChannelRequest' {tokenKey} -> tokenKey) (\s@APNSSandboxChannelRequest' {} a -> s {tokenKey = a} :: APNSSandboxChannelRequest)

-- | The bundle identifier that\'s assigned to your iOS app. This identifier
-- is used for APNs tokens.
aPNSSandboxChannelRequest_bundleId :: Lens.Lens' APNSSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelRequest_bundleId = Lens.lens (\APNSSandboxChannelRequest' {bundleId} -> bundleId) (\s@APNSSandboxChannelRequest' {} a -> s {bundleId = a} :: APNSSandboxChannelRequest)

-- | The default authentication method that you want Amazon Pinpoint to use
-- when authenticating with the APNs sandbox environment, key or
-- certificate.
aPNSSandboxChannelRequest_defaultAuthenticationMethod :: Lens.Lens' APNSSandboxChannelRequest (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelRequest_defaultAuthenticationMethod = Lens.lens (\APNSSandboxChannelRequest' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSSandboxChannelRequest' {} a -> s {defaultAuthenticationMethod = a} :: APNSSandboxChannelRequest)

instance Prelude.Hashable APNSSandboxChannelRequest where
  hashWithSalt _salt APNSSandboxChannelRequest' {..} =
    _salt `Prelude.hashWithSalt` privateKey
      `Prelude.hashWithSalt` teamId
      `Prelude.hashWithSalt` tokenKeyId
      `Prelude.hashWithSalt` certificate
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` tokenKey
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` defaultAuthenticationMethod

instance Prelude.NFData APNSSandboxChannelRequest where
  rnf APNSSandboxChannelRequest' {..} =
    Prelude.rnf privateKey
      `Prelude.seq` Prelude.rnf teamId
      `Prelude.seq` Prelude.rnf tokenKeyId
      `Prelude.seq` Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf tokenKey
      `Prelude.seq` Prelude.rnf bundleId
      `Prelude.seq` Prelude.rnf defaultAuthenticationMethod

instance Core.ToJSON APNSSandboxChannelRequest where
  toJSON APNSSandboxChannelRequest' {..} =
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
